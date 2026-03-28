library(shiny)
library(shinydashboard)
library(caret)
library(randomForest)
library(rpart)
library(ggplot2)

# ------------------
# Load saved models and reference objects
# ------------------
lm_model <- readRDS("lm_model.rds")
tree_model <- readRDS("tree_model.rds")
rf_model <- readRDS("rf_model.rds")
reference_data <- readRDS("reference_data.rds")
lm_top_areas <- readRDS("lm_top_areas.rds")

# Make sure categorical columns are factors
reference_data$Township <- as.factor(reference_data$Township)
reference_data$Area <- as.factor(reference_data$Area)
reference_data$State <- as.factor(reference_data$State)
reference_data$Primary_Type <- as.factor(reference_data$Primary_Type)

# Helper for currency formatting
format_rm <- function(x) {
  paste0("RM ", format(round(x, 2), big.mark = ",", scientific = FALSE))
}

# ------------------
# UI
# ------------------
ui <- dashboardPage(
  dashboardHeader(title = "House Price Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction Dashboard", tabName = "prediction", icon = icon("chart-line")),
      menuItem("Model Overview", tabName = "overview", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Input Property Features",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              inputId = "township",
              label = "Select Township",
              choices = sort(unique(as.character(reference_data$Township)))
            ),
            
            selectInput(
              inputId = "area",
              label = "Select Area",
              choices = sort(unique(as.character(reference_data$Area)))
            ),
            
            selectInput(
              inputId = "state",
              label = "Select State",
              choices = sort(unique(as.character(reference_data$State)))
            ),
            
            selectInput(
              inputId = "primary_type",
              label = "Select Property Type",
              choices = sort(unique(as.character(reference_data$Primary_Type)))
            ),
            
            selectInput(
              inputId = "multi_type",
              label = "Multiple Property Types in Area?",
              choices = c("No" = 0, "Yes" = 1),
              selected = 0
            ),
            
            selectInput(
              inputId = "primary_tenure",
              label = "Primary Tenure",
              choices = c("Leasehold" = 0, "Freehold" = 1),
              selected = 1
            ),
            
            numericInput(
              inputId = "median_psf",
              label = "Median Price per Square Foot (MYR)",
              value = round(median(exp(reference_data$Log_Median_PSF), na.rm = TRUE), 2),
              min = 1,
              step = 1
            ),
            
            numericInput(
              inputId = "transactions",
              label = "Number of Transactions",
              value = round(median(exp(reference_data$Log_Transactions), na.rm = TRUE)),
              min = 1,
              step = 1
            ),
            
            actionButton(
              "predict_btn",
              "Predict Price",
              icon = icon("play"),
              class = "btn-success"
            )
          ),
          
          box(
            title = "Predicted House Prices",
            width = 8,
            status = "success",
            solidHeader = TRUE,
            
            fluidRow(
              valueBoxOutput("lm_box", width = 4),
              valueBoxOutput("tree_box", width = 4),
              valueBoxOutput("rf_box", width = 4)
            ),
            
            fluidRow(
              valueBoxOutput("best_box", width = 6),
              valueBoxOutput("avg_box", width = 6)
            ),
            
            br(),
            textOutput("mapping_note"),
            br(),
            h4("Prediction Comparison"),
            tableOutput("prediction_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Prediction Comparison Chart",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            plotOutput("prediction_plot", height = 320)
          )
        )
      ),
      
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("records_box", width = 3),
          valueBoxOutput("areas_box", width = 3),
          valueBoxOutput("townships_box", width = 3),
          valueBoxOutput("states_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Model Notes",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            htmlOutput("model_notes")
          ),
          
          box(
            title = "Input Mapping Notes",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            htmlOutput("mapping_notes")
          )
        )
      )
    )
  )
)

# ------------------
# Server
# ------------------
server <- function(input, output, session) {
  
  output$records_box <- renderValueBox({
    valueBox(nrow(reference_data), "Records", icon = icon("database"), color = "purple")
  })
  
  output$areas_box <- renderValueBox({
    valueBox(length(unique(reference_data$Area)), "Areas", icon = icon("map-marker-alt"), color = "blue")
  })
  
  output$townships_box <- renderValueBox({
    valueBox(length(unique(reference_data$Township)), "Townships", icon = icon("city"), color = "teal")
  })
  
  output$states_box <- renderValueBox({
    valueBox(length(unique(reference_data$State)), "States", icon = icon("globe-asia"), color = "olive")
  })
  
  output$model_notes <- renderUI({
    HTML(paste0(
      "<ul>",
      "<li><b>Multiple Linear Regression</b>: uses grouped Area values (top training areas + Other).</li>",
      "<li><b>Decision Tree</b>: uses original Township, Area, and State.</li>",
      "<li><b>Random Forest</b>: uses original Township, Area, and State and is the strongest overall model.</li>",
      "</ul>"
    ))
  })
  
  output$mapping_notes <- renderUI({
    HTML(paste0(
      "<ul>",
      "<li>For Linear Regression, areas outside the saved top areas are automatically mapped to <b>Other</b>.</li>",
      "<li>For Decision Tree and Random Forest, the selected actual Area is used directly.</li>",
      "<li>Predictions are trained on <b>log-transformed prices</b> and converted back to MYR.</li>",
      "</ul>"
    ))
  })
  
  prediction_results <- eventReactive(input$predict_btn, {
    validate(
      need(input$median_psf > 0, "Median PSF must be greater than 0."),
      need(input$transactions > 0, "Transactions must be greater than 0.")
    )
    
    # LM area mapping
    lm_area_value <- if (input$area %in% lm_top_areas) input$area else "Other"
    
    # New data for LM
    lm_new_data <- data.frame(
      Area = factor(lm_area_value, levels = c(lm_top_areas, "Other")),
      Primary_Type = factor(input$primary_type, levels = levels(reference_data$Primary_Type)),
      Multi_Type = as.numeric(input$multi_type),
      Primary_Tenure = as.numeric(input$primary_tenure),
      Log_Median_PSF = log(input$median_psf),
      Log_Transactions = log(input$transactions)
    )
    
    # New data for Tree
    tree_new_data <- data.frame(
      Township = factor(input$township, levels = levels(reference_data$Township)),
      Area = factor(input$area, levels = levels(reference_data$Area)),
      State = factor(input$state, levels = levels(reference_data$State)),
      Primary_Type = factor(input$primary_type, levels = levels(reference_data$Primary_Type)),
      Multi_Type = as.numeric(input$multi_type),
      Primary_Tenure = as.numeric(input$primary_tenure),
      Log_Median_PSF = log(input$median_psf),
      Log_Transactions = log(input$transactions)
    )
    
    # New data for RF
    rf_new_data <- data.frame(
      Township = factor(input$township, levels = levels(reference_data$Township)),
      Area = factor(input$area, levels = levels(reference_data$Area)),
      State = factor(input$state, levels = levels(reference_data$State)),
      Primary_Type = factor(input$primary_type, levels = levels(reference_data$Primary_Type)),
      Multi_Type = as.numeric(input$multi_type),
      Primary_Tenure = as.numeric(input$primary_tenure),
      Log_Median_PSF = log(input$median_psf),
      Log_Transactions = log(input$transactions)
    )
    
    # Debug check if needed
    # print(lm_new_data)
    # print(tree_new_data)
    # print(rf_new_data)
    
    # Predictions
    lm_pred_log <- predict(lm_model, newdata = lm_new_data)
    tree_pred_log <- predict(tree_model, newdata = tree_new_data)
    rf_pred_log <- predict(rf_model, newdata = rf_new_data)
    
    # Convert back to actual price
    lm_price <- as.numeric(exp(lm_pred_log))
    tree_price <- as.numeric(exp(tree_pred_log))
    rf_price <- as.numeric(exp(rf_pred_log))
    
    pred_df <- data.frame(
      Model = c("Multiple Linear Regression", "Decision Tree", "Random Forest"),
      Predicted_Price = c(lm_price, tree_price, rf_price)
    )
    
    avg_price <- mean(pred_df$Predicted_Price)
    
    list(
      lm_price = lm_price,
      tree_price = tree_price,
      rf_price = rf_price,
      avg_price = avg_price,
      pred_df = pred_df,
      lm_area_value = lm_area_value
    )
  })
  
  output$lm_box <- renderValueBox({
    req(prediction_results())
    valueBox(format_rm(prediction_results()$lm_price), "Linear Regression", icon = icon("chart-line"), color = "blue")
  })
  
  output$tree_box <- renderValueBox({
    req(prediction_results())
    valueBox(format_rm(prediction_results()$tree_price), "Decision Tree", icon = icon("sitemap"), color = "yellow")
  })
  
  output$rf_box <- renderValueBox({
    req(prediction_results())
    valueBox(format_rm(prediction_results()$rf_price), "Random Forest", icon = icon("tree"), color = "green")
  })
  
  output$best_box <- renderValueBox({
    req(prediction_results())
    valueBox(
      format_rm(prediction_results()$rf_price),
      "Recommended Prediction (Best Model: Random Forest)",
      icon = icon("award"),
      color = "red"
    )
  })
  
  output$avg_box <- renderValueBox({
    req(prediction_results())
    valueBox(format_rm(prediction_results()$avg_price), "Average of 3 Models", icon = icon("calculator"), color = "purple")
  })
  
  output$mapping_note <- renderText({
    req(prediction_results())
    paste("Linear Regression mapped selected area to:", prediction_results()$lm_area_value)
  })
  
  output$prediction_table <- renderTable({
    req(prediction_results())
    out <- prediction_results()$pred_df
    out$Predicted_Price <- format_rm(out$Predicted_Price)
    out
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  output$prediction_plot <- renderPlot({
    req(prediction_results())
    plot_df <- prediction_results()$pred_df
    
    ggplot(plot_df, aes(x = Model, y = Predicted_Price)) +
      geom_col() +
      labs(
        title = "Predicted Price by Model",
        x = "Model",
        y = "Predicted Price (MYR)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
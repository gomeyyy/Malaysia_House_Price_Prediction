library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)

# ------------------
# Load saved models, reference objects, and optional summary files
# ------------------
lm_model_path <- "lm_model.rds"
tree_model_path <- "tree_model.rds"
rf_model_path <- "rf_model.rds"
reference_data <- readRDS("reference_data.rds")
lm_top_areas <- readRDS("lm_top_areas.rds")

model_metrics <- tryCatch({
  read.csv("model_comparison.csv", stringsAsFactors = FALSE)
}, error = function(e) NULL)

lm_results_plot <- tryCatch({
  read.csv("linear_model_predicted_price.csv", stringsAsFactors = FALSE)
}, error = function(e) NULL)

tree_results_plot <- tryCatch({
  read.csv("tree_model_predicted_price.csv", stringsAsFactors = FALSE)
}, error = function(e) NULL)

rf_results_plot <- tryCatch({
  read.csv("rf_model_predicted_price.csv", stringsAsFactors = FALSE)
}, error = function(e) NULL)

reference_data <- reference_data %>%
  mutate(
    Township = as.factor(Township),
    Area = as.factor(Area),
    State = as.factor(State),
    Primary_Type = as.factor(Primary_Type)
  )

states <- sort(unique(as.character(reference_data$State)))
areas <- sort(unique(as.character(reference_data$Area)))
townships <- sort(unique(as.character(reference_data$Township)))
property_types <- sort(unique(as.character(reference_data$Primary_Type)))

median_price_range <- range(reference_data$Median_Price, na.rm = TRUE)
median_psf_range <- range(exp(reference_data$Log_Median_PSF), na.rm = TRUE)
transactions_range <- range(exp(reference_data$Log_Transactions), na.rm = TRUE)

prediction_default_psf <- round(median(exp(reference_data$Log_Median_PSF), na.rm = TRUE))
prediction_default_transactions <- round(median(exp(reference_data$Log_Transactions), na.rm = TRUE))

format_rm <- function(x) {
  paste0("RM ", format(round(x, 2), big.mark = ",", scientific = FALSE))
}

format_count <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE)
}

first_or_null <- function(x) {
  if (length(x) == 0) NULL else x[[1]]
}

predict_from_saved_model <- function(model_path, newdata) {
  model <- readRDS(model_path)
  on.exit({
    rm(model)
    gc(verbose = FALSE)
  }, add = TRUE)

  as.numeric(exp(predict(model, newdata = newdata)))
}

best_metric_row <- if (!is.null(model_metrics) && "R2" %in% names(model_metrics)) {
  model_metrics[which.max(model_metrics$R2), , drop = FALSE]
} else {
  NULL
}

best_model_name <- if (!is.null(best_metric_row)) best_metric_row$Model[[1]] else "Random Forest"
best_model_r2 <- if (!is.null(best_metric_row)) best_metric_row$R2[[1]] else NA_real_

ui <- dashboardPage(
  dashboardHeader(title = "House Price Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction Dashboard", tabName = "prediction", icon = icon("chart-line")),
      menuItem("Interactive Insights", tabName = "insights", icon = icon("sliders-h")),
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
              inputId = "state",
              label = "Select State",
              choices = states,
              selected = states[[1]]
            ),
            selectInput(
              inputId = "area",
              label = "Select Area",
              choices = areas
            ),
            selectInput(
              inputId = "township",
              label = "Select Township",
              choices = townships
            ),
            selectInput(
              inputId = "primary_type",
              label = "Select Property Type",
              choices = property_types
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
            sliderInput(
              inputId = "median_psf",
              label = "Median Price per Square Foot (MYR)",
              min = floor(median_psf_range[[1]]),
              max = ceiling(median_psf_range[[2]]),
              value = prediction_default_psf,
              step = 1
            ),
            sliderInput(
              inputId = "transactions",
              label = "Number of Transactions",
              min = floor(transactions_range[[1]]),
              max = ceiling(transactions_range[[2]]),
              value = prediction_default_transactions,
              step = 1
            ),
            actionButton("predict_btn", "Predict Price", icon = icon("play"), class = "btn-success"),
            br(),
            br(),
            textOutput("prediction_context")
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
            plotlyOutput("prediction_plot", height = 350)
          )
        )
      ),
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Interactive Filters",
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            selectizeInput(
              inputId = "filter_states",
              label = "States",
              choices = states,
              multiple = TRUE,
              selected = states
            ),
            selectizeInput(
              inputId = "filter_property_types",
              label = "Property Types",
              choices = property_types,
              multiple = TRUE,
              selected = property_types
            ),
            sliderInput(
              inputId = "filter_price_range",
              label = "Median Price Range (MYR)",
              min = floor(median_price_range[[1]]),
              max = ceiling(median_price_range[[2]]),
              value = c(floor(median_price_range[[1]]), ceiling(median_price_range[[2]])),
              step = 10000,
              pre = "RM "
            ),
            sliderInput(
              inputId = "filter_transactions_range",
              label = "Transactions Range",
              min = floor(transactions_range[[1]]),
              max = ceiling(transactions_range[[2]]),
              value = c(floor(transactions_range[[1]]), ceiling(transactions_range[[2]])),
              step = 1
            ),
            checkboxInput("show_top_states_only", "Show top 10 states by median price", value = TRUE)
          ),
          box(
            title = "Filtered Market Snapshot",
            width = 9,
            status = "success",
            solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("filtered_records_box", width = 4),
              valueBoxOutput("filtered_price_box", width = 4),
              valueBoxOutput("filtered_psf_box", width = 4)
            ),
            fluidRow(
              valueBoxOutput("filtered_transactions_box", width = 4),
              valueBoxOutput("filtered_states_box", width = 4),
              valueBoxOutput("filtered_types_box", width = 4)
            )
          )
        ),
        fluidRow(
          box(
            title = "Median Price Distribution",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("median_distribution_plot", height = 320)
          ),
          box(
            title = "Median Price by State",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("median_price_by_state_plot", height = 320)
          )
        ),
        fluidRow(
          box(
            title = "Median PSF vs Median Price",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("psf_vs_price_plot", height = 340)
          ),
          box(
            title = "Median Price by Property Type",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("price_by_type_plot", height = 340)
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
            title = "Recommendation Logic",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            htmlOutput("mapping_notes")
          )
        ),
        fluidRow(
          box(
            title = "Model Performance Summary",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            tableOutput("metrics_table")
          )
        ),
        fluidRow(
          box(
            title = "Actual vs Predicted: Linear Regression",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("lm_scatter", height = 280)
          ),
          box(
            title = "Actual vs Predicted: Decision Tree",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("tree_scatter", height = 280)
          ),
          box(
            title = "Actual vs Predicted: Random Forest",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("rf_scatter", height = 280)
          )
        ),
        fluidRow(
          box(
            title = "R2 Comparison",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("metrics_plot", height = 300)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    req(input$state)

    state_data <- reference_data %>%
      filter(State == input$state)

    available_areas <- sort(unique(as.character(state_data$Area)))
    selected_area <- if (!is.null(input$area) && input$area %in% available_areas) {
      input$area
    } else {
      first_or_null(available_areas)
    }

    updateSelectInput(session, "area", choices = available_areas, selected = selected_area)

    township_data <- if (!is.null(selected_area) && nzchar(selected_area)) {
      state_data %>% filter(Area == selected_area)
    } else {
      state_data
    }

    available_townships <- sort(unique(as.character(township_data$Township)))
    selected_township <- if (!is.null(input$township) && input$township %in% available_townships) {
      input$township
    } else {
      first_or_null(available_townships)
    }

    updateSelectInput(session, "township", choices = available_townships, selected = selected_township)
  })

  filtered_dashboard_data <- reactive({
    req(input$filter_states, input$filter_property_types)

    reference_data %>%
      mutate(
        Median_PSF = exp(Log_Median_PSF),
        Transactions = exp(Log_Transactions)
      ) %>%
      filter(
        State %in% input$filter_states,
        Primary_Type %in% input$filter_property_types,
        Median_Price >= input$filter_price_range[[1]],
        Median_Price <= input$filter_price_range[[2]],
        Transactions >= input$filter_transactions_range[[1]],
        Transactions <= input$filter_transactions_range[[2]]
      )
  })

  prediction_results <- eventReactive(input$predict_btn, {
    validate(
      need(!is.null(input$state) && nzchar(input$state), "Please select a state."),
      need(!is.null(input$area) && nzchar(input$area), "Please select an area."),
      need(!is.null(input$township) && nzchar(input$township), "Please select a township."),
      need(!is.null(input$primary_type) && nzchar(input$primary_type), "Please select a property type."),
      need(input$median_psf > 0, "Median PSF must be greater than 0."),
      need(input$transactions > 0, "Transactions must be greater than 0.")
    )

    tryCatch({
      requireNamespace("caret", quietly = TRUE)
      requireNamespace("randomForest", quietly = TRUE)
      requireNamespace("rpart", quietly = TRUE)

      lm_area_value <- if (input$area %in% lm_top_areas) input$area else "Other"

      lm_new_data <- data.frame(
        Area = factor(lm_area_value, levels = c(lm_top_areas, "Other")),
        Primary_Type = factor(input$primary_type, levels = levels(reference_data$Primary_Type)),
        Multi_Type = as.numeric(input$multi_type),
        Primary_Tenure = as.numeric(input$primary_tenure),
        Log_Median_PSF = log(input$median_psf),
        Log_Transactions = log(input$transactions)
      )

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

      validate(
        need(!anyNA(lm_new_data), "The selected inputs are not compatible with the saved linear model."),
        need(!anyNA(tree_new_data), "The selected location or property type is not available in the saved training data.")
      )

      rf_new_data <- tree_new_data

      lm_price <- predict_from_saved_model(lm_model_path, lm_new_data)
      tree_price <- predict_from_saved_model(tree_model_path, tree_new_data)
      rf_price <- predict_from_saved_model(rf_model_path, rf_new_data)

      pred_df <- data.frame(
        Model = c("Multiple Linear Regression", "Decision Tree", "Random Forest"),
        Predicted_Price = c(lm_price, tree_price, rf_price),
        stringsAsFactors = FALSE
      )

      recommended_row <- if (!is.null(model_metrics) && "R2" %in% names(model_metrics)) {
        merged_df <- pred_df %>%
          left_join(model_metrics[, c("Model", "R2")], by = "Model")

        merged_df[which.max(merged_df$R2), , drop = FALSE]
      } else {
        pred_df[pred_df$Model == "Random Forest", , drop = FALSE]
      }

      list(
        lm_price = lm_price,
        tree_price = tree_price,
        rf_price = rf_price,
        avg_price = mean(pred_df$Predicted_Price),
        pred_df = pred_df,
        lm_area_value = lm_area_value,
        recommended_model = recommended_row$Model[[1]],
        recommended_price = recommended_row$Predicted_Price[[1]],
        recommended_r2 = if ("R2" %in% names(recommended_row)) recommended_row$R2[[1]] else NA_real_
      )
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) {
        stop(e)
      }

      validate(
        need(FALSE, paste("Prediction failed:", conditionMessage(e)))
      )
    })
  })

  output$prediction_context <- renderText({
    paste(
      "Prediction inputs are tailored to",
      tools::toTitleCase(input$state),
      "with area and township choices updated automatically."
    )
  })

  output$records_box <- renderValueBox({
    valueBox(format_count(nrow(reference_data)), "Records", icon = icon("database"), color = "purple")
  })

  output$areas_box <- renderValueBox({
    valueBox(format_count(length(unique(reference_data$Area))), "Areas", icon = icon("map-marker-alt"), color = "blue")
  })

  output$townships_box <- renderValueBox({
    valueBox(format_count(length(unique(reference_data$Township))), "Townships", icon = icon("city"), color = "teal")
  })

  output$states_box <- renderValueBox({
    valueBox(format_count(length(unique(reference_data$State))), "States", icon = icon("globe-asia"), color = "olive")
  })

  output$filtered_records_box <- renderValueBox({
    snapshot <- filtered_dashboard_data()
    valueBox(format_count(nrow(snapshot)), "Filtered Records", icon = icon("filter"), color = "light-blue")
  })

  output$filtered_price_box <- renderValueBox({
    snapshot <- filtered_dashboard_data()
    valueBox(format_rm(median(snapshot$Median_Price, na.rm = TRUE)), "Median Price", icon = icon("wallet"), color = "green")
  })

  output$filtered_psf_box <- renderValueBox({
    snapshot <- filtered_dashboard_data()
    valueBox(format_rm(median(snapshot$Median_PSF, na.rm = TRUE)), "Median PSF", icon = icon("ruler-combined"), color = "yellow")
  })

  output$filtered_transactions_box <- renderValueBox({
    snapshot <- filtered_dashboard_data()
    valueBox(format_count(median(snapshot$Transactions, na.rm = TRUE)), "Median Transactions", icon = icon("exchange-alt"), color = "aqua")
  })

  output$filtered_states_box <- renderValueBox({
    snapshot <- filtered_dashboard_data()
    valueBox(format_count(length(unique(snapshot$State))), "States in View", icon = icon("globe-asia"), color = "navy")
  })

  output$filtered_types_box <- renderValueBox({
    snapshot <- filtered_dashboard_data()
    valueBox(format_count(length(unique(snapshot$Primary_Type))), "Property Types", icon = icon("home"), color = "maroon")
  })

  output$model_notes <- renderUI({
    HTML(paste0(
      "<ul>",
      "<li><b>Multiple Linear Regression</b> uses grouped Area values (top training areas + Other).</li>",
      "<li><b>Decision Tree</b> uses the actual Township, Area, and State selected by the user.</li>",
      "<li><b>Random Forest</b> uses the full property context and remains one of the strongest predictive models.</li>",
      "</ul>"
    ))
  })

  output$mapping_notes <- renderUI({
    note_text <- if (!is.na(best_model_r2)) {
      paste0(
        "<p>The app now recommends the prediction from the model with the strongest saved <b>R2</b> value. ",
        "Current leader: <b>", best_model_name, "</b> with R2 = <b>", round(best_model_r2, 3), "</b>.</p>"
      )
    } else {
      "<p>The app recommends the strongest model result based on the saved model comparison file.</p>"
    }

    HTML(paste0(
      note_text,
      "<ul>",
      "<li>The interactive dashboard filters the market data by state, property type, price range, and transaction volume.</li>",
      "<li>The prediction inputs narrow area and township choices to match the selected state.</li>",
      "<li>Predictions are trained on <b>log-transformed prices</b> and converted back to MYR for display.</li>",
      "</ul>"
    ))
  })

  output$metrics_table <- renderTable({
    if (is.null(model_metrics)) {
      return(data.frame(Note = "Save model_comparison.csv in the app folder to display RMSE, MAE, R2, MAPE, and Approx Accuracy."))
    }
    model_metrics
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$median_distribution_plot <- renderPlotly({
    snapshot <- filtered_dashboard_data()
    req(nrow(snapshot) > 0)

    ggplotly(
      ggplot(snapshot, aes(x = Median_Price)) +
        geom_histogram(bins = 30, fill = "#0f766e", color = "white", alpha = 0.9) +
        scale_x_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        labs(x = "Median Price", y = "Count") +
        theme_minimal()
    )
  })

  output$median_price_by_state_plot <- renderPlotly({
    snapshot <- filtered_dashboard_data()
    req(nrow(snapshot) > 0)

    state_summary <- snapshot %>%
      group_by(State) %>%
      summarise(
        Median_Price = median(Median_Price, na.rm = TRUE),
        Records = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Median_Price))

    if (isTRUE(input$show_top_states_only)) {
      state_summary <- head(state_summary, 10)
    }

    plot_ly(
      state_summary,
      x = ~reorder(State, Median_Price),
      y = ~Median_Price,
      type = "bar",
      text = ~paste0("Records: ", Records, "<br>Median Price: ", format_rm(Median_Price)),
      hoverinfo = "text",
      marker = list(color = "#2563eb")
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = "Median Price", tickprefix = "RM ", separatethousands = TRUE)
      )
  })

  output$psf_vs_price_plot <- renderPlotly({
    snapshot <- filtered_dashboard_data()
    req(nrow(snapshot) > 0)

    plot_ly(
      snapshot,
      x = ~Median_PSF,
      y = ~Median_Price,
      color = ~Primary_Type,
      colors = "Set2",
      type = "scatter",
      mode = "markers",
      marker = list(size = 9, opacity = 0.65),
      text = ~paste(
        "State:", State,
        "<br>Area:", Area,
        "<br>Township:", Township,
        "<br>Median PSF:", format_rm(Median_PSF),
        "<br>Median Price:", format_rm(Median_Price),
        "<br>Transactions:", format_count(Transactions)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Median Price Per Square Foot", tickprefix = "RM ", separatethousands = TRUE),
        yaxis = list(title = "Median Price", tickprefix = "RM ", separatethousands = TRUE),
        legend = list(orientation = "h")
      )
  })

  output$price_by_type_plot <- renderPlotly({
    snapshot <- filtered_dashboard_data()
    req(nrow(snapshot) > 0)

    ggplotly(
      ggplot(snapshot, aes(x = reorder(Primary_Type, Median_Price, FUN = median), y = Median_Price, fill = Primary_Type)) +
        geom_boxplot(alpha = 0.85, show.legend = FALSE) +
        scale_y_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        labs(x = "Property Type", y = "Median Price") +
        theme_minimal()
    )
  })

  output$lm_scatter <- renderPlotly({
    req(lm_results_plot)
    ggplotly(
      ggplot(lm_results_plot, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.5, color = "#2563eb") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        scale_x_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        scale_y_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        labs(x = "Actual Price", y = "Predicted Price") +
        theme_minimal()
    )
  })

  output$tree_scatter <- renderPlotly({
    req(tree_results_plot)
    ggplotly(
      ggplot(tree_results_plot, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.5, color = "#ca8a04") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        scale_x_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        scale_y_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        labs(x = "Actual Price", y = "Predicted Price") +
        theme_minimal()
    )
  })

  output$rf_scatter <- renderPlotly({
    req(rf_results_plot)
    ggplotly(
      ggplot(rf_results_plot, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.5, color = "#15803d") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        scale_x_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        scale_y_continuous(labels = label_number(prefix = "RM ", big.mark = ",")) +
        labs(x = "Actual Price", y = "Predicted Price") +
        theme_minimal()
    )
  })

  output$metrics_plot <- renderPlotly({
    req(model_metrics)

    metrics_plot_df <- model_metrics %>%
      mutate(Hover_Text = paste0("R2: ", round(R2, 3), "<br>RMSE: ", format_rm(RMSE)))

    plot_ly(
      metrics_plot_df,
      x = ~Model,
      y = ~R2,
      type = "bar",
      text = ~Hover_Text,
      hoverinfo = "text",
      marker = list(color = c("#2563eb", "#ca8a04", "#15803d"))
    ) %>%
      layout(
        title = "R2 Comparison Across Models",
        xaxis = list(title = ""),
        yaxis = list(title = "R2")
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
    recommended_label <- prediction_results()$recommended_model
    recommended_r2 <- prediction_results()$recommended_r2

    subtitle <- if (!is.na(recommended_r2)) {
      paste0("Recommended by Highest R2 (", round(recommended_r2, 3), ")")
    } else {
      "Recommended Prediction"
    }

    valueBox(
      format_rm(prediction_results()$recommended_price),
      paste(recommended_label, subtitle),
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

    recommendation_text <- if (!is.na(prediction_results()$recommended_r2)) {
      paste0(
        prediction_results()$recommended_model,
        " is recommended because it has the strongest saved R2 value of ",
        round(prediction_results()$recommended_r2, 3),
        "."
      )
    } else {
      paste0(prediction_results()$recommended_model, " is currently used as the recommended model.")
    }

    paste(
      "Linear Regression mapped selected area to:", prediction_results()$lm_area_value, "|",
      recommendation_text
    )
  })

  output$prediction_table <- renderTable({
    req(prediction_results())
    out <- prediction_results()$pred_df

    if (!is.null(model_metrics) && "R2" %in% names(model_metrics)) {
      out <- out %>%
        left_join(model_metrics[, c("Model", "R2")], by = "Model")
      out$R2 <- round(out$R2, 3)
    }

    out$Predicted_Price <- format_rm(out$Predicted_Price)
    out
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$prediction_plot <- renderPlotly({
    req(prediction_results())
    plot_df <- prediction_results()$pred_df

    if (!is.null(model_metrics) && "R2" %in% names(model_metrics)) {
      plot_df <- plot_df %>%
        left_join(model_metrics[, c("Model", "R2")], by = "Model")
    }

    plot_df$Hover_Text <- if ("R2" %in% names(plot_df)) {
      paste0("Predicted Price: ", format_rm(plot_df$Predicted_Price), "<br>R2: ", round(plot_df$R2, 3))
    } else {
      paste0("Predicted Price: ", format_rm(plot_df$Predicted_Price))
    }

    plot_ly(
      plot_df,
      x = ~Model,
      y = ~Predicted_Price,
      type = "bar",
      text = ~Hover_Text,
      hoverinfo = "text",
      marker = list(color = c("#2563eb", "#ca8a04", "#15803d"))
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Predicted Price", tickprefix = "RM ", separatethousands = TRUE)
      )
  })
}

shinyApp(ui = ui, server = server)

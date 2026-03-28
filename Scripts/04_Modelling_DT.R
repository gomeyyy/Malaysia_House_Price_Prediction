# ------------------
# Load Cleaned Dataset and reconvert datatypes
# ------------------
df <- read.csv("Datasets/cleaned_malaysia_house_price_data_2025.csv")

df$Township <- as.factor(df$Township)
df$Area <- as.factor(df$Area)
df$State <- as.factor(df$State)
df$Primary_Type <- as.factor(df$Primary_Type)

str(df)
summary(df)

# -----------------
# Modelling Prep
# -----------------
tree_model_df <- df[, c(
  "Log_Median_Price",
  "Township",
  "Area",
  "State",
  "Primary_Type",
  "Multi_Type",
  "Primary_Tenure",
  "Log_Median_PSF",
  "Log_Transactions"
)]
colSums(is.na(tree_model_df))

# Split dataset into train and test (80/20)
train_index <- createDataPartition(tree_model_df$Log_Median_Price, p = 0.8, list = FALSE)
tree_train_data <- tree_model_df[train_index, ]
tree_test_data <- tree_model_df[-train_index, ]

# setup cross validation
ctrl <- trainControl(
  method = "cv",
  number = 10
)

# Evaluation Function
evaluate_model <- function(actual, predicted) {
  rmse_val <- RMSE(predicted, actual)
  mae_val  <- MAE(actual, predicted)
  r2_val   <- R2(predicted, actual)
  
  data.frame(
    RMSE = rmse_val,
    MAE = mae_val,
    R2 = r2_val
  )
}


# ------------------
# Decision Tree Model
# ------------------

# Training

set.seed(123)

tree_model <- train(
  Log_Median_Price ~ Township + Area + State + Primary_Type + Multi_Type + Primary_Tenure + Log_Median_PSF + Log_Transactions,
  data = tree_train_data,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

tree_model$bestTune

print(tree_model)

tree_pred <- predict(tree_model, newdata = tree_test_data)
tree_results <- evaluate_model(tree_test_data$Log_Median_Price, tree_pred)
tree_results

rpart.plot(tree_model$finalModel)

tree_predicted_price <- exp(tree_pred)
tree_actual_price <- exp(tree_test_data$Log_Median_Price)

tree_prediction_df <- tree_test_data

tree_prediction_df$Actual <- tree_actual_price
tree_prediction_df$Predicted <- tree_predicted_price

# recreate original-scale predictors
tree_prediction_df$Median_PSF <- exp(tree_prediction_df$Log_Median_PSF)
tree_prediction_df$Transactions <- exp(tree_prediction_df$Log_Transactions)

tree_test_results_clean <- tree_prediction_df[, c(
  "Township",
  "Area",
  "State",
  "Primary_Type",
  "Multi_Type",
  "Primary_Tenure",
  "Median_PSF",
  "Transactions",
  "Actual",
  "Predicted"
)]

View(tree_test_results_clean)

options(scipen = 999)
plot(tree_actual_price, tree_predicted_price,
     main = "Actual vs Predicted Prices (Decision Tree)",
     xlab = "Actual Price",
     ylab = "Predicted Price")

abline(0, 1, col = "red")

write.csv(tree_test_results_clean, "Datasets/tree_model_predicted_price.csv", row.names = FALSE)
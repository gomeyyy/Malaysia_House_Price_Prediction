# ------------------
# Random Forest Prep
# ------------------

rf_df <- df[, c(
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

colSums(is.na(rf_df))

set.seed(123)
rf_train_index <- createDataPartition(rf_df$Log_Median_Price, p = 0.8, list = FALSE)
rf_train_data <- rf_df[rf_train_index, ]
rf_test_data  <- rf_df[-rf_train_index, ]

# reduced folds bcs rf training becomes too long to finish if use original
fast_ctrl <- trainControl(
  method = "cv",
  number = 5
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

# -------------------
# Random Forest Model
# -------------------

set.seed(123)

rf_model <- train(
  Log_Median_Price ~ Township + Area + State + Primary_Type + Multi_Type + Primary_Tenure + Log_Median_PSF + Log_Transactions,
  data = rf_train_data,
  method = "rf",
  trControl = fast_ctrl,
  tuneLength = 2,
  ntree = 200,
  importance = TRUE
)

rf_model$bestTune
print(rf_model)

# Predict on test set
rf_pred <- predict(rf_model, newdata = rf_test_data)
rf_results <- evaluate_model(rf_test_data$Log_Median_Price, rf_pred)
rf_results

# Convert log price back to actual price
rf_predicted_price <- exp(rf_pred)
rf_actual_price <- exp(rf_test_data$Log_Median_Price)

rf_prediction_df <- rf_test_data
rf_prediction_df$Actual <- rf_actual_price
rf_prediction_df$Predicted <- rf_predicted_price

# recreate original-scale predictors
rf_prediction_df$Median_PSF <- exp(rf_prediction_df$Log_Median_PSF)
rf_prediction_df$Transactions <- exp(rf_prediction_df$Log_Transactions)

# add error columns
rf_prediction_df$Error <- rf_prediction_df$Predicted - rf_prediction_df$Actual
rf_prediction_df$Abs_Error <- abs(rf_prediction_df$Error)
rf_prediction_df$Percent_Error <- (rf_prediction_df$Error / rf_prediction_df$Actual) * 100

rf_test_results_clean <- rf_prediction_df[, c(
  "Township",
  "Area",
  "State",
  "Primary_Type",
  "Multi_Type",
  "Primary_Tenure",
  "Median_PSF",
  "Transactions",
  "Actual",
  "Predicted",
  "Error",
  "Percent_Error"
)]

View(rf_test_results_clean)

options(scipen = 999)
plot(rf_actual_price, rf_predicted_price,
     main = "Actual vs Predicted Prices (Random Forest)",
     xlab = "Actual Price",
     ylab = "Predicted Price")
abline(0, 1, col = "red")

rf_imp <- varImp(rf_model, scale = TRUE)
rf_imp
rf_imp_df <- rf_imp$importance
rf_imp_df$Variable <- rownames(rf_imp_df)

rf_imp_df <- rf_imp_df[order(-rf_imp_df$Overall), ]

head(rf_imp_df, 15)

top15 <- head(rf_imp_df, 15)

par(mar = c(5, 12, 4, 2))
barplot(
  rev(top15$Overall),
  names.arg = rev(top15$Variable),
  horiz = TRUE,
  las = 1,
  main = "Top 15 Variable Importance (Random Forest)",
  xlab = "Importance",
  cex.names = 0.7
)
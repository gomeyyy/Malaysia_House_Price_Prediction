# ------------------
# Model Comparison
# ------------------

# MAPE (Mean Absolute Percentage Error) function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Actual-scale values
lm_actual <- exp(test_data$Log_Median_Price)
lm_pred_actual <- exp(lm_pred)

tree_actual <- exp(tree_test_data$Log_Median_Price)
tree_pred_actual <- exp(tree_pred)

rf_actual <- exp(rf_test_data$Log_Median_Price)
rf_pred_actual <- exp(rf_pred)

# MAPE
lm_mape <- mape(lm_actual, lm_pred_actual)
tree_mape <- mape(tree_actual, tree_pred_actual)
rf_mape <- mape(rf_actual, rf_pred_actual)

# Log-scale comparison
log_scale_comparison <- data.frame(
  Model = c("Multiple Linear Regression", "Decision Tree", "Random Forest"),
  RMSE = c(lm_results$RMSE, tree_results$RMSE, rf_results$RMSE),
  MAE = c(lm_results$MAE, tree_results$MAE, rf_results$MAE),
  R2 = c(lm_results$R2, tree_results$R2, rf_results$R2),
  MAPE_Percent = c(lm_mape, tree_mape, rf_mape)
)

log_scale_comparison <- log_scale_comparison[order(log_scale_comparison$RMSE), ]
log_scale_comparison

# Actual-scale comparison
lm_results_actual <- evaluate_model(lm_actual, lm_pred_actual)
tree_results_actual <- evaluate_model(tree_actual, tree_pred_actual)
rf_results_actual <- evaluate_model(rf_actual, rf_pred_actual)

actual_scale_comparison <- data.frame(
  Model = c("Multiple Linear Regression", "Decision Tree", "Random Forest"),
  RMSE = c(lm_results_actual$RMSE, tree_results_actual$RMSE, rf_results_actual$RMSE),
  MAE = c(lm_results_actual$MAE, tree_results_actual$MAE, rf_results_actual$MAE),
  R2 = c(lm_results_actual$R2, tree_results_actual$R2, rf_results_actual$R2),
  MAPE_Percent = c(lm_mape, tree_mape, rf_mape)
)

actual_scale_comparison <- actual_scale_comparison[order(actual_scale_comparison$RMSE), ]
actual_scale_comparison$Approx_Accuracy <- 100 - actual_scale_comparison$MAPE_Percent
actual_scale_comparison
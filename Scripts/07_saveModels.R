# ------------------
# Save Reference Dataset
# ------------------
saveRDS(df, "price_prediction_app/reference_data.rds")

# ------------------
# Save trained models
# ------------------
saveRDS(lm_model, "price_prediction_app/lm_model.rds")
saveRDS(tree_model, "price_prediction_app/tree_model.rds")
saveRDS(rf_model, "price_prediction_app/rf_model.rds")
saveRDS(df, "price_prediction_app/reference_data.rds")
saveRDS(top_areas, "price_prediction_app/lm_top_areas.rds")

# ------------------
# save comparison table
# ------------------
write.csv(actual_scale_comparison, "price_prediction_app/model_comparison.csv", row.names = FALSE)

# ------------------
# save csvs in app folder
# ------------------
write.csv(lm_test_results_clean, "price_prediction_app/linear_model_predicted_price.csv", row.names = FALSE)
write.csv(tree_test_results_clean, "price_prediction_app/tree_model_predicted_price.csv", row.names = FALSE)
write.csv(rf_test_results_clean, "price_prediction_app/rf_model_predicted_price.csv", row.names = FALSE)
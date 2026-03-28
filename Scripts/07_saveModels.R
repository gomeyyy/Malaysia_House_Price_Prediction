# ------------------
# Save Reference Dataset
# ------------------
saveRDS(df, "reference_data.rds")

# ------------------
# Save trained models
# ------------------
saveRDS(lm_model, "lm_model.rds")
saveRDS(tree_model, "tree_model.rds")
saveRDS(rf_model, "rf_model.rds")
saveRDS(df, "reference_data.rds")
saveRDS(top_areas, "lm_top_areas.rds")
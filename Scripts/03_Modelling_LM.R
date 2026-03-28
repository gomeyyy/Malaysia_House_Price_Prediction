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
lm_df <- df[, c(
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
colSums(is.na(lm_df))

# Split dataset into train and test (80/20)
train_index <- createDataPartition(lm_df$Log_Median_Price, p = 0.8, list = FALSE)
train_data <- lm_df[train_index, ]
test_data <- lm_df[-train_index, ]

# Save area copy
train_data$Area_Actual <- train_data$Area
test_data$Area_Actual  <- test_data$Area

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


# -------------------
# Linear Regression Model
# -------------------

# Training

# Find top 20 Area levels from training data only
top_areas <- names(sort(table(train_data$Area), decreasing = TRUE))[1:20]
# Recode both train and test using the top levels
train_data$Area <- fct_other(train_data$Area, keep = top_areas, other_level = "Other")
test_data$Area  <- fct_other(test_data$Area, keep = top_areas, other_level = "Other")
# Ensure test has exactly same levels as train
test_data$Area <- factor(test_data$Area, levels = levels(train_data$Area))

set.seed(123)
lm_model <- train(
  Log_Median_Price ~ Area + Primary_Type + Multi_Type + Primary_Tenure + Log_Median_PSF + Log_Transactions,
  data = train_data,
  method = "lm",
  trControl = ctrl
)

# Predict on test set
lm_pred <- predict(lm_model, newdata = test_data)
lm_results <- evaluate_model(test_data$Log_Median_Price, lm_pred)
lm_results
summary(lm_model$finalModel)

# View Prediction on test data

# convert log price into actual price
predicted_price <- exp(lm_pred)
actual_price <- exp(test_data$Log_Median_Price)

lm_prediction_df <- test_data

lm_prediction_df$Actual <- actual_price
lm_prediction_df$Predicted <- predicted_price

# recreate original-scale predictors for easier viewing
lm_prediction_df$Median_PSF <- exp(lm_prediction_df$Log_Median_PSF)
lm_prediction_df$Transactions <- exp(lm_prediction_df$Log_Transactions)

# select useful columns only
lm_test_results_clean <- lm_prediction_df[, c(
  "Area_Actual",
  "Area", #Model Area
  "Primary_Type",
  "Multi_Type",
  "Primary_Tenure",
  "Median_PSF",
  "Transactions",
  "Actual",
  "Predicted"
)]

View(lm_test_results_clean)

options(scipen = 999)
plot(actual_price, predicted_price,
     main = "Actual vs Predicted Prices (Linear Model)",
     xlab = "Actual Price",
     ylab = "Predicted Price")

abline(0, 1, col = "red")

write.csv(lm_test_results_clean, "Datasets/linear_model_predicted_price.csv", row.names = FALSE)

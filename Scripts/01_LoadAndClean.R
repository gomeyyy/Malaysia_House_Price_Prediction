
# ------------------
# Load Dataset
# ------------------
df <- read.csv("Datasets/malaysia_house_price_data_2025.csv")

# ------------------
# Check df structure before pre processing
# ------------------
str(df)
summary(df)
head(df)

# ------------------
# handle missing values
# ------------------
colSums(is.na(df))
df <- na.omit(df)

# ------------------
# handle duplicate rows
# ------------------
sum(duplicated(df))
df <- unique(df)

# ------------------
# handling structure
# ------------------
# Type has multi-values, create column Primary_Type and Multi_Type
# standardize writing to lowercase
df$Type <- trimws(tolower(df$Type))
# 1. Multi_Type (check if comma exists)
df$Multi_Type <- as.numeric(grepl(",", df$Type))
# 2. Primary_Type (first value only)
df$Primary_Type <- sapply(strsplit(df$Type, ","), function(x) trimws(x[1]))
# Convert to factor
df$Primary_Type <- as.factor(df$Primary_Type)
# 3. Drop original Type column
df$Type <- NULL

# Tenure has multi-values, convert into 2 numerical columns
# standardize writing to lowercase
df$Tenure <- trimws(tolower(df$Tenure))
# create freehold and leasehold columns
df$Freehold <- as.numeric(grepl("freehold", df$Tenure))
df$Leasehold <- as.numeric(grepl("leasehold", df$Tenure))
# create primary tenure column to be used for linear regression
df$Primary_Tenure <- df$Freehold
# Drop original column
df$Tenure <- NULL

# handle text columns (State, Area, Township)
df$Township <- trimws(tolower(df$Township))
df$Area <- trimws(tolower(df$Area))
df$State <- trimws(tolower(df$State))
df$Township <- as.factor(df$Township)
df$Area <- as.factor(df$Area)
df$State <- as.factor(df$State)

# handle numerical columns
summary(df[, c("Median_Price", "Median_PSF", "Transactions")])
df$Median_Price <- as.numeric(df$Median_Price)
df$Median_PSF <- as.numeric(df$Median_PSF)
df$Transactions <- as.numeric(df$Transactions)
# view outliers
boxplot(df$Median_Price, main = "Median Price")
boxplot(df$Median_PSF, main = "Median PSF")
boxplot(df$Transactions, main = "Transactions")
# apply log transformation to reduce skewness in numerical variables
df$Log_Median_Price <- log(df$Median_Price)
df$Log_Median_PSF <- log(df$Median_PSF)
df$Log_Transactions <- log(df$Transactions)
# view standardized numerical columns
boxplot(df$Log_Median_Price, main = "Log Median Price")
boxplot(df$Log_Median_PSF, main = "Log Median PSF")
boxplot(df$Log_Transactions, main = "Log Transactions")

# check new columns
unique(df$Primary_Type)
table(df$Primary_Type)
table(df$Multi_Type)
table(df$Freehold)
table(df$Leasehold)

# ------------------
# Check df structure after pre processing
# ------------------
str(df)
summary(df)
head(df)
colSums(is.na(df))
sum(duplicated(df))

# Final variables:
# Township      -> factor
# Area          -> factor
# State         -> factor
# Primary_Type  -> factor
# Multi_Type    -> numeric (0/1)
# Freehold      -> numeric (0/1)
# Leasehold     -> numeric (0/1)
# Median_Price  -> numeric
# Median_PSF    -> numeric
# Transactions  -> numeric

# ------------------
# Save cleaned dataset
# ------------------
write.csv(df, "Datasets/cleaned_malaysia_house_price_data_2025.csv", row.names = FALSE)


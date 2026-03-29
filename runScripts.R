# ======================================
# run once
source("Scripts/00_ImportLibraries.R")
# ======================================

# ======================================
# Data Preparation and Understanding
# ======================================
# 1. Load and Clean
source("Scripts/01_LoadAndClean.R")
# 2. EDA Script
# ======================================

# ======================================
# Modelling
# ======================================
# 3. Linear Model
source("Scripts/03_Modelling_LM.R")
# 4. Decision Tree
source("Scripts/04_Modelling_DT.R")
# 5. Random Forest
source("Scripts/05_Modelling_RF.R")
# 6. Compare Models
source("Scripts/06_ModelComparison.R")
# ======================================

# ======================================
# Save RDS and start App
# ======================================
source("Scripts/07_saveModels.R")
runApp('price_prediction_app')
# ======================================

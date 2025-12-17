set.seed(607)

# Load and sample raw data
source("scripts/load_data.R")

# Run EDA and create cleaned dataset
source("scripts/eda.R")

# Create train, validation, and test datasets
source("scripts/dataset_creation.R")

# Fit models on training and validation data
source("scripts/random_forest.R")
source("scripts/multicollinearity_check.R")
source("scripts/basic_linear_model.R")
source("scripts/full_linear_model.R")
source("scripts/lasso_model.R")
source("scripts/xgboost_model.R")

# Compare models on test split
source("scripts/evaluation_test.R")

# Train final random forest on train plus validation and evaluate on test
source("scripts/model_test_data.R")

# Create additional data visualizations and summaries
source("scripts/get_value_codes.R")
source("scripts/other_visualizations.R")

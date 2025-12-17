# Project Overview
This project uses data from the 2018-2022 5-Year American Community Survey, run by the United States Census Bureau, in order to identify how well annual household income can be predicted based on demographic (age, sex, race, marital status), education level, employment status, number of people per household, housing structure (ownership, number of vehicles), and disability status.

# Data
Source: 2018–2022 5-year ACS data accessed via the tidycensus package.\
Unit of analysis: households.\
Sample: random 0.10% sample of households from the full ACS file.\
Restrictions:
+ Only households with positive reported income.
+ Only households with complete information on all predictors used in the models.

Income is highly right-skewed, so the models are trained on the logarithm of income and predictions are back-transformed into dollar units for interpretation.

# Methods
Split the cleaned data into training and test sets\
Trained and cross-validated four models:
+ Linear regression
+ Lasso regression
+ Random forest
+ XGBoost

Evaluated models using RMSE and MAE on the log-income scale.\
Selected the random forest as the final model (RMSE = 0.85, MAE = 0.59), with linear and lasso models performing similarly and XGBoost performing the worst.

# Repository Structure
data/ - sample, cleaned, train, cross-validation, and datasets\
outputs/
+ figures/ - visualizations for data analysis
+ metrics/ - metrics in csv files to analyze model performance
+ models/ - stores all models as .rds files for easy access

scripts/ - all scripts used

# Requirements
R (latest version preferred).\
R packages tidycensus, tidyverse, tidymodels, randomForest, car, glmnet, xgboost.\
At least 90 MB of space.

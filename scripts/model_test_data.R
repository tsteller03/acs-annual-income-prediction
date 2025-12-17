library("tidyverse")
library("randomForest")

train_raw <- readRDS("data/acs_train.rds")
val_raw   <- readRDS("data/acs_val.rds")
test_raw  <- readRDS("data/acs_test.rds")

# Combine train and validation for final training
full_train_raw <- bind_rows(train_raw, val_raw)

# Build modeling dataset for final training
rf_train <- full_train_raw %>%
  select(
    HINCP_log,  # Outcome (log income)
    
    # Demographics
    AGEP,    # Age
    SEX,     # Sex
    RAC1P,   # Race
    MAR,     # Marital status
    
    # Education
    SCHL,    # Education level
    
    # Employment
    ESR,     # Employment status
    
    # Household structure
    NP,      # Number of people in household
    
    # Housing structure
    TEN,     # Own or rent
    VEH,     # Vehicles
    
    # Disability
    DIS      # Disability status
  ) %>%
  drop_na()

rf_train <- rf_train %>%
  mutate(
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR   = as.factor(MAR),
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    DIS   = as.factor(DIS)
  )

# Build modeling dataset for test
rf_test <- test_raw %>%
  select(
    HINCP_log,  # Outcome (log income)
    
    # Demographics
    AGEP,    # Age
    SEX,     # Sex
    RAC1P,   # Race
    MAR,     # Marital status
    
    # Education
    SCHL,    # Education level
    
    # Employment
    ESR,     # Employment status
    
    # Household structure
    NP,      # Number of people in household
    
    # Housing structure
    TEN,     # Own or rent
    VEH,     # Vehicles
    
    # Disability
    DIS      # Disability status
  ) %>%
  drop_na()

rf_test <- rf_test %>%
  mutate(
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR   = as.factor(MAR),
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    DIS   = as.factor(DIS)
  )

# Fit final random forest on train + validation
set.seed(607)
rf_final <- randomForest(HINCP_log ~ ., data = rf_train, ntree = 300, importance = TRUE)
rf_final
saveRDS(rf_final, "outputs/models/random_forest_final.rds")

# Predict on test set
test_pred_log <- predict(rf_final, newdata = rf_test)

# Metrics on log scale
rmse_log <- sqrt(mean((test_pred_log - rf_test$HINCP_log)^2))
mae_log  <- mean(abs(test_pred_log - rf_test$HINCP_log))

# Compute metrics on income scale
test_results <- tibble(
  truth_log = rf_test$HINCP_log,
  pred_log  = test_pred_log
) %>%
  mutate(
    truth_income = exp(truth_log),
    pred_income  = exp(pred_log)
  )

rmse_income <- sqrt(mean((test_results$pred_income - test_results$truth_income)^2))
mae_income  <- mean(abs(test_results$pred_income - test_results$truth_income))

# Save test metrics
rf_test_metrics <- tibble(
  model = "random_forest",
  rmse_log = rmse_log,
  mae_log = mae_log,
  rmse_income = rmse_income,
  mae_income = mae_income
)

rf_test_metrics
write_csv(rf_test_metrics, "outputs/metrics/final/rf_test_metrics.csv")

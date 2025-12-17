library("tidyverse")
library("randomForest")

# Load train and validation data
train_raw <- readRDS("data/acs_train.rds")
val_raw   <- readRDS("data/acs_val.rds")

# Build modeling dataset for training - occupation and industry excluded
acs_train <- train_raw %>%
  select(
    HINCP_log,   # Outcome (log income)
    
    # Demographics
    AGEP,        # Age
    SEX,         # Sex
    RAC1P,       # Race
    MAR,         # Marital status
    
    # Education
    SCHL,        # Education level
    
    # Employment
    ESR,         # Employment status
    
    # Household structure
    NP,          # Number of people in household
    
    # Housing structure
    TEN,         # Own or rent
    VEH,         # Vehicles
    
    # Disability
    DIS          # Disability status
  ) %>%
  drop_na()

# Ensure categorical variables are factors
acs_train <- acs_train %>%
  mutate(
    SEX = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR = as.factor(MAR),
    SCHL = as.factor(SCHL),
    ESR = as.factor(ESR),
    TEN = as.factor(TEN),
    DIS = as.factor(DIS)
  )

# Build modeling dataset for validation
acs_val <- val_raw %>%
  select(
    HINCP_log,
    AGEP,
    SEX,
    RAC1P,
    MAR,
    SCHL,
    ESR,
    NP,
    TEN,
    VEH,
    DIS
  ) %>%
  drop_na()

acs_val <- acs_val %>%
  mutate(
    SEX = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR = as.factor(MAR),
    SCHL = as.factor(SCHL),
    ESR = as.factor(ESR),
    TEN = as.factor(TEN),
    DIS = as.factor(DIS)
  )

# Fit random forest on training data
set.seed(607)
rf_mod <- randomForest(HINCP_log ~ ., data = acs_train, ntree = 300, importance = TRUE)
rf_mod

# Variable importance
imp_vars <- importance(rf_mod, type = 1) %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  arrange(desc(`%IncMSE`))

imp_vars

write_csv(imp_vars, "outputs/metrics/random_forest/rf_importance.csv")
saveRDS(rf_mod, "outputs/models/rf_mod.rds")

# Validation performance
val_pred <- predict(rf_mod, newdata = acs_val)

rmse_log <- sqrt(mean((val_pred - acs_val$HINCP_log)^2))
mae_log  <- mean(abs(val_pred - acs_val$HINCP_log))

rf_val_metrics <- tibble(
  model = "random_forest",
  rmse_log = rmse_log,
  mae_log  = mae_log
)

rf_val_metrics

write_csv(rf_val_metrics, "outputs/metrics/random_forest/random_forest_validation_metrics.csv")
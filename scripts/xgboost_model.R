library("tidyverse")
library("xgboost")

train_raw <- readRDS("data/acs_train.rds")
val_raw   <- readRDS("data/acs_val.rds")

# Build dataset for training
xgb_train <- train_raw %>%
  mutate(
    HINCP_log = as.numeric(HINCP_log),
    AGEP = as.numeric(AGEP),
    NP   = as.numeric(NP),
    VEH  = as.numeric(VEH)
  ) %>%
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

xgb_train <- xgb_train %>%
  mutate(
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR   = as.factor(MAR),
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    DIS   = as.factor(DIS)
  )

# Build dataset for validation
xgb_val <- val_raw %>%
  mutate(
    HINCP_log = as.numeric(HINCP_log),
    AGEP = as.numeric(AGEP),
    NP   = as.numeric(NP),
    VEH  = as.numeric(VEH)
  ) %>%
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

xgb_val <- xgb_val %>%
  mutate(
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR   = as.factor(MAR),
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    DIS   = as.factor(DIS)
  )

# Combine train and validation to build a common model matrix
combined <- bind_rows(
  xgb_train %>% mutate(.dataset = "train"),
  xgb_val   %>% mutate(.dataset = "val")
)

x_all <- model.matrix(HINCP_log ~ . - .dataset, data = combined)[, -1]
y_all <- combined$HINCP_log

train_index <- combined$.dataset == "train"

x_train <- x_all[train_index, , drop = FALSE]
y_train <- y_all[train_index]

x_val <- x_all[!train_index, , drop = FALSE]
y_val <- y_all[!train_index]

# XGBoost model for regression on training data
set.seed(607)
xgb_model <- xgboost(
  data        = x_train,
  label       = y_train,
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  nrounds     = 200,
  verbose     = 0
)

# Predict on validation set
y_pred_log <- predict(xgb_model, newdata = x_val)

# Validation accuracy metrics on log scale
rmse_log <- sqrt(mean((y_val - y_pred_log)^2))
mae_log  <- mean(abs(y_val - y_pred_log))

xgb_val_metrics <- tibble(
  model = "xgboost",
  rmse_log = rmse_log,
  mae_log = mae_log,
  nrounds = 200
)

xgb_val_metrics

write_csv(xgb_val_metrics, "outputs/metrics/xgboost/xgb_validation_metrics.csv")

# Variable importance table based on training 
xgb_imp <- xgb.importance(
  model = xgb_model,
  feature_names = colnames(x_train)
)

xgb_imp_tbl <- as_tibble(xgb_imp)

write_csv(xgb_imp_tbl, "outputs/metrics/xgboost/xgb_importance.csv")
saveRDS(xgb_model, "outputs/models/xgb_model.rds")

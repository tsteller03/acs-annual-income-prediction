library("tidyverse")
library("glmnet")

train_raw <- readRDS("data/acs_train.rds")
val_raw   <- readRDS("data/acs_val.rds")

# Build dataset for training
lasso_train <- train_raw %>%
  mutate(
    HINCP_log = as.numeric(HINCP_log),
    AGEP = as.numeric(AGEP),
    NP   = as.numeric(NP),
    VEH  = as.numeric(VEH)
  ) %>%
  select(
    HINCP_log,
    
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

lasso_train <- lasso_train %>%
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
lasso_val <- val_raw %>%
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

lasso_val <- lasso_val %>%
  mutate(
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR   = as.factor(MAR),
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    DIS   = as.factor(DIS)
  )

# Combine train and validation to build common model matrix
combined <- bind_rows(
  lasso_train %>% mutate(.dataset = "train"),
  lasso_val   %>% mutate(.dataset = "val")
)

x_all <- model.matrix(HINCP_log ~ . - .dataset, data = combined)[, -1]
y_all <- combined$HINCP_log

train_index <- combined$.dataset == "train"

x_train <- x_all[train_index, , drop = FALSE]
y_train <- y_all[train_index]

x_val <- x_all[!train_index, , drop = FALSE]

# Cross-validated LASSO on training data
set.seed(607)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, standardize = TRUE)

best_lambda <- lasso_cv$lambda.min
best_lambda

# Fit final LASSO at chosen lambda on training data
lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda, standardize = TRUE)

# Coefficients
coef_lasso <- coef(lasso_mod)

coef_df <- data.frame(
  term = rownames(coef_lasso),
  estimate = as.numeric(coef_lasso)
)

write_csv(coef_df, "outputs/metrics/lasso/lasso_coefficients.csv")

# Validation performance
val_pred_log <- predict(lasso_mod, newx = x_val)

rmse_log <- sqrt(mean((val_pred_log - lasso_val$HINCP_log)^2))
mae_log  <- mean(abs(val_pred_log - lasso_val$HINCP_log))

lasso_val_metrics <- tibble(
  model = "lasso",
  rmse_log = rmse_log,
  mae_log  = mae_log
)

lasso_val_metrics

write_csv(lasso_val_metrics, "outputs/metrics/lasso/lasso_validation_metrics.csv")

saveRDS(lasso_mod, "outputs/models/lasso_model.rds")
saveRDS(lasso_cv,  "outputs/models/lasso_cv.rds")

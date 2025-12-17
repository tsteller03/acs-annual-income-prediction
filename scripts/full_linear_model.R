library("tidyverse")

train_raw <- readRDS("data/acs_train.rds")
val_raw   <- readRDS("data/acs_val.rds")

# Build modeling dataset for training - using all predictors
lm_full_train <- train_raw %>%
  mutate(
    HINCP_log = as.numeric(HINCP_log),
    AGEP = as.numeric(AGEP),
    NP   = as.numeric(NP),
    VEH  = as.numeric(VEH)
  ) %>%
  select(
    HINCP_log,
    
    # Demographics
    AGEP,   # Age
    SEX,    # Sex
    RAC1P,  # Race
    MAR,    # Marital status
    
    # Education
    SCHL,   # Educational level
    
    # Employment
    ESR,    # Employment status
    
    # Household structure
    NP,     # Number of people in household
    
    # Housing structure
    TEN,    # Own or rent
    VEH,    # Vehicles
    
    # Disability
    DIS     # Disability status
  ) %>%
  drop_na()

# Make sure categorical variables are factors
lm_full_train <- lm_full_train %>%
  mutate(
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    MAR   = as.factor(MAR),
    DIS   = as.factor(DIS),
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P)
  )

# Build modeling dataset for validation
lm_full_val <- val_raw %>%
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

lm_full_val <- lm_full_val %>%
  mutate(
    SCHL  = as.factor(SCHL),
    ESR   = as.factor(ESR),
    TEN   = as.factor(TEN),
    MAR   = as.factor(MAR),
    DIS   = as.factor(DIS),
    SEX   = as.factor(SEX),
    RAC1P = as.factor(RAC1P)
  )

# Fit full linear model on log income using training data
lm_full <- lm(HINCP_log ~ ., data = lm_full_train)
lm_full_summary <- summary(lm_full)
lm_full_summary

# Predictions on validation set
val_pred_log_full <- predict(lm_full, newdata = lm_full_val)

# Validation accuracy metrics on log scale
rmse_log_full <- sqrt(mean((lm_full_val$HINCP_log - val_pred_log_full)^2))
mae_log_full  <- mean(abs(lm_full_val$HINCP_log - val_pred_log_full))

# Put validation metrics into data frame for model comparison
lm_full_val_metrics <- tibble(
  model = "full_linear_model",
  rmse_log = rmse_log_full,
  mae_log  = mae_log_full
)

lm_full_val_metrics

write_csv(lm_full_val_metrics, "outputs/metrics/full_lm/full_linear_validation_metrics.csv")

# Observed vs predicted (log) plot for full model on validation set
plot_data_full <- tibble(
  HINCP_log = lm_full_val$HINCP_log,
  pred_log = val_pred_log_full
)

ggplot(plot_data_full, aes(x = HINCP_log, y = pred_log)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "Observed vs predicted (log income) – full linear model",
    x = "Actual log(HINCP)",
    y = "Predicted log(HINCP)"
  )

ggsave("outputs/figures/full_lm/full_lm_observed_vs_predicted.png")

saveRDS(lm_full, "outputs/models/full_linear_model.rds")

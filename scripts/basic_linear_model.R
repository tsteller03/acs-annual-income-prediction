library("tidyverse")

train_raw <- readRDS("data/acs_train.rds")
val_raw   <- readRDS("data/acs_val.rds")

# Build modeling dataset for training - using main predictors per RF
lm_train <- train_raw %>%
  mutate(
    HINCP_log = as.numeric(HINCP_log),
    AGEP = as.numeric(AGEP),
    NP = as.numeric(NP),
    VEH = as.numeric(VEH)
  ) %>%
  select(
    HINCP_log,
    
    # Demographic
    AGEP, # Age
    SCHL, # Educational level
    ESR,  # Employment status
    
    # Household structure
    NP,   # Number of people in household
    TEN,  # Own or rent
    VEH   # Vehicles
  ) %>%
  drop_na()

# Make sure categorical variables are factors
lm_train <- lm_train %>%
  mutate(
    SCHL = as.factor(SCHL),
    ESR  = as.factor(ESR),
    TEN  = as.factor(TEN)
  )

# Build modeling dataset for validation
lm_val <- val_raw %>%
  mutate(
    HINCP_log = as.numeric(HINCP_log),
    AGEP = as.numeric(AGEP),
    NP = as.numeric(NP),
    VEH = as.numeric(VEH)
  ) %>%
  select(
    HINCP_log,
    AGEP,
    SCHL,
    ESR,
    NP,
    TEN,
    VEH
  ) %>%
  drop_na()

lm_val <- lm_val %>%
  mutate(
    SCHL = as.factor(SCHL),
    ESR  = as.factor(ESR),
    TEN  = as.factor(TEN)
  )

# Fit basic linear model on log income using training data
lm_basic <- lm(HINCP_log ~ ., data = lm_train)
lm_summary <- summary(lm_basic)
lm_summary

# Predictions on validation set
val_pred_log <- predict(lm_basic, newdata = lm_val)

# Validation accuracy metrics
rmse_log <- sqrt(mean((lm_val$HINCP_log - val_pred_log)^2))
mae_log  <- mean(abs(lm_val$HINCP_log - val_pred_log))

# Put validation metrics into data frame for model comparison
lm_val_metrics <- tibble(
  model = "basic_linear_model",
  rmse_log = rmse_log,
  mae_log  = mae_log
)

lm_val_metrics

write_csv(lm_val_metrics, "outputs/metrics/basic_lm/basic_linear_validation_metrics.csv")

# Observed vs predicted (log) plot on validation set
plot_data <- tibble(
  HINCP_log = lm_val$HINCP_log,
  pred_log  = val_pred_log
)

ggplot(plot_data, aes(x = HINCP_log, y = pred_log)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "Observed vs predicted (log income) - basic linear model",
    x = "Actual log(HINCP)",
    y = "Predicted log(HINCP)"
  )

ggsave("outputs/figures/basic_lm/basic_lm_observed_vs_predicted.png")

saveRDS(lm_basic, "outputs/models/basic_linear_model.rds")

library("tidyverse")

# Read all validation metric files
basic_lm_val <- read_csv("outputs/metrics/basic_lm/basic_linear_validation_metrics.csv", show_col_types = FALSE)
full_lm_val  <- read_csv("outputs/metrics/full_lm/full_linear_validation_metrics.csv", show_col_types = FALSE)
lasso_val <- read_csv("outputs/metrics/lasso/lasso_validation_metrics.csv", show_col_types = FALSE)
rf_val <- read_csv("outputs/metrics/random_forest/random_forest_validation_metrics.csv", show_col_types = FALSE)
xgb_val <- read_csv("outputs/metrics/xgboost/xgb_validation_metrics.csv", show_col_types = FALSE)

# Sort into a new csv
all_val_metrics <- bind_rows(
  basic_lm_val,
  full_lm_val,
  lasso_val,
  rf_val,
  xgb_val
)

all_val_metrics_sorted <- all_val_metrics %>%
  arrange(rmse_log, mae_log)
all_val_metrics_sorted

write_csv(all_val_metrics_sorted,"outputs/metrics/evaluation/model_comparison_validation_metrics.csv")

library("tidyverse")

lm_full <- readRDS("outputs/models/full_linear_model.rds")

# Get fitted values and residuals
fitted_vals <- fitted(lm_full)
resid_vals  <- resid(lm_full)

diag_data <- tibble(
  fitted    = fitted_vals,
  residuals = resid_vals
)

# Residuals vs fitted plot
ggplot(diag_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values (log income)",
    y = "Residuals"
  )

ggsave("outputs/figures/full_lm_residuals_vs_fitted.png")

# Residuals histogram
ggplot(diag_data, aes(x = residuals)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Residuals Histogram",
    x = "Residuals",
    y = "Count"
  )

ggsave("outputs/figures/full_lm_residuals_histogram.png")

# Quantile-quantile plot of residuals
ggplot(diag_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Residuals QQ Plot",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  )

ggsave("outputs/figures/full_lm_residuals_qqplot.png")

# Check with Cook's distance
cooks_vals <- cooks.distance(lm_full)

cooks_data <- tibble(
  obs_index = seq_along(cooks_vals),
  cooksd    = cooks_vals
)

ggplot(cooks_data, aes(x = obs_index, y = cooksd)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Cook's Distance",
    x = "Observation index",
    y = "Cook's distance"
  )

ggsave("outputs/figures/full_lm_cooks_distance.png")

# Save top 50 most influential points
top_influence <- cooks_data %>%
  arrange(desc(cooksd)) %>%
  head(50)

write_csv(top_influence, "outputs/metrics/full_lm_top_influence_points.csv")

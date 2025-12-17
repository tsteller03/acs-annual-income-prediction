library("tidyverse")
library("scales")

acs_clean <- readRDS("data/acs_eda_clean.rds")

acs_clean <- acs_clean %>%
  mutate(
    HINCP = as.numeric(HINCP),
    HINCP_log = as.numeric(HINCP_log)
  ) %>%
  filter(
    !is.na(HINCP),
    HINCP > 0
  )

# Income bands and indicators
acs_clean <- acs_clean %>%
  mutate(
    income_band = case_when(
      HINCP < 40000 ~ "Low (<$40k)",
      HINCP < 100000 ~ "Middle ($40k–$100k)",
      TRUE ~ "High (≥$100k)"
    ),
    low_income = HINCP < 40000,
    mid_income = HINCP >= 40000 & HINCP < 100000,
    high_income = HINCP >= 100000
  )

# Numeric versions of key codes
acs_clean <- acs_clean %>%
  mutate(
    DIS_num = as.numeric(as.character(DIS)),
    TEN_num = as.numeric(as.character(TEN)),
    ESR_num = as.numeric(as.character(ESR)),
    SCHL_num = as.numeric(as.character(SCHL))
  )

# Labels for key categorical codes
acs_clean <- acs_clean %>%
  mutate(
    DIS_label = case_when(
      DIS_num == 1 ~ "With disability",
      DIS_num == 2 ~ "No disability",
      TRUE ~ "Other / unknown"
    ),
    TEN_label = case_when(
      TEN_num == 1 ~ "Owned, with mortgage",
      TEN_num == 2 ~ "Owned, without mortgage",
      TEN_num == 3 ~ "Rented",
      TEN_num == 4 ~ "Occupied, no rent paid",
      TRUE ~ "Other / unknown"
    ),
    ESR_label = case_when(
      ESR_num == 1 ~ "Employed, at work",
      ESR_num == 2 ~ "Employed with job, not at work",
      ESR_num == 3 ~ "Unemployed",
      ESR_num == 4 ~ "Armed forces, at work",
      ESR_num == 5 ~ "Armed forces with job, not at work",
      ESR_num == 6 ~ "Not in labor force",
      TRUE ~ "Other / unknown"
    ),
    SCHL_label = case_when(
      SCHL_num < 16 ~ "Less than high school",
      SCHL_num == 16 ~ "High school diploma",
      SCHL_num == 17 ~ "GED or alternative",
      SCHL_num == 18 ~ "Some college, <1 year",
      SCHL_num == 19 ~ "Some college, 1+ years",
      SCHL_num == 20 ~ "Associates degree",
      SCHL_num == 21 ~ "Bachelors degree",
      SCHL_num == 22 ~ "Masters degree",
      SCHL_num == 23 ~ "Professional degree",
      SCHL_num == 24 ~ "Doctorate degree",
      TRUE ~ "Other / unknown"
    )
  )

# Make ESR_label an ordered factor
esr_levels <- c(
  "Employed, at work",
  "Employed with job, not at work",
  "Unemployed",
  "Armed forces, at work",
  "Armed forces with job, not at work",
  "Not in labor force"
)

acs_clean <- acs_clean %>%
  mutate(
    ESR_label = factor(ESR_label, levels = c(esr_levels, "Other / unknown"))
  )

# Income band distribution
band_summary <- acs_clean %>%
  count(income_band) %>%
  mutate(prop = n / sum(n))

write_csv(band_summary, "outputs/metrics/other/income_band_distribution.csv")

ggplot(band_summary, aes(x = income_band, y = prop)) +
  geom_col() +
  geom_text(aes(label = round(prop * 100, 1)), vjust = -0.3) +
  labs(
    title = "Distribution of household income bands",
    x = "Income band",
    y = "Share of households"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("outputs/figures/other/income_band_distribution.png")

# Overall stacked low, middle, high share
band_stack <- acs_clean %>%
  mutate(
    band_simple = case_when(
      low_income ~ "Low",
      mid_income ~ "Middle",
      high_income ~ "High"
    )
  ) %>%
  count(band_simple) %>%
  mutate(
    prop = n / sum(n),
    band_simple = factor(band_simple, levels = c("Low", "Middle", "High"))
  )

ggplot(band_stack, aes(x = "", y = prop, fill = band_simple)) +
  geom_col() +
  geom_text(
    aes(label = round(prop * 100, 1)),
    position = position_stack(vjust = 0.5)
  ) +
  coord_flip() +
  labs(
    title = "Overall share of low, middle, and high income households",
    x = NULL,
    y = "Share of households",
    fill = "Income band"
  )

ggsave("outputs/figures/other/income_band_overall_stacked.png")

# Education summary (grouped by label only)
edu_summary <- acs_clean %>%
  group_by(SCHL_label) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(SCHL_label != "Other / unknown") %>%
  mutate(
    SCHL_label = factor(
      SCHL_label,
      levels = c(
        "Less than high school",
        "High school diploma",
        "GED or alternative",
        "Some college, <1 year",
        "Some college, 1+ years",
        "Associates degree",
        "Bachelors degree",
        "Masters degree",
        "Professional degree",
        "Doctorate degree"
      )
    )
  ) %>%
  arrange(SCHL_label)

write_csv(edu_summary, "outputs/metrics/other/income_by_education_low_mid_high.csv")

edu_long <- edu_summary %>%
  select(SCHL_label, low_share, mid_share, high_share) %>%
  pivot_longer(
    cols = c(low_share, mid_share, high_share),
    names_to = "band",
    values_to = "share"
  ) %>%
  mutate(
    band = recode(
      band,
      low_share = "Low",
      mid_share = "Middle",
      high_share = "High"
    ),
    band = factor(band, levels = c("Low", "Middle", "High"))
  )

ggplot(edu_long, aes(x = SCHL_label, y = share, fill = band)) +
  geom_col() +
  geom_text(
    aes(label = round(share * 100, 1)),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Income band shares by education level",
    x = "Education level",
    y = "Share of households",
    fill = "Income band"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/income_bands_by_education_stacked.png")

ggplot(edu_summary, aes(x = SCHL_label, y = median_income)) +
  geom_col() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by education level",
    x = "Education level",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/median_income_by_education.png")

# Employment summary
esr_summary <- acs_clean %>%
  filter(!is.na(ESR_label)) %>%
  group_by(ESR_label) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(ESR_label != "Other / unknown") %>%
  arrange(ESR_label)

write_csv(esr_summary, "outputs/metrics/other/income_by_employment_low_mid_high.csv")

esr_long <- esr_summary %>%
  select(ESR_label, low_share, mid_share, high_share) %>%
  pivot_longer(
    cols = c(low_share, mid_share, high_share),
    names_to = "band",
    values_to = "share"
  ) %>%
  mutate(
    band = recode(
      band,
      low_share = "Low",
      mid_share = "Middle",
      high_share = "High"
    ),
    band = factor(band, levels = c("Low", "Middle", "High"))
  )

ggplot(esr_long, aes(x = ESR_label, y = share, fill = band)) +
  geom_col() +
  geom_text(
    aes(label = round(share * 100, 1)),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Income band shares by employment status",
    x = "Employment status",
    y = "Share of households",
    fill = "Income band"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/income_bands_by_employment_stacked.png")

ggplot(esr_summary, aes(x = ESR_label, y = median_income)) +
  geom_col() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by employment status",
    x = "Employment status",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/median_income_by_employment.png")

# Tenure summary
ten_summary <- acs_clean %>%
  group_by(TEN_label) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(TEN_label != "Other / unknown") %>%
  arrange(TEN_label)

write_csv(ten_summary, "outputs/metrics/other/income_by_tenure_low_mid_high.csv")

ten_long <- ten_summary %>%
  select(TEN_label, low_share, mid_share, high_share) %>%
  pivot_longer(
    cols = c(low_share, mid_share, high_share),
    names_to = "band",
    values_to = "share"
  ) %>%
  mutate(
    band = recode(
      band,
      low_share = "Low",
      mid_share = "Middle",
      high_share = "High"
    ),
    band = factor(band, levels = c("Low", "Middle", "High"))
  )

ggplot(ten_long, aes(x = TEN_label, y = share, fill = band)) +
  geom_col() +
  geom_text(
    aes(label = round(share * 100, 1)),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Income band shares by housing tenure",
    x = "Housing tenure",
    y = "Share of households",
    fill = "Income band"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/income_bands_by_tenure_stacked.png")

ggplot(ten_summary, aes(x = TEN_label, y = median_income)) +
  geom_col() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by housing tenure",
    x = "Housing tenure",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/median_income_by_tenure.png")

# Disability summary
dis_summary <- acs_clean %>%
  group_by(DIS_label) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(DIS_label != "Other / unknown") %>%
  arrange(DIS_label)

write_csv(dis_summary, "outputs/metrics/other/income_by_disability_low_mid_high.csv")

dis_long <- dis_summary %>%
  select(DIS_label, low_share, mid_share, high_share) %>%
  pivot_longer(
    cols = c(low_share, mid_share, high_share),
    names_to = "band",
    values_to = "share"
  ) %>%
  mutate(
    band = recode(
      band,
      low_share = "Low",
      mid_share = "Middle",
      high_share = "High"
    ),
    band = factor(band, levels = c("Low", "Middle", "High"))
  )

ggplot(dis_long, aes(x = DIS_label, y = share, fill = band)) +
  geom_col() +
  geom_text(
    aes(label = round(share * 100, 1)),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Income band shares by disability status",
    x = "Disability status",
    y = "Share of households",
    fill = "Income band"
  )

ggsave("outputs/figures/other/income_bands_by_disability_stacked.png")

ggplot(dis_summary, aes(x = DIS_label, y = median_income)) +
  geom_col() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by disability status",
    x = "Disability status",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma)

ggsave("outputs/figures/other/median_income_by_disability.png")

# Age vs log income scatter and smooth
ggplot(acs_clean, aes(x = AGEP, y = HINCP_log)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Log household income vs age",
    x = "Age",
    y = "log(HINCP)"
  )

ggsave("outputs/figures/other/age_vs_log_income_scatter_smooth.png")

# Age bands
acs_clean <- acs_clean %>%
  mutate(
    age_band = case_when(
      AGEP < 30 ~ "Under 30",
      AGEP < 45 ~ "30–44",
      AGEP < 65 ~ "45–64",
      TRUE ~ "65+"
    )
  )

age_summary <- acs_clean %>%
  group_by(age_band) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(age_band)

write_csv(age_summary, "outputs/metrics/other/income_by_age_band_low_mid_high.csv")

ggplot(age_summary, aes(x = age_band, y = median_income, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by age band",
    x = "Age band",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma)

ggsave("outputs/figures/other/median_income_by_age_band_line.png")

# Household size bands
acs_clean <- acs_clean %>%
  mutate(
    household_size_band = case_when(
      NP <= 1 ~ "1",
      NP == 2 ~ "2",
      NP == 3 ~ "3",
      NP == 4 ~ "4",
      NP == 5 ~ "5",
      NP >= 6 ~ "6+",
      TRUE ~ as.character(NP)
    )
  )

np_summary <- acs_clean %>%
  group_by(household_size_band) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(household_size_band)

write_csv(np_summary, "outputs/metrics/other/income_by_household_size_low_mid_high.csv")

ggplot(np_summary, aes(x = household_size_band, y = median_income)) +
  geom_col() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by household size band",
    x = "Household size band",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma)

ggsave("outputs/figures/other/median_income_by_household_size.png")

# Vehicles summary
veh_summary <- acs_clean %>%
  group_by(VEH) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    low_share = mean(low_income, na.rm = TRUE),
    mid_share = mean(mid_income, na.rm = TRUE),
    high_share = mean(high_income, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(VEH)

write_csv(veh_summary, "outputs/metrics/other/income_by_vehicles_low_mid_high.csv")

ggplot(veh_summary, aes(x = as.factor(VEH), y = median_income)) +
  geom_col() +
  geom_text(aes(label = round(median_income, 0)), vjust = -0.3) +
  labs(
    title = "Median household income by number of vehicles",
    x = "Number of vehicles",
    y = "Median household income"
  ) +
  scale_y_continuous(labels = comma)

ggsave("outputs/figures/other/median_income_by_vehicles.png")

# Boxplot of income by education (trimmed)
income_trim <- acs_clean %>%
  filter(HINCP <= 300000)

ggplot(income_trim, aes(x = SCHL_label, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household income by education level (trimmed at $300k)",
    x = "Education level",
    y = "Household income"
  ) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/other/income_boxplot_by_education_trimmed.png")

# Income deciles summary
acs_clean <- acs_clean %>%
  mutate(
    income_decile = ntile(HINCP, 10)
  )

decile_summary <- acs_clean %>%
  group_by(income_decile) %>%
  summarise(
    n = n(),
    mean_income = mean(HINCP, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    mean_age = mean(AGEP, na.rm = TRUE),
    mean_np = mean(NP, na.rm = TRUE),
    mean_veh = mean(VEH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(income_decile)

write_csv(decile_summary, "outputs/metrics/other/income_decile_summary.csv")

ggplot(decile_summary, aes(x = income_decile, y = mean_income)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(mean_income, 0)), vjust = -0.3) +
  labs(
    title = "Mean household income by income decile",
    x = "Income decile (1 = lowest, 10 = highest)",
    y = "Mean household income"
  ) +
  scale_y_continuous(labels = comma)

ggsave("outputs/figures/other/mean_income_by_decile.png")

# Density of log income by income band
ggplot(acs_clean, aes(x = HINCP_log, color = income_band)) +
  geom_density() +
  labs(
    title = "Density of log household income by income band",
    x = "log(HINCP)",
    y = "Density",
    color = "Income band"
  )

ggsave("outputs/figures/other/log_income_density_by_band.png")

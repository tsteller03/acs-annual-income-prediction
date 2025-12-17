library("tidycensus")
library("tidyverse")

acs_data <- read.csv("data/acs_pums_2022_sample.csv")

# Clean & filter data
acs_clean <- acs_data %>%
  mutate(
    HINCP = as.numeric(HINCP),
    AGEP = as.numeric(AGEP),
    NP = as.numeric(NP),
    VEH = as.numeric(VEH)
  )

# Remove observations with missing or invalid income
acs_clean <- acs_clean %>%
  filter(!is.na(HINCP),
         HINCP >= 0)

# Log-income variable (for later reference) for strictly positive income
acs_clean <- acs_clean %>%
  mutate(HINCP_log = if_else(HINCP > 0, log(HINCP), NA_real_))

# Run overall summary statistics
summary(acs_clean$HINCP)
summary(acs_clean$HINCP_log)

numeric_summary <- acs_clean %>%
  select(HINCP, HINCP_log, AGEP, NP, VEH) %>%
  summary()

numeric_summary

capture.output(numeric_summary, file = "outputs/metrics/eda/numeric_summary.txt") # Save output

# Distribution of income data exploration

# Raw income 
acs_clean %>%
  ggplot(aes(x = HINCP)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of Household Income",
    x = "Household income (HINCP)",
    y = "Count"
  )

ggsave("outputs/figures/eda/hincp_hist_trimmed.png")

# Log income (positive HINCP only)
acs_clean %>%
  filter(!is.na(HINCP_log)) %>%
  ggplot(aes(x = HINCP_log)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of Household Income (log)",
    x = "log(HINCP)",
    y = "Count"
  )

ggsave("outputs/figures/eda/hincp_log_hist.png")

# Boxplot of income
acs_clean %>%
  ggplot(aes(y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Household Income",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_boxplot_trimmed.png")

# Categorical variable coding
acs_clean <- acs_clean %>%
  mutate(
    SEX = as.factor(SEX),
    RAC1P = as.factor(RAC1P),
    MAR = as.factor(MAR),
    SCHL = as.factor(SCHL),
    ESR = as.factor(ESR),
    TEN = as.factor(TEN),
    DIS = as.factor(DIS)
  )

cat_counts <- list(
  SEX = acs_clean %>% count(SEX),
  RAC1P = acs_clean %>% count(RAC1P),
  MAR = acs_clean %>% count(MAR),
  SCHL = acs_clean %>% count(SCHL),
  ESR = acs_clean %>% count(ESR),
  TEN = acs_clean %>% count(TEN),
  DIS = acs_clean %>% count(DIS)
)

cat_counts

# Income versus key categorical predictors

# Education (SCHL)
income_by_education <- acs_clean %>%
  group_by(SCHL) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(SCHL)

income_by_education
write_csv(income_by_education, "outputs/metrics/eda/income_by_education.csv")

acs_clean %>%
  ggplot(aes(x = SCHL, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Education Level (SCHL)",
    x = "Education level (SCHL)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_education_boxplot.png")

# Employment status (ESR)
income_by_employment <- acs_clean %>%
  group_by(ESR) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(ESR)

income_by_employment
write_csv(income_by_employment, "outputs/metrics/eda/income_by_employment.csv")

acs_clean %>%
  ggplot(aes(x = ESR, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Employment Status (ESR)",
    x = "Employment status (ESR)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_employment_boxplot.png")

# Housing tenure (TEN)
income_by_tenure <- acs_clean %>%
  group_by(TEN) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(TEN)

income_by_tenure
write_csv(income_by_tenure, "outputs/metrics/eda/income_by_tenure.csv")

acs_clean %>%
  ggplot(aes(x = TEN, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Housing Tenure (TEN)",
    x = "Tenure (TEN)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_tenure_boxplot.png")

# Disability status (DIS)
income_by_disability <- acs_clean %>%
  group_by(DIS) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(DIS)

income_by_disability
write_csv(income_by_disability, "outputs/metrics/eda/income_by_disability.csv")

acs_clean %>%
  ggplot(aes(x = DIS, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Disability Status (DIS)",
    x = "Disability status (DIS)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_disability_boxplot.png")

# Sex (SEX)
income_by_sex <- acs_clean %>%
  group_by(SEX) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(SEX)

income_by_sex
write_csv(income_by_sex, "outputs/metrics/eda/income_by_sex.csv")

acs_clean %>%
  ggplot(aes(x = SEX, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Sex",
    x = "Sex",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_sex_boxplot.png")

# Race (RAC1P)
income_by_race <- acs_clean %>%
  group_by(RAC1P) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(RAC1P)

income_by_race
write_csv(income_by_race, "outputs/metrics/eda/income_by_race.csv")

acs_clean %>%
  ggplot(aes(x = RAC1P, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Race (RAC1P)",
    x = "Race (RAC1P)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_race_boxplot.png")

# Marital status (MAR)
income_by_marital <- acs_clean %>%
  group_by(MAR) %>%
  summarise(
    n = n(),
    mean_val = mean(HINCP, na.rm = TRUE),
    med_val = median(HINCP, na.rm = TRUE),
    q1_val = quantile(HINCP, 0.25, na.rm = TRUE),
    q3_val = quantile(HINCP, 0.75, na.rm = TRUE)
  ) %>%
  arrange(MAR)

income_by_marital
write_csv(income_by_marital, "outputs/metrics/eda/income_by_marital.csv")

acs_clean %>%
  ggplot(aes(x = MAR, y = HINCP)) +
  geom_boxplot() +
  labs(
    title = "Household Income by Marital Status (MAR)",
    x = "Marital status (MAR)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_by_marital_boxplot.png")

# Numeric relationships

# Age
acs_clean %>%
  ggplot(aes(x = AGEP, y = HINCP)) +
  geom_point() +
  labs(
    title = "Household Income vs Age (AGEP)",
    x = "Age (AGEP)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_vs_age.png")

# Household size
acs_clean %>%
  ggplot(aes(x = NP, y = HINCP)) +
  geom_point() +
  labs(
    title = "Household Income vs Household Size (NP)",
    x = "Number of people in household (NP)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_vs_np.png")

# Number of vehicles
acs_clean %>%
  ggplot(aes(x = VEH, y = HINCP)) +
  geom_point() +
  labs(
    title = "Household Income vs Number of Vehicles (VEH)",
    x = "Number of vehicles (VEH)",
    y = "Household income (HINCP)"
  )

ggsave("outputs/figures/eda/hincp_vs_veh.png")

# Correlation among numeric variables
numeric_vars <- acs_clean %>%
  select(HINCP, AGEP, NP, VEH) %>%
  drop_na()

cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
cor_matrix

capture.output(cor_matrix, file = "outputs/metrics/eda/correlation_matrix_numeric.txt")

# Occupation and industry counts
occ_counts <- acs_clean %>%
  count(OCCP) %>%
  arrange(desc(n))

ind_counts <- acs_clean %>%
  count(INDP) %>%
  arrange(desc(n))

write_csv(occ_counts, "outputs/metrics/eda/occupation_counts.csv")
write_csv(ind_counts, "outputs/metrics/eda/industry_counts.csv")

top_occupations <- head(occ_counts, 20)
top_industries <- head(ind_counts, 20)

top_occupations
top_industries

# Save dataset to R's internal structure for easier loading
saveRDS(acs_clean, "data/acs_eda_clean.rds")

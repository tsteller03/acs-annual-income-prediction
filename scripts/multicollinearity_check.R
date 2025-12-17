library("tidyverse")
library("car")

acs_train <- readRDS("data/acs_train.rds")

# Build dataset for checks
multicol_data <- acs_train %>%
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

# Treat categorical variables as factors
multicol_data <- multicol_data %>%
  mutate(
    SCHL = as.factor(SCHL),
    ESR = as.factor(ESR),
    TEN = as.factor(TEN),
    MAR = as.factor(MAR),
    DIS = as.factor(DIS),
    SEX = as.factor(SEX),
    RAC1P = as.factor(RAC1P)
  )

# Find correlation among numeric predictors
numeric_predictors <- multicol_data %>%
  select(AGEP, NP, VEH)

cor_numeric <- cor(numeric_predictors)
cor_numeric

capture.output(cor_numeric, file = "outputs/metrics/multicollinearity_check/correlation_numeric_predictors.txt")

# Linear model for Variance Inflation Factor
lm_full <- lm(HINCP_log ~ ., data = multicol_data)
summary(lm_full)

# VIF values
vif_values <- vif(lm_full)
vif_values

# Convert what vif() returns into a data frame
vif_df <- as.data.frame(vif_values)

# Add variable names as a proper column
vif_df$variable <- rownames(vif_values)
rownames(vif_df) <- NULL

# Save VIF table
write_csv(vif_df, "outputs/metrics/multicollinearity_check/vif_full_model.csv")

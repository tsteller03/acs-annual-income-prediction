library(tidyverse)
library(tidymodels)

acs_clean <- readRDS("data/acs_eda_clean.rds")

# Drop N/A income rows
acs_clean <- acs_clean %>%
  filter(!is.na(HINCP_log))

# Data split - 70% train/validate, 30% test
set.seed(607)
split_1 <- initial_split(acs_clean, prop = 0.7)
train_val <- training(split_1)
test_data <- testing(split_1)

# Second split - 80% of train/validate data into training, 20% into validation
set.seed(607)
split_2 <- initial_split(train_val, prop = 0.8)
train_data <- training(split_2)
val_data   <- testing(split_2)

# Save data
saveRDS(train_data, "data/acs_train.rds")
saveRDS(val_data,   "data/acs_val.rds")
saveRDS(test_data,  "data/acs_test.rds")

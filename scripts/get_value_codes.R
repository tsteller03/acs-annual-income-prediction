library(tidycensus)
library(tidyverse)

data("pums_variables")

pums_2022_acs5 <- pums_variables %>%
  filter(survey == "acs5", year == "2022")

vars_of_interest <- c("DIS", "TEN", "ESR", "SCHL")

pums_2022_acs5 %>%
  filter(var_code %in% vars_of_interest) %>%
  select(var_code, val_min, val_max, val_label) %>%
  distinct() %>%
  arrange(var_code, val_min) %>%
  write_csv("outputs/metrics/other/pums_value_codes.csv")

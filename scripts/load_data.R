install.packages("tidycensus")
library("tidycensus")
library("tidyverse")
  
pums_vars <- c(
    # Outcome
    "HINCP",
    
    # Demographics
    "AGEP", # Age
    "SEX", # Sex
    "RAC1P", # Race
    "MAR", # Marital status
    
    # Education
    "SCHL", # Education level
    
    # Employment
    "ESR", # Employment status
    "OCCP", # Occupation
    "INDP", # Industry
    
    # Household structure
    "NP", # Number of people in household
    
    # Housing structure
    "TEN", # Own or rent
    "VEH", # Vehicles
    
    # Disability
    "DIS" # Disability status
)
  
acs <- get_pums(
    variables = pums_vars,
    survey = "acs5",
    year = 2022,
    state = "all",
    rep_weights = NULL
)
  
write_csv(acs, "acs_pums_2022.csv")

acs <- read_csv("acs_pums_2022.csv")
set.seed(607)

# Collect a random sample (0.10% of data) for testing purposes, as original file is 15.7M rows long.
acs_sample <- acs %>% 
  sample_frac(0.001)

write_csv(acs_sample, "acs_pums_2022_sample.csv")

getwd()

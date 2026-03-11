# ---------------------------------------------------------
# Script: 01_import_data.R
# Purpose: Import raw CDC mortality data for initial inspection
# ---------------------------------------------------------

library(readr)
library(dplyr)

# Import raw mortality dataset
mort_raw <- read_csv("data/raw/cdc_mortality_1999_2020_raw.csv")

# Check structure
glimpse(mort_raw)

# Preview first rows
head(mort_raw)

# Check column names
names(mort_raw)

# Check missing values
summary(mort_raw)

mort_raw %>%
  select(Year, `Single-Year Ages`, `Single-Year Ages Code`, Deaths, Population) %>%
  summary()

mort_raw %>%
  group_by(Year) %>%
  summarise(total_deaths = sum(Deaths, na.rm = TRUE))
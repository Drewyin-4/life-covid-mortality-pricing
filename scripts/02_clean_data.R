# ---------------------------------------------------------
# Script: 02_clean_data.R
# Purpose: Clean CDC mortality data for actuarial analysis
# ---------------------------------------------------------

library(readr)
library(dplyr)

# Import raw mortality dataset
mort_raw <- read_csv("data/raw/cdc_mortality_1999_2020_raw.csv")

# Clean data
mort_clean <- mort_raw %>%
  rename(
    age_label = `Single-Year Ages`,
    age = `Single-Year Ages Code`,
    sex = Sex,
    deaths = Deaths,
    population = Population,
    crude_rate = `Crude Rate`,
    notes = Notes
  ) %>%
  filter(is.na(notes)) %>%
  mutate(
    age = as.numeric(age),
    population = as.numeric(population),
    crude_rate = as.numeric(crude_rate)
  ) %>%
  filter(!is.na(Year)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(deaths)) %>%
  filter(!is.na(population))

# Inspect cleaned data
glimpse(mort_clean)
head(mort_clean)
summary(mort_clean)

# Missing value check
colSums(is.na(mort_clean))

mort_clean <- mort_clean %>%
  mutate(qx = deaths / population)

summary(mort_clean$qx)


write_csv(mort_clean, "data/processed/mortality_clean.csv")
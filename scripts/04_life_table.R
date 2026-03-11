# ---------------------------------------------------------
# Script: 04_life_table.R
# Purpose: Construct life table from mortality rates
# ---------------------------------------------------------

library(readr)
library(dplyr)

# Load cleaned data
mort_clean <- read_csv("data/processed/mortality_clean.csv")

# Select Female mortality in 2019
mort_2019 <- mort_clean %>%
  filter(Year == 2019, sex == "Female") %>%
  arrange(age)

# Keep needed columns
life_data <- mort_2019 %>%
  select(age, qx)

# find px

life_data <- life_data %>%
  mutate(
    px = 1 - qx
  )

# find lx

life_data$lx <- NA
life_data$lx[1] <- 100000

for(i in 2:nrow(life_data)){
  life_data$lx[i] <- life_data$lx[i-1] * life_data$px[i-1]
}


# find dx

life_data <- life_data %>%
  mutate(
    dx = lx * qx
  )

head(life_data)

write_csv(life_data, "data/processed/life_table_2019_female.csv")

# some possible extra work 


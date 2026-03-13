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

write_csv(life_data, "data/processed/life_table_2019_female_basic.csv")

# some possible extra work 

# Construct l(x+1) for Lx calculation
life_data <- life_data %>%
  mutate(
    lx_next = lead(lx)
  )

# Construct Lx
# For the last age, use a simple approximation: Lx = lx / 2
life_data <- life_data %>%
  mutate(
    Lx = if_else(is.na(lx_next), lx / 2, (lx + lx_next) / 2)
  )

# Construct Tx from bottom to top
life_data$Tx <- rev(cumsum(rev(life_data$Lx)))

# Construct ex
life_data <- life_data %>%
  mutate(
    ex = Tx / lx
  )

# Keep final columns
life_table <- life_data %>%
  select(age, qx, px, lx, dx, Lx, Tx, ex)

# Inspect results
glimpse(life_table)
head(life_table)
summary(life_table)

write_csv(life_table, "data/processed/life_table_2019_female_with_LxTxex.csv")


build_life_table <- function(data, year_value, sex_value){
  
  life_data <- data %>%
    filter(Year == year_value, sex == sex_value) %>%
    arrange(age) %>%
    select(age, qx) %>%
    mutate(px = 1 - qx)
  
  life_data$lx <- NA
  life_data$lx[1] <- 100000
  
  for(i in 2:nrow(life_data)){
    life_data$lx[i] <- life_data$lx[i-1] * life_data$px[i-1]
  }
  
  life_data <- life_data %>%
    mutate(
      dx = lx * qx,
      lx_next = lead(lx),
      Lx = if_else(is.na(lx_next), lx/2, (lx + lx_next)/2)
    )
  
  life_data$Tx <- rev(cumsum(rev(life_data$Lx)))
  
  life_data <- life_data %>%
    mutate(ex = Tx / lx)
  
  return(life_data)
}


life_2019_f <- build_life_table(mort_clean, 2019, "Female")
life_2020_f <- build_life_table(mort_clean, 2020, "Female")

life_2019_m <- build_life_table(mort_clean, 2019, "Male")
life_2020_m <- build_life_table(mort_clean, 2020, "Male")


write_csv(life_2019_f, "data/processed/life_table_2019_female.csv")
write_csv(life_2020_f, "data/processed/life_table_2020_female.csv")

write_csv(life_2019_m, "data/processed/life_table_2019_male.csv")
write_csv(life_2020_m, "data/processed/life_table_2020_male.csv")

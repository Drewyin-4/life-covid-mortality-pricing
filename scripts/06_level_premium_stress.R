# This script extends Script 05.
# Script 05 calculates the net single premium (NSP), which is equal to
# the expected present value of death benefits.
# Script 06 uses the same mortality assumptions and contract settings,
# and converts the single premium framework into a level annual premium
# framework under the equivalence principle.

# ---------------------------------------------------------
# Script: 06_level_premium_stress.R
# Purpose: Price level annual premium for term life under
#          baseline, full stress, and temporary stress scenarios
# ---------------------------------------------------------

library(readr)
library(dplyr)
library(tibble)
library(ggplot2)

# Load life tables
life_2019_f <- read_csv("data/processed/life_table_2019_female.csv")
life_2020_f <- read_csv("data/processed/life_table_2020_female.csv")

life_2019_m <- read_csv("data/processed/life_table_2019_male.csv")
life_2020_m <- read_csv("data/processed/life_table_2020_male.csv")


# Set pricing assumptions
issue_age <- 40
term <- 20
benefit <- 100000
i <- 0.03

# Set how many years the temporary stress lasts
stress_years <- 2


# This function calculates level annual premium from one life table.
price_level_term <- function(life_table, issue_age, term, benefit, i) {
  
  v <- 1 / (1 + i)
  
  pricing_data <- life_table %>%
    filter(age >= issue_age, age < issue_age + term) %>%
    arrange(age)
  
  lx0 <- life_table %>%
    filter(age == issue_age) %>%
    pull(lx)
  
  # Check whether the issue age exists in the table
  if(length(lx0) == 0){
    stop("Issue age not found in life table.")
  }
  
  pricing_data <- pricing_data %>%
    mutate(
      t = age - issue_age,
      survival_prob = lx / lx0,
      death_prob = survival_prob * qx,
      pv_benefit = benefit * v^(t + 1) * death_prob,
      pv_premium = v^t * survival_prob
    )
  
  pv_benefit <- sum(pricing_data$pv_benefit, na.rm = TRUE)
  pv_premium_factor <- sum(pricing_data$pv_premium, na.rm = TRUE)
  
  level_premium <- pv_benefit / pv_premium_factor
  
  return(level_premium)
}


# This function builds a temporary stress scenario:
price_level_term_temp_stress <- function(
    baseline_table,
    stress_table,
    issue_age,
    term,
    benefit,
    i,
    stress_years = 2
) {
  
  v <- 1 / (1 + i)
  
  baseline_qx <- baseline_table %>%
    filter(age >= issue_age, age < issue_age + term) %>%
    arrange(age) %>%
    select(age, qx_baseline = qx)
  
  stress_qx <- stress_table %>%
    filter(age >= issue_age, age < issue_age + term) %>%
    arrange(age) %>%
    select(age, qx_stress = qx)
  
  pricing_data <- baseline_qx %>%
    left_join(stress_qx, by = "age") %>%
    mutate(
      t = age - issue_age,
      qx_used = if_else(t < stress_years, qx_stress, qx_baseline),
      px_used = 1 - qx_used
    )
  
  # Check for missing qx values
  if(any(is.na(pricing_data$qx_used))){
    stop("Missing qx values found in temporary stress scenario.")
  }
  
  # Build survival probabilities recursively
  pricing_data$survival_prob <- NA_real_
  pricing_data$survival_prob[1] <- 1
  
  if(nrow(pricing_data) > 1){
    for(k in 2:nrow(pricing_data)){
      pricing_data$survival_prob[k] <-
        pricing_data$survival_prob[k - 1] * pricing_data$px_used[k - 1]
    }
  }
  
  pricing_data <- pricing_data %>%
    mutate(
      death_prob = survival_prob * qx_used,
      pv_benefit = benefit * v^(t + 1) * death_prob,
      pv_premium = v^t * survival_prob
    )
  
  pv_benefit <- sum(pricing_data$pv_benefit, na.rm = TRUE)
  pv_premium_factor <- sum(pricing_data$pv_premium, na.rm = TRUE)
  
  level_premium <- pv_benefit / pv_premium_factor
  
  return(level_premium)
}


# Calculate baseline level premiums
lp_2019_f <- price_level_term(
  life_table = life_2019_f,
  issue_age = issue_age,
  term = term,
  benefit = benefit,
  i = i
)

lp_2019_m <- price_level_term(
  life_table = life_2019_m,
  issue_age = issue_age,
  term = term,
  benefit = benefit,
  i = i
)


# Calculate full stress level premiums
lp_2020_f <- price_level_term(
  life_table = life_2020_f,
  issue_age = issue_age,
  term = term,
  benefit = benefit,
  i = i
)

lp_2020_m <- price_level_term(
  life_table = life_2020_m,
  issue_age = issue_age,
  term = term,
  benefit = benefit,
  i = i
)


# Calculate temporary stress level premiums-- selecting 1 year mixed.
# note: this is just a temporarly approach, stress is fading, not disappearing instantly. May consider expo/linear/other mix later.
lp_temp_f <- price_level_term_temp_stress(
  baseline_table = life_2019_f,
  stress_table = life_2020_f,
  issue_age = issue_age,
  term = term,
  benefit = benefit,
  i = i,
  stress_years = stress_years
)

lp_temp_m <- price_level_term_temp_stress(
  baseline_table = life_2019_m,
  stress_table = life_2020_m,
  issue_age = issue_age,
  term = term,
  benefit = benefit,
  i = i,
  stress_years = stress_years
)


# Build final comparison table
level_premium_summary <- tibble(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  scenario = c(
    "2019_baseline",
    "2020_full_stress",
    paste0("temporary_stress_", stress_years, "yr"),
    "2019_baseline",
    "2020_full_stress",
    paste0("temporary_stress_", stress_years, "yr")
  ),
  level_premium = c(
    lp_2019_f,
    lp_2020_f,
    lp_temp_f,
    lp_2019_m,
    lp_2020_m,
    lp_temp_m
  )
)


# Add percentage changes relative to baseline within each sex
level_premium_summary <- level_premium_summary %>%
  group_by(sex) %>%
  mutate(
    baseline_premium = level_premium[scenario == "2019_baseline"][1],
    pct_change_vs_baseline = level_premium / baseline_premium - 1
  ) %>%
  ungroup()


# Print results
print(level_premium_summary)


# Save results
write_csv(
  level_premium_summary,
  "data/processed/level_premium_stress_comparison.csv"
)


ggplot(level_premium_summary,
       aes(x = scenario, y = level_premium, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Level Premium by Scenario",
    x = "Scenario",
    y = "Level Premium"
  )

ggplot(level_premium_summary,
       aes(x = scenario, y = pct_change_vs_baseline, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage Change in Premium vs Baseline",
    y = "Percent Change"
  )

baseline_f <- life_2019_f %>%
  filter(age >= issue_age, age < issue_age + term) %>%
  arrange(age)

stress_f <- life_2020_f %>%
  filter(age >= issue_age, age < issue_age + term) %>%
  arrange(age)

df_plot <- tibble(
  t = 0:(term - 1),
  baseline = baseline_f$qx,
  full_stress = stress_f$qx,
  temporary_stress = ifelse(0:(term - 1) < stress_years, stress_f$qx, baseline_f$qx)
)

ggplot(df_plot, aes(x = t)) +
  geom_line(aes(y = baseline, color = "baseline"), linewidth = 1) +
  geom_line(aes(y = full_stress, color = "stress"), linewidth = 1) +
  geom_line(aes(y = temporary_stress, color = "temporary"), linewidth = 1) +
  labs(
    title = "Mortality Path by Scenario",
    x = "Policy Year",
    y = "Mortality Rate",
    color = "Scenario"
  )
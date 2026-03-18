# ---------------------------------------------------------
# Script: 05_term_life_pricing.R
# Purpose: Term life pricing under baseline and stress mortality
# ---------------------------------------------------------

library(readr)
library(dplyr)

# Load life tables
life_2019_f <- read_csv("data/processed/life_table_2019_female.csv")
life_2020_f <- read_csv("data/processed/life_table_2020_female.csv")

life_2019_m <- read_csv("data/processed/life_table_2019_male.csv")
life_2020_m <- read_csv("data/processed/life_table_2020_male.csv")

# Pricing assumptions
issue_age <- 40
term <- 20
benefit <- 100000
i <- 0.03

# Pricing function
price_term_life <- function(life_table, issue_age, term, benefit, i) {
  
  v <- 1 / (1 + i)
  
  pricing_data <- life_table %>%
    filter(age >= issue_age, age < issue_age + term) %>%
    arrange(age)
  
  lx0 <- life_table %>%
    filter(age == issue_age) %>%
    pull(lx)
  
  pricing_data <- pricing_data %>%
    mutate(
      t = age - issue_age,
      survival_prob = lx / lx0,
      death_prob = survival_prob * qx,
      discount = v^(t + 1),
      pv_benefit = benefit * discount * death_prob
    )
  
  sum(pricing_data$pv_benefit)
}

# Calculate premiums
nsp_2019_f <- price_term_life(life_2019_f, issue_age, term, benefit, i)
nsp_2020_f <- price_term_life(life_2020_f, issue_age, term, benefit, i)

nsp_2019_m <- price_term_life(life_2019_m, issue_age, term, benefit, i)
nsp_2020_m <- price_term_life(life_2020_m, issue_age, term, benefit, i)

# Combine results
pricing_summary <- tibble(
  scenario = c(
    "Female_2019_baseline",
    "Female_2020_stress",
    "Male_2019_baseline",
    "Male_2020_stress"
  ),
  NSP = c(
    nsp_2019_f,
    nsp_2020_f,
    nsp_2019_m,
    nsp_2020_m
  )
)

# Calculate percentage change
pricing_summary <- pricing_summary %>%
  mutate(
    pct_change_vs_baseline =
      case_when(
        scenario == "Female_2020_stress" ~ NSP / nsp_2019_f - 1,
        scenario == "Male_2020_stress" ~ NSP / nsp_2019_m - 1,
        TRUE ~ 0
      )
  )

print(pricing_summary)

# Save result
write_csv(pricing_summary,
          "data/processed/term_life_pricing_comparison.csv")

# aviod error
if(!dir.exists("outputs")){
  dir.create("outputs")
}

p_nsp <- ggplot(pricing_summary,
       aes(x = scenario, y = NSP)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Net Single Premium Comparison",
    x = "Scenario",
    y = "NSP"
  )

ggsave(
  "outputs/nsp_comparison.png",
  plot = p_nsp,
  width = 8,
  height = 5,
  dpi = 300
)


p_nsp_pct <- ggplot(pricing_summary,
       aes(x = scenario, y = pct_change_vs_baseline)) +
  geom_col(fill = "orange") +
  labs(
    title = "NSP Increase Due to Stress",
    y = "Percentage Change"
  )

ggsave(
  "outputs/nsp_pct_change.png",
  plot = p_nsp_pct,
  width = 8,
  height = 5,
  dpi = 300
)

# ---------------------------------------------------------
# Script: 07_prt_group_annuity.R
# Purpose: Extend life pricing project toward PRT context.
#







library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)




life_2019_f <- read_csv("data/processed/life_table_2019_female.csv")
life_2019_m <- read_csv("data/processed/life_table_2019_male.csv")
life_2020_f <- read_csv("data/processed/life_table_2020_female.csv")
life_2020_m <- read_csv("data/processed/life_table_2020_male.csv")

#   Section 1: Whole life annuity pricing (single life)

price_whole_life_annuity <- function(life_table, issue_age, annual_benefit, i) {
  
  v <- 1 / (1 + i)
  
  annuity_data <- life_table %>%
    filter(age >= issue_age) %>%
    arrange(age)
  
  lx0 <- annuity_data$lx[1]
  
  annuity_data <- annuity_data %>%
    mutate(
      t             = age - issue_age,
      survival_prob = lx / lx0,
      pv_payment    = annual_benefit * (v ^ t) * survival_prob
    )
  
  sum(annuity_data$pv_payment, na.rm = TRUE)
}


nsp_single_f65 <- price_whole_life_annuity(life_2019_f, 65, 12000, 0.03)
nsp_single_m65 <- price_whole_life_annuity(life_2019_m, 65, 12000, 0.03)

cat("Single life annuity NSP (Female 65, $12k/yr, i=3%):", round(nsp_single_f65, 2), "\n")
cat("Single life annuity NSP (Male   65, $12k/yr, i=3%):", round(nsp_single_m65, 2), "\n")
cat("Note: male NSP is LOWER because higher mortality -> fewer expected payments\n")
cat("      This is the OPPOSITE direction vs term life pricing.\n\n")


#   Section 2: Group census pricing

set.seed(42)

census <- tibble(
  participant_id  = 1:15,
  age             = c(58, 60, 61, 63, 64, 65, 65, 67, 68, 69, 70, 70, 71, 72, 72),
  sex             = c("F","M","F","M","F","F","M","M","F","M","F","M","F","M","F"),
  monthly_benefit = c(1800, 2500, 1500, 3200, 2100, 1700, 2800, 3500,
                      1600, 2200, 1900, 3000, 2400, 2700, 1400)
) %>%
  mutate(annual_benefit = monthly_benefit * 12)

print(census)


price_census <- function(census, life_table_f, life_table_m, i) {
  census %>%
    rowwise() %>%
    mutate(
      nsp = {
        lt <- if (sex == "F") life_table_f else life_table_m
        price_whole_life_annuity(lt, age, annual_benefit, i)
      }
    ) %>%
    ungroup()
}


census_priced_baseline <- price_census(census, life_2019_f, life_2019_m, 0.03)
census_priced_stress   <- price_census(census, life_2020_f, life_2020_m, 0.03)

total_liability_baseline <- sum(census_priced_baseline$nsp)
total_liability_stress   <- sum(census_priced_stress$nsp)

cat("--- Group census pricing ---\n")
cat("Total liability, 2019 baseline (i=3%): $",
    format(round(total_liability_baseline), big.mark = ","), "\n")
cat("Total liability, 2020 stress   (i=3%): $",
    format(round(total_liability_stress), big.mark = ","), "\n")
cat("Change:", round((total_liability_stress / total_liability_baseline - 1) * 100, 2), "%\n")
cat("Note: liability DECREASES under higher mortality for annuities\n\n")

census_comparison <- bind_rows(
  census_priced_baseline %>% mutate(mortality_scenario = "2019_baseline"),
  census_priced_stress   %>% mutate(mortality_scenario = "2020_stress")
) %>%
  select(participant_id, age, sex, annual_benefit, mortality_scenario, nsp)

write_csv(census_comparison, "data/processed/prt_census_pricing.csv")

if (!dir.exists("outputs")) dir.create("outputs")

p_census <- ggplot(census_comparison,
                   aes(x = factor(participant_id), y = nsp,
                       fill = mortality_scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title    = "Individual annuity NSP: baseline vs COVID stress",
    subtitle = "Higher mortality reduces annuity liability (opposite of term life)",
    x        = "Participant ID", y = "NSP ($)", fill = "Mortality scenario"
  ) +
  theme_minimal()

ggsave("outputs/prt_census_nsp.png", plot = p_census, width = 10, height = 5, dpi = 300)


#   Section 3: Interest rate sensitivity

rates <- c(0.02, 0.03, 0.04, 0.05)

rate_sensitivity <- tibble(
  interest_rate   = rates,
  total_liability = sapply(rates, function(r) {
    sum(price_census(census, life_2019_f, life_2019_m, i = r)$nsp)
  })
) %>%
  mutate(pct_change_vs_3pct = total_liability / total_liability[interest_rate == 0.03] - 1)

cat("--- Interest rate sensitivity ---\n")
print(rate_sensitivity)
cat("\n")

write_csv(rate_sensitivity, "data/processed/prt_rate_sensitivity.csv")

p_rate <- ggplot(rate_sensitivity, aes(x = interest_rate * 100, y = total_liability)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_label(aes(label = paste0("$", format(round(total_liability), big.mark = ","))),
             nudge_y = 5000, size = 3.5) +
  labs(
    title    = "Group annuity liability vs interest rate",
    subtitle = "Lower rates increase present value of long-duration liabilities",
    x = "Interest rate (%)", y = "Total group NSP ($)"
  ) +
  theme_minimal()

ggsave("outputs/prt_rate_sensitivity.png", plot = p_rate, width = 8, height = 5, dpi = 300)


#   Section 4: Duration and convexity of group liability

compute_duration_convexity <- function(census, life_f, life_m,
                                       i = 0.03, h = 0.0001) {
  P    <- sum(price_census(census, life_f, life_m, i)$nsp)
  P_up <- sum(price_census(census, life_f, life_m, i + h)$nsp)
  P_dn <- sum(price_census(census, life_f, life_m, i - h)$nsp)
  
  list(
    base_liability    = P,
    modified_duration = (P_dn - P_up) / (2 * h * P),
    convexity         = (P_up - 2 * P + P_dn) / (h ^ 2 * P)
  )
}


dc <- compute_duration_convexity(census, life_2019_f, life_2019_m, i = 0.03)

cat("--- Duration and convexity (i=3%, 2019 baseline) ---\n")
cat("Base liability:    $", format(round(dc$base_liability), big.mark = ","), "\n")
cat("Modified duration: ", round(dc$modified_duration, 2), "years\n")
cat("Convexity:         ", round(dc$convexity, 2), "\n\n")

# Validation: compare duration prediction vs actual rate sensitivity
di                   <- -0.01
predicted_pct_change <- -dc$modified_duration * di
actual_pct_change    <- rate_sensitivity$pct_change_vs_3pct[rate_sensitivity$interest_rate == 0.02]

cat("Validation: rate drops 3% -> 2% (di = -1%)\n")
cat("  Duration-predicted change: +", round(predicted_pct_change * 100, 2), "%\n")
cat("  Actual change (Section 3): +", round(actual_pct_change * 100, 2), "%\n")
cat("  Gap absorbed by convexity: ",
    round((actual_pct_change - predicted_pct_change) * 100, 2), "%\n\n")

# Duration is not constant: rises as rates fall
duration_by_rate <- tibble(
  interest_rate     = rates,
  modified_duration = sapply(rates, function(r) {
    compute_duration_convexity(census, life_2019_f, life_2019_m, i = r)$modified_duration
  })
)

cat("--- Duration at different rate levels ---\n")
print(duration_by_rate)
cat("Note: duration rises as rates fall — liability becomes more rate-sensitive\n\n")

write_csv(duration_by_rate, "data/processed/prt_duration_by_rate.csv")

p_duration <- ggplot(duration_by_rate,
                     aes(x = interest_rate * 100, y = modified_duration)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_label(aes(label = round(modified_duration, 1)), nudge_y = 0.15, size = 3.5) +
  labs(
    title    = "Modified duration vs interest rate",
    subtitle = "Duration rises as rates fall — convexity effect",
    x = "Interest rate (%)", y = "Modified duration (years)"
  ) +
  theme_minimal()

ggsave("outputs/prt_duration_by_rate.png", plot = p_duration,
       width = 8, height = 5, dpi = 300)


#   Section 5: Exponential decay stress scenario
#
# Script 06 used a step function for temporary stress:
#   years 0 to (stress_years-1): use 2020 mortality
#   year stress_years onwards:   snap back instantly to 2019 baseline
#
# This is unrealistic. A shock like COVID would fade gradually.
# A more defensible assumption is exponential decay of excess mortality:
#
#   qx_used(t) = qx_baseline(age+t) + excess(age+t) * decay_rate^t
#   excess(age) = qx_stress(age) - qx_baseline(age)
#
#   decay_rate = 1.0  -> stress never fades (permanent shift)
#   decay_rate = 0.5  -> excess mortality halves each year
#   decay_rate = 0.0  -> stress fully gone after year 0
#
# This connects to how Scale MP-2021 works: mortality improvement is a continuous path, not a discrete jump. 



price_annuity_exp_decay <- function(life_table_baseline, life_table_stress,
                                    issue_age, annual_benefit, i,
                                    decay_rate = 0.5) {
  v <- 1 / (1 + i)
  
  base_data <- life_table_baseline %>%
    filter(age >= issue_age) %>% arrange(age) %>%
    select(age, qx_base = qx, lx)
  
  stress_data <- life_table_stress %>%
    filter(age >= issue_age) %>% arrange(age) %>%
    select(age, qx_stress = qx)
  
  pricing_data <- base_data %>%
    left_join(stress_data, by = "age") %>%
    mutate(
      t       = age - issue_age,
      excess  = qx_stress - qx_base,
      qx_used = qx_base + excess * (decay_rate ^ t),
      px_used = 1 - qx_used
    )
  
  pricing_data$survival_prob    <- NA_real_
  pricing_data$survival_prob[1] <- 1
  
  for (k in 2:nrow(pricing_data)) {
    pricing_data$survival_prob[k] <-
      pricing_data$survival_prob[k - 1] * pricing_data$px_used[k - 1]
  }
  
  pricing_data <- pricing_data %>%
    mutate(pv_payment = annual_benefit * (v ^ t) * survival_prob)
  
  sum(pricing_data$pv_payment, na.rm = TRUE)
}


# Compare mortality paths: step vs exponential decay
issue_age_plot <- 65
t_vals         <- 0:19

baseline_qx <- life_2019_f %>%
  filter(age >= issue_age_plot, age < issue_age_plot + 20) %>%
  arrange(age) %>% pull(qx)

stress_qx <- life_2020_f %>%
  filter(age >= issue_age_plot, age < issue_age_plot + 20) %>%
  arrange(age) %>% pull(qx)

excess_qx <- stress_qx - baseline_qx

decay_paths <- tibble(
  t            = t_vals,
  baseline     = baseline_qx,
  full_stress  = stress_qx,
  step_2yr     = ifelse(t_vals < 2, stress_qx, baseline_qx),
  exp_decay_50 = baseline_qx + excess_qx * (0.50 ^ t_vals),
  exp_decay_75 = baseline_qx + excess_qx * (0.75 ^ t_vals)
) %>%
  pivot_longer(-t, names_to = "scenario", values_to = "qx")

p_decay <- ggplot(decay_paths, aes(x = t, y = qx, color = scenario, linetype = scenario)) +
  geom_line(linewidth = 0.9) +
  scale_linetype_manual(values = c(
    baseline     = "solid",   full_stress  = "solid",
    step_2yr     = "dashed",  exp_decay_50 = "dotdash",
    exp_decay_75 = "dotdash"
  )) +
  labs(
    title    = "Mortality path comparison: step vs exponential decay (female, age 65+)",
    subtitle = "Step function snaps back abruptly; exponential decay is more realistic",
    x = "Policy year (t)", y = "Mortality rate qx",
    color = "Scenario", linetype = "Scenario"
  ) +
  theme_minimal()

ggsave("outputs/prt_mortality_path_decay.png", plot = p_decay,
       width = 10, height = 5, dpi = 300)


# Price group census under different stress assumptions
price_census_exp_decay <- function(census, life_f_base, life_m_base,
                                   life_f_stress, life_m_stress,
                                   i, decay_rate) {
  census %>%
    rowwise() %>%
    mutate(
      nsp = {
        lb <- if (sex == "F") life_f_base   else life_m_base
        ls <- if (sex == "F") life_f_stress else life_m_stress
        price_annuity_exp_decay(lb, ls, age, annual_benefit, i, decay_rate)
      }
    ) %>%
    ungroup()
}

total_exp50 <- sum(price_census_exp_decay(census,
                                          life_2019_f, life_2019_m, life_2020_f, life_2020_m, 0.03, 0.50)$nsp)
total_exp75 <- sum(price_census_exp_decay(census,
                                          life_2019_f, life_2019_m, life_2020_f, life_2020_m, 0.03, 0.75)$nsp)
total_exp00 <- sum(price_census_exp_decay(census,
                                          life_2019_f, life_2019_m, life_2020_f, life_2020_m, 0.03, 0.00)$nsp)

stress_comparison <- tibble(
  scenario        = c("baseline", "full_stress_(decay=1)",
                      "exp_decay_75pct", "exp_decay_50pct", "no_persistence_(decay=0)"),
  decay_rate      = c(NA, 1.0, 0.75, 0.50, 0.0),
  total_liability = c(total_liability_baseline, total_liability_stress,
                      total_exp75, total_exp50, total_exp00)
) %>%
  mutate(pct_change = total_liability / total_liability_baseline - 1)

cat("--- Stress scenario comparison (group census, i=3%) ---\n")
print(stress_comparison)
cat("\n")

write_csv(stress_comparison, "data/processed/prt_stress_comparison.csv")



# Summary of key numbers for interview reference


cat("\n========== KEY NUMBERS FOR INTERVIEW ==========\n")

cat("\n[Term life stress — Scripts 05/06]\n")
cat("  Female NSP increase (2020 vs 2019): +16.7%\n")
cat("  Male   NSP increase (2020 vs 2019): +20.9%\n")

cat("\n[Single life annuity — age 65, $12k/yr, i=3%]\n")
cat("  Female: $", format(round(nsp_single_f65), big.mark=","), "\n")
cat("  Male:   $", format(round(nsp_single_m65), big.mark=","), "\n")
cat("  Male is LOWER — opposite of term life\n")

cat("\n[Group census — 15 participants, i=3%]\n")
cat("  Baseline: $", format(round(total_liability_baseline), big.mark=","), "\n")
cat("  Full stress (2020): $", format(round(total_liability_stress), big.mark=","),
    sprintf(" (%+.1f%%)\n", (total_liability_stress/total_liability_baseline-1)*100))

cat("\n[Interest rate sensitivity]\n")
for (r in 1:nrow(rate_sensitivity)) {
  cat(sprintf("  i=%.0f%%: $%s  (%+.1f%% vs i=3%%)\n",
              rate_sensitivity$interest_rate[r]*100,
              format(round(rate_sensitivity$total_liability[r]), big.mark=","),
              rate_sensitivity$pct_change_vs_3pct[r]*100))
}

cat("\n[Duration and convexity — i=3%, 2019 baseline]\n")
cat("  Modified duration:", round(dc$modified_duration, 2), "years\n")
cat("  Convexity:        ", round(dc$convexity, 2), "\n")
cat("  Meaning: 1% rate drop -> liability up ~",
    round(dc$modified_duration, 1), "%\n")
cat(sprintf("  Duration predicted: %+.2f%%   Actual: %+.2f%%   Gap (convexity): %+.2f%%\n",
            predicted_pct_change*100, actual_pct_change*100,
            (actual_pct_change - predicted_pct_change)*100))

cat("\n[Exponential decay stress]\n")
for (r in 1:nrow(stress_comparison)) {
  cat(sprintf("  %-30s $%s  (%+.1f%%)\n",
              stress_comparison$scenario[r],
              format(round(stress_comparison$total_liability[r]), big.mark=","),
              stress_comparison$pct_change[r]*100))
}
cat("  decay=0.5 means excess mortality halves each year\n")
cat("  More realistic than step function; consistent with MP-2021 logic\n")


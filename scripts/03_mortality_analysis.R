library(readr)
library(dplyr)
library(ggplot2)

mort_clean <- read_csv("data/processed/mortality_clean.csv")

mort_clean <- mort_clean %>%
  mutate(qx = deaths / population)

# mortality curve for one year
mort_2019 <- mort_clean %>%
  filter(Year == 2019)

ggplot(mort_2019, aes(x = age, y = qx, color = sex)) +
  geom_line() +
  scale_y_log10() +
  labs(
    title = "Mortality Curve (2019)",
    x = "Age",
    y = "Mortality Rate (log scale)"
  )

mort_compare <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Female")

ggplot(mort_compare, aes(x = age, y = qx, color = factor(Year))) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Female Mortality Curve: 2019 vs 2020",
    x = "Age",
    y = "Mortality Rate (log scale)",
    color = "Year"
  )

mort_compare <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Male")

ggplot(mort_compare, aes(x = age, y = qx, color = factor(Year))) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Male Mortality Curve: 2019 vs 2020",
    x = "Age",
    y = "Mortality Rate (log scale)",
    color = "Year"
  )



mort_trend <- mort_clean %>%
  filter(age %in% c(40, 60, 80), sex == "Female")

ggplot(mort_trend, aes(x = Year, y = qx, color = factor(age))) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Female Mortality Trend by Selected Ages",
    x = "Year",
    y = "Mortality Rate",
    color = "Age"
  )


mort_excess <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Female") %>%
  select(Year, age, qx) %>%
  tidyr::pivot_wider(names_from = Year, values_from = qx) %>%
  mutate(excess_ratio = (`2020` / `2019`) - 1)

ggplot(mort_excess, aes(x = age, y = excess_ratio)) +
  geom_line() +
  labs(
    title = "Excess Mortality Ratio by Age (Female, 2020 vs 2019)",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

mort_excess <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Male") %>%
  select(Year, age, qx) %>%
  tidyr::pivot_wider(names_from = Year, values_from = qx) %>%
  mutate(excess_ratio = (`2020` / `2019`) - 1)

ggplot(mort_excess, aes(x = age, y = excess_ratio)) +
  geom_line() +
  labs(
    title = "Excess Mortality Ratio by Age (Male, 2020 vs 2019)",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

twentyy_baseline_mort <- mort_clean %>% #here can be argued with precisee weight, intuitively should be closer to 2020 the heigher weight.
  filter(Year >= 2000, Year <= 2019, sex == "Female") %>%
  group_by(age) %>%
  summarise(
    baseline_qx = mean(qx, na.rm = TRUE)
  )


mort_2020 <- mort_clean %>%
  filter(Year == 2020, sex == "Female") %>%
  select(age, qx)

covid_excess <- twentyy_baseline_mort %>%
  left_join(mort_2020, by = "age") %>%
  mutate(
    excess_ratio = qx / baseline_qx - 1
  )

ggplot(covid_excess, aes(x = age, y = excess_ratio)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Excess Mortality in 2020 Relative to 2000–2019 Baseline",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

baseline_5yr <- mort_clean %>%
  filter(Year >= 2015, Year <= 2019, sex == "Female") %>%
  group_by(age) %>%
  summarise(
    baseline_qx = mean(qx, na.rm = TRUE)
  )


covid_excess_5yr <- baseline_5yr %>%
  left_join(mort_2020, by = "age") %>%
  mutate(
    excess_ratio = qx / baseline_qx - 1
  )


ggplot(covid_excess_5yr, aes(x = age, y = excess_ratio)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Excess Mortality in 2020 Relative to 2015–2019 Baseline",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

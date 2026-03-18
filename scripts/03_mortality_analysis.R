library(readr)
library(dplyr)
library(ggplot2)

mort_clean <- read_csv("data/processed/mortality_clean.csv")

mort_clean <- mort_clean %>%
  mutate(qx = deaths / population)

# mortality curve for one year
mort_2019 <- mort_clean %>%
  filter(Year == 2019)

p_mort_2019 <- ggplot(mort_2019, aes(x = age, y = qx, color = sex)) +
  geom_line() +
  scale_y_log10() +
  labs(
    title = "Mortality Curve (2019)",
    x = "Age",
    y = "Mortality Rate (log scale)"
  )

ggsave(
  "outputs/03_mortality_curve_2019.png",
  plot = p_mort_2019,
  width = 8,
  height = 5,
  dpi = 300
)

mort_compare_f <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Female")

p_mort_f <- ggplot(mort_compare_f, aes(x = age, y = qx, color = factor(Year))) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Female Mortality Curve: 2019 vs 2020",
    x = "Age",
    y = "Mortality Rate (log scale)",
    color = "Year"
  )

ggsave(
  "outputs/03_mortality_female_2019_2020.png",
  plot = p_mort_f,
  width = 8,
  height = 5,
  dpi = 300
)

mort_compare_m <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Male")

p_mort_m <- ggplot(mort_compare_m, aes(x = age, y = qx, color = factor(Year))) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Male Mortality Curve: 2019 vs 2020",
    x = "Age",
    y = "Mortality Rate (log scale)",
    color = "Year"
  )

ggsave(
  "outputs/03_mortality_male_2019_2020.png",
  plot = p_mort_m,
  width = 8,
  height = 5,
  dpi = 300
)


mort_trend <- mort_clean %>%
  filter(age %in% c(40, 60, 80), sex == "Female")

p_trend <- ggplot(mort_trend, aes(x = Year, y = qx, color = factor(age))) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Female Mortality Trend by Selected Ages",
    x = "Year",
    y = "Mortality Rate",
    color = "Age"
  )

ggsave(
  "outputs/03_mortality_trend_female.png",
  plot = p_trend,
  width = 8,
  height = 5,
  dpi = 300
)

mort_excess_f <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Female") %>%
  select(Year, age, qx) %>%
  tidyr::pivot_wider(names_from = Year, values_from = qx) %>%
  mutate(excess_ratio = (`2020` / `2019`) - 1)

p_excess_f <- ggplot(mort_excess_f, aes(x = age, y = excess_ratio)) +
  geom_line() +
  labs(
    title = "Excess Mortality Ratio (Female, 2020 vs 2019)",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

ggsave(
  "outputs/03_excess_mortality_female.png",
  plot = p_excess_f,
  width = 8,
  height = 5,
  dpi = 300
)

mort_excess_m <- mort_clean %>%
  filter(Year %in% c(2019, 2020), sex == "Male") %>%
  select(Year, age, qx) %>%
  tidyr::pivot_wider(names_from = Year, values_from = qx) %>%
  mutate(excess_ratio = (`2020` / `2019`) - 1)

p_excess_m <- ggplot(mort_excess_m, aes(x = age, y = excess_ratio)) +
  geom_line() +
  labs(
    title = "Excess Mortality Ratio (Male, 2020 vs 2019)",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

ggsave(
  "outputs/03_excess_mortality_male.png",
  plot = p_excess_m,
  width = 8,
  height = 5,
  dpi = 300
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

p1 <- ggplot(covid_excess, aes(x = age, y = excess_ratio)) +
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


p2 <- ggplot(covid_excess_5yr, aes(x = age, y = excess_ratio)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Excess Mortality in 2020 Relative to 2015–2019 Baseline",
    x = "Age",
    y = "Excess Mortality Ratio"
  )

# avoid error
if(!dir.exists("outputs")){
  dir.create("outputs")
}

# output plots
ggsave(
  filename = "outputs/excess_mortality_2000_2019.png",
  plot = p1,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "outputs/excess_mortality_2015_2019.png",
  plot = p2,
  width = 8,
  height = 5,
  dpi = 300
)



# 08_spda_pricing.R
# Single Premium Deferred Annuity — pricing, reserve adequacy, profit testing

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# ── 0. Load life tables ───────────────────────────────────────────────────────

life_2019_f <- read_csv("data/processed/life_table_2019_female.csv", show_col_types = FALSE)
life_2019_m <- read_csv("data/processed/life_table_2019_male.csv",   show_col_types = FALSE)

price_whole_life_annuity <- function(lt, issue_age, annual_benefit, i) {
  v   <- 1 / (1 + i)
  dat <- lt %>% filter(age >= issue_age) %>% arrange(age)
  lx0 <- dat$lx[1]
  dat %>%
    mutate(t = age - issue_age, surv = lx / lx0) %>%
    summarise(nsp = sum(annual_benefit * v^t * surv, na.rm = TRUE)) %>%
    pull(nsp)
}

# ── 1. Assumptions ────────────────────────────────────────────────────────────

ISSUE_AGE      <- 50
DEFER_YRS      <- 15       # annuitizes at age 65
SP             <- 100000
CRED_RATE      <- 0.04     # credited to policyholder
INV_YIELD      <- 0.05     # company earns
EXP_RATIO      <- 0.005
ANNUITY_I      <- 0.03

sc_schedule    <- c(0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, rep(0, 10))
base_lapse     <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, rep(0.10, 10))

get_qx <- function(age) {
  life_2019_f %>% filter(age == !!age) %>% pull(qx) %>% first()
}

# ── 2. Accumulation phase ─────────────────────────────────────────────────────

run_accum <- function(lapse_mult = 1) {
  
  av   <- SP
  surv <- 1.0
  out  <- vector("list", DEFER_YRS)
  
  for (t in seq_len(DEFER_YRS)) {
    qx <- get_qx(ISSUE_AGE + t - 1)
    lr <- base_lapse[t] * lapse_mult
    sc <- sc_schedule[t]
    
    deaths   <- surv * qx
    lapses   <- (surv - deaths) * lr
    surv_new <- surv - deaths - lapses
    
    av_new   <- av * (1 + CRED_RATE) * (1 - EXP_RATIO)
    
    rem     <- DEFER_YRS - t
    av65    <- av_new * (1 + CRED_RATE)^rem
    reserve <- av65 / (1 + ANNUITY_I)^rem
    
    out[[t]] <- tibble(
      t          = t,
      av_bop     = av,
      av_eop     = av_new,
      qx         = qx,
      lapse_rate = lr,
      sc         = sc,
      surv_bop   = surv,
      deaths     = deaths,
      lapses     = lapses,
      surv_eop   = surv_new,
      reserve    = reserve,
      sv         = av_new * (1 - ifelse(t < DEFER_YRS, sc_schedule[t+1], 0))
    )
    
    av   <- av_new
    surv <- surv_new
  }
  
  bind_rows(out)
}

base <- run_accum()

# ── 3. Reserve adequacy ───────────────────────────────────────────────────────

base <- base %>% mutate(adequate = reserve >= sv)

cat("Reserve adequacy failures:", sum(!base$adequate), "\n")
print(base %>% select(t, av_eop, reserve, sv, adequate))

# ── 4. Annuitization at age 65 ────────────────────────────────────────────────

av65        <- base$av_eop[DEFER_YRS]
price_per1  <- price_whole_life_annuity(life_2019_f, 65, 1, ANNUITY_I)
ann_benefit <- av65 / price_per1

cat(sprintf("\nAV at 65: $%s  |  Annual benefit: $%s/yr  ($%s/mo)\n",
            format(round(av65), big.mark=","),
            format(round(ann_benefit), big.mark=","),
            format(round(ann_benefit/12), big.mark=",")))

# ── 5. Profit testing ─────────────────────────────────────────────────────────

run_profit <- function(lapse_mult = 1) {
  
  acc     <- run_accum(lapse_mult)
  profits <- numeric(DEFER_YRS + 1)
  
  res_prev <- SP
  profits[1] <- 0
  
  for (t in seq_len(DEFER_YRS)) {
    r       <- acc[t, ]
    assets  <- res_prev * r$surv_bop
    res_eop <- r$reserve * r$surv_eop
    
    profits[t+1] <-
      assets * INV_YIELD                    -
      r$av_bop * r$surv_bop * CRED_RATE    -
      r$av_bop * r$surv_bop * EXP_RATIO    -
      r$deaths * r$av_bop                  +
      r$lapses * r$av_bop * r$sc           -
      (res_eop - assets)
    
    res_prev <- r$reserve
  }
  
  irr <- tryCatch(
    uniroot(function(r) sum(profits / (1+r)^(0:DEFER_YRS)),
            c(-0.5, 2))$root,
    error = function(e) NA_real_
  )
  
  list(profits = profits, irr = irr, total = sum(profits))
}

p_base <- run_profit(1.0)
p_high <- run_profit(1.5)
p_low  <- run_profit(0.5)

cat("\n── Lapse sensitivity ──────────────────────\n")
cat(sprintf("  Low  (0.5x): total $%s  IRR %.2f%%\n",
            format(round(p_low$total),  big.mark=","), p_low$irr*100))
cat(sprintf("  Base (1.0x): total $%s  IRR %.2f%%\n",
            format(round(p_base$total), big.mark=","), p_base$irr*100))
cat(sprintf("  High (1.5x): total $%s  IRR %.2f%%\n",
            format(round(p_high$total), big.mark=","), p_high$irr*100))

# ── 6. Plots ──────────────────────────────────────────────────────────────────

dir.create("outputs", showWarnings = FALSE)

base %>%
  select(t, av_eop, reserve, sv) %>%
  pivot_longer(-t) %>%
  mutate(name = recode(name, av_eop="Account Value",
                       reserve="Reserve", sv="Surrender Value")) %>%
  ggplot(aes(t, value/1000, color=name)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  labs(title="SPDA: AV, Reserve, Surrender Value",
       x="Policy Year", y="Value ($000s)", color=NULL) +
  theme_minimal() + theme(legend.position="bottom")

ggsave("outputs/08_av_reserve_sv.png", width=7, height=4, dpi=150)

tibble(
  year          = 1:DEFER_YRS,
  `Low (0.5x)`  = p_low$profits[-1],
  `Base (1.0x)` = p_base$profits[-1],
  `High (1.5x)` = p_high$profits[-1]
) %>%
  pivot_longer(-year) %>%
  ggplot(aes(year, value/1000, color=name)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60") +
  labs(title="SPDA Annual Profit by Lapse Scenario",
       x="Policy Year", y="Profit ($000s)", color="Lapse") +
  theme_minimal() + theme(legend.position="bottom")

ggsave("outputs/08_profit_lapse.png", width=7, height=4, dpi=150)

cat("\nDone. Plots saved to outputs/\n")
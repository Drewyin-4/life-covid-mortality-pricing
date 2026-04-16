# Life Insurance Pricing & Mortality Stress Analysis

Actuarial pricing project built in R using U.S. mortality data from CDC WONDER (1999–2020).

## Overview

Scripts 01–06 cover term life pricing and COVID mortality stress testing. Script 07 extends into a PRT context with group annuity pricing, interest rate sensitivity, and duration analysis. Script 08 adds a Single Premium Deferred Annuity (SPDA) pricing model covering the full product lifecycle.

**Key insight across 07–08:** Higher mortality increases term life premiums but reduces annuity liability, because annuitants collect for fewer years. This asymmetry runs through all the annuity-side work.

## Scripts

**01–02** — Import and clean CDC mortality data

**03** — Mortality analysis: 2019 vs 2020 excess mortality by age and sex

**04** — Life table construction: qx, lx, dx, ex for 2019 and 2020

**05** — Term life pricing: net single premium for a 20-year policy, COVID stress test (+20.9% male, +16.7% female NSP under 2020 mortality)

**06** — Level premium and stress scenarios: equivalence principle, step-function and exponential decay stress

**07** — PRT extension: whole life annuity pricing, group census (15 participants), interest rate sensitivity (i = 2%–5%), modified duration and convexity of group liability

**08** — SPDA pricing: accumulation phase with crediting rate and lapse/surrender modeling, reserve adequacy testing against surrender value, annuitization at age 65, profit testing with IRR, lapse sensitivity analysis (0.5x / 1x / 1.5x)

## Data

CDC WONDER Multiple Cause of Death public data. Raw data not included due to file size. Download from https://wonder.cdc.gov and place in `data/raw/`.

## Structure

- `data/raw/` — raw CDC data (not tracked)
- `data/processed/` — cleaned datasets and life tables
- `scripts/` — R scripts 01–08
- `outputs/` — figures
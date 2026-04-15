# Life Insurance Pricing & PRT Extension
Actuarial pricing project built in R using U.S. mortality data from CDC WONDER (1999–2020).

## What this project covers
**Scripts 01–06: Term life pricing and mortality stress testing**
- Construct life tables from CDC mortality data by age, sex, and year
- Price a 20-year term life product under baseline (2019) and COVID stress (2020) mortality
- Stress test results: male NSP increased ~20.9%, female ~16.7% under a one-year mortality shock
- Model temporary stress scenarios including an exponential decay approach

**Script 07: Extension into PRT (Pension Risk Transfer) context**
- Convert term life pricing logic into whole life annuity pricing
- Price a synthetic group census (15 participants) to approximate a PRT transaction
- Interest rate sensitivity analysis (i = 2% to 5%)
- Modified duration and convexity of the group annuity liability

Key insight: term life and annuity pricing go in opposite directions under a mortality shock. Higher mortality increases term life premiums but reduces annuity liability — because annuitants collect for fewer years.

## Data
CDC WONDER Multiple Cause of Death public data. Raw data not included in this repo due to file size. Download from https://wonder.cdc.gov and place in `data/raw/`.

## Structure
- `data/raw/` — raw CDC data (not tracked)
- `data/processed/` — cleaned datasets and model outputs
- `scripts/` — R scripts 01–07
- `outputs/` — figures
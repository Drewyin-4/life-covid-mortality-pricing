# Life COVID Mortality Pricing

This project analyzes COVID-era mortality shock and its implications for life insurance pricing using U.S. mortality data from CDC WONDER.

## Project Goals
- Study mortality changes before and during COVID
- Construct mortality rates by age and year
- Build actuarial life tables
- Evaluate the impact on term life insurance pricing

## Data Source
- CDC WONDER Multiple Cause of Death data

## Project Structure
- `data/raw/`: raw downloaded data
- `data/processed/`: cleaned datasets
- `scripts/`: R scripts
- `outputs/`: figures and tables
- `report/`: final actuarial-style report


### Baseline Comparison

Excess mortality in 2020 was compared against two baselines: the long-term average (2000–2019) and a recent pre-pandemic average (2015–2019). 

Relative to the long-term baseline, the increase in mortality appears modest. However, compared with the recent baseline, the mortality rise in 2020 becomes much more evident. This likely reflects the long-term decline in mortality over the past two decades, meaning older historical data may partially dilute the observed COVID-era mortality shock.
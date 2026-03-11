# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# read dataset
tether <- read_csv("data/Tether USDt/Tether_USDt_combined.csv")

# inspect columns
names(tether)
str(tether)
# Structure matches the other stablecoin datasets.
# timeOpen will be used to construct the dataset date.
# name appears to be a numeric CoinMarketCap id and is not useful for analysis.
# marketCap and circulatingSupply will be checked before deciding whether to retain them.
summary(tether$marketCap)
summary(tether$circulatingSupply)
sum(tether$marketCap == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
sum(tether$circulatingSupply == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
# marketCap and circulatingSupply appear valid for Tether.
# Both variables contain meaningful positive values across the sample, with only one zero observation in circulatingSupply.

# quick inspection of first few rows
head(tether[, c("timeOpen","close","volume","marketCap","circulatingSupply")])

# using timeOpen to create date
tether <- tether %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# quick check
head(tether[, c("date","close","volume","marketCap","circulatingSupply")])

# getting dataset time range now
min(tether$date)
max(tether$date)
## The Tether dataset covers 2017-11-27 to 2025-12-31

# check for missing values
colSums(is.na(tether)) # none
# check for duplicate days
sum(duplicated(tether$date)) # none
# check for volume = 0
sum(tether$volume == 0) # none


## IMPORTANT: Summary so far ##
# Dataset covers: 2017-11-27 to 2025-12-31
# Total observations: 2957 daily observations
# No missing values across variables
# No duplicate dates
# No zero trading volume observations
# circulatingSupply is zero only once

# Rolling 30-day cumulative trading volume used in dynamic threshold construction
tether <- tether %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
tether <- tether %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
         ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)
# Dynamic depegging thresholds following liquidity-adjusted formulation
# Larger trading volume narrows the allowed deviation from the $1 peg

# create next-day prediction variable
tether <- tether %>%
  mutate(next_close = lead(close), .after = close)

# create depegging binary variable
tether <- tether %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)
# Depegging event defined when next-day closing price breaches dynamic threshold band

# quick check for no. of depegging events
table(tether$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(tether$depeg, na.rm = TRUE)

# Depegging events: 1016 observations (~34.7% of sample)
# Tether shows a much higher event frequency than DAI, Pax, or USDC.
# This likely reflects the very tight dynamic threshold band implied by Tether’s extremely high trading volume, so even small next-day price deviations are often classified as depegs.

# small improvement: remove invalid rows like first 29 rows (no Vmonthly) and last row (no next_close)
tether_final <- tether %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(next_close))

# check final dataset span
min(tether_final$date)
max(tether_final$date)
nrow(tether_final)

## Final analysis dataset after feature construction
## First 29 observations removed due to rolling 30-day volume calculation
## Last observation removed due to missing next-day close price

## Final dataset span: 2017-12-26 to 2025-12-30
## Total observations used for analysis: 2927

# quick verify event counts after trimming dataset
table(tether_final$depeg)
mean(tether_final$depeg)

## Final event statistics after dataset trimming
## Total observations: 2927
## Depegging events: 1016
## Non-depegging observations: 1911
## Event frequency: ~34.7%
## Event frequency is much higher for Tether compared to other stablecoins, likely due to extremely high trading volumes producing very tight thresholds.

# quick check for thresholds to make sure they look reasonable
summary(tether_final$ThreshD)
summary(tether_final$ThreshU)
# Dynamic thresholds are extremely tight for Tether.
# Typical band lies roughly between 0.999 and 1.001 (±0.1% around the peg).
# This occurs because USDT has very large trading volumes, which shrink the liquidity-adjusted threshold band and cause small deviations to be flagged as depegging events more frequently.

# check abs deviation from peg
summary(abs(tether_final$close - 1))
# Absolute deviations from the $1 peg remain small on average.
# Median deviation is ~0.0005 (≈0.05%), indicating USDT usually trades very close to the peg.
# Maximum deviation reaches ~5.36%, reflecting rare stress periods in the market.

# final dataset cols check
names(tether_final)
# arrange cols in nice order
tether_final <- tether_final %>%
  select(
    date,
    open,
    high,
    low,
    close,
    next_close,
    depeg,
    volume,
    V_monthly,
    ThreshD,
    ThreshU,
    marketCap,
    circulatingSupply
  )
# Clean analysis dataset: removed raw CoinMarketCap timestamp fields
# Retained price variables, liquidity measures, thresholds, and depeg label

# identify largest deviations of Tether price from the $1 peg, helps visually verify that major deviations correspond to depegging events
# using close here (instead of next_close) since not tied to the pred horizon and is descriptive
tether_check <- tether_final %>%
  mutate(dev_from_peg = abs(close - 1))
# find largest deviations
tether_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, volume) %>%
  head(10)
# check if these were labelled as depeg
tether_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg) %>%
  head(10)
# Most large deviations are classified as depegging events.
# However, a few extreme price spikes (e.g., 2020-03-12) are labelled 0
# because the next-day price returned within the threshold band.

# Largest deviation in dataset, check for one day after
tether_check %>%
  filter(date == "2020-03-12") %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg)
# On 2020-03-12 USDT showed its largest price spike.
# But the next-day price moved back within the threshold band, so it is correctly labelled depeg = 0.

# quick verification to see if all largest deviations correspond to actual threshold breaches when using next_close
tether_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg) %>%
  head(15)
# Most large deviations correspond to depeg = 1, confirming the threshold rule works.
# A few large spikes are labelled 0 because the next-day price returned within the threshold band, indicating temporary dislocations rather than sustained depegs.


dim(tether_final)
summary(tether_final$depeg)

# Save dataset
write_csv(tether_final, "data/Tether USDt/Tether_model_trimmed_dataset.csv")
write_csv(tether, "data/Tether USDt/Tether_model_dataset.csv")


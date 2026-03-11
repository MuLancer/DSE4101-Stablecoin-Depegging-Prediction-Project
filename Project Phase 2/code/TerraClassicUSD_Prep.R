# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# read dataset
terra_usd <- read_csv("data/TerraClassicUSD/TerraClassicUSD_combined.csv")

# inspect columns
names(terra_usd)
str(terra_usd)
# timeOpen is the daily date -> can be used as dataset date
# name is 2781 everywhere, likely CMC id so useless, can be dropped
# marketCap and circulatingSupply have first few values =0, let's check
summary(terra_usd$marketCap)
summary(terra_usd$circulatingSupply)
sum(terra_usd$marketCap == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
sum(terra_usd$circulatingSupply == 0, na.rm = TRUE) # quick check for how many zeros and non zeros

# quick inspection of first few rows
head(terra_usd[, c("timeOpen","close","volume","marketCap","circulatingSupply")])

# using timeOpen to create date
terra_usd <- terra_usd %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# quick check
head(terra_usd[, c("date","close","volume","marketCap","circulatingSupply")])

# getting dataset time range now
min(terra_usd$date)
max(terra_usd$date)
## The Terra_USD Dataset covers 2020-11-25 to 2025-12-31

# check for missing values
colSums(is.na(terra_usd)) # none
# check for duplicate days
sum(duplicated(terra_usd$date)) # none
# check for volume = 0
sum(terra_usd$volume == 0) # none


## IMPORTANT: Summary so far ##
# Dataset covers: 2020-11-25 to 2025-12-31
# Total observations: 1863 daily observations
# No missing values across variables
# No duplicate dates
# No zero trading volume observations

# Rolling 30-day cumulative trading volume used in dynamic threshold construction
terra_usd <- terra_usd %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
terra_usd <- terra_usd %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
         ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)
# Dynamic depegging thresholds following liquidity-adjusted formulation
# Larger trading volume narrows the allowed deviation from the $1 peg

# create next-day prediction variable
terra_usd <- terra_usd %>%
  mutate(next_close = lead(close), .after = close)

# create depegging binary variable
terra_usd <- terra_usd %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)
# Depegging event defined when next-day closing price breaches dynamic threshold band

# quick check for no. of depegging events
table(terra_usd$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(terra_usd$depeg, na.rm = TRUE)

# Depegging events: 1361 observations (~74.2% of sample)
# All starting May 8th~9th are depegged (this was the May 2022 crash and formerly UST, now TerraUSD never recovered)
# Dataset contains post-collapse regime

# TerraUSD permanently lost its peg during the May 2022 collapse.
# To ensure the model captures temporary depegging events rather than a permanent failure regime, the sample is restricted to the pre-collapse period.
terra_usd <- terra_usd %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# Restrict TerraUSD sample to pre-collapse regime
terra_usd <- terra_usd %>%
  filter(date < "2022-05-09")
# Rolling 30-day cumulative trading volume used in dynamic threshold construction
terra_usd <- terra_usd %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
terra_usd <- terra_usd %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
         ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)

# create next-day prediction variable
terra_usd <- terra_usd %>%
  mutate(next_close = lead(close), .after = close)

# create depegging binary variable
terra_usd <- terra_usd %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)

# quick check for no. of depegging events
table(terra_usd$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(terra_usd$depeg, na.rm = TRUE)
# Depegging events: 28 observations (~5.6% of sample which is 530 events)
# Dataset contains only pre-collapse regime

# small improvement: remove invalid rows like first 29 rows (no Vmonthly) and last row (no next_close)
terra_usd_final <- terra_usd %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(next_close))

# check final dataset span
min(terra_usd_final$date)
max(terra_usd_final$date)
nrow(terra_usd_final)

## Final analysis dataset after feature construction
## First 29 observations removed due to rolling 30-day volume calculation
## Last observation removed due to missing next-day close price

## Final dataset span: 2020-12-24 to 2022-05-07
## Total observations used for analysis: 500

# quick verify event counts after trimming dataset
table(terra_usd_final$depeg)
mean(terra_usd_final$depeg)

## Final event statistics after dataset trimming
## Total observations: 500
## Depegging events: 28
## Non-depegging observations: 472
## Event frequency: ~5.6%
## This indicates a realistic rare-event setting for depegging prediction.

# quick check for thresholds to make sure they look reasonable
summary(terra_usd_final$ThreshD)
summary(terra_usd_final$ThreshU)
# Dynamic thresholds behave as expected for TerraUSD.
# The typical band lies roughly between 0.992 and 1.008 (≈ ±0.8% around the $1 peg).
# When trading volume decreases the band widens slightly, allowing larger deviations before classifying a depegging event.
# Typical allowed band is ≈ ±0.8% around $1 which is very realistic for UST, which was known to drift slightly more than fiat-backed stablecoins.

# check abs deviation from peg
summary(abs(terra_usd_final$close - 1))
# TerraUSD mostly trades very close to the $1 peg in the pre-collapse period.
# The median deviation is only ~0.0015 (≈0.15%), indicating tight peg stability.

# final dataset cols check
names(terra_usd_final)
# arrange cols in nice order
terra_usd_final <- terra_usd_final %>%
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

# identify largest deviations of TerraUSD price from the $1 peg, helps visually verify that major deviations correspond to depegging events
# using close here (instead of next_close) since not tied to the pred horizon and is descriptive
terra_usd_check <- terra_usd_final %>%
  mutate(dev_from_peg = abs(close - 1))
# find largest deviations
terra_usd_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, volume) %>%
  head(10)
# check if these were labelled as depeg
terra_usd_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg) %>%
  head(10)
# First observation for 2020-12-30 has extremely low volume for UST. Early in its launch, TerraUSD markets were thin, so small trades could move price a lot.
# Rest rep the largest real stress period in our pre-collapse data. May 2021 was when btc also dropped 40% during some days
# UST during the time, probably lost peg due to redemption pressure.

# Largest deviation in dataset, check for one day after
terra_usd_check %>%
  filter(date == "2020-12-30") %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg)
# The large deviation on 2020-12-30  likely reflects early launch liquidity conditions when TerraUSD markets were still thin.
# The next-day price returned inside the threshold band, so the event is not classified as a persistent depegging episode.

# quick verification to see if all largest deviations correspond to actual threshold breaches when using next_close
terra_usd_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg) %>%
  head(15)
# Most large deviations are correctly classified as depegging events.
# Observations with depeg = 0 occur when the deviation is short-lived and the next-day price returns within the dynamic threshold band.
# This confirms the rule captures persistent peg breaks rather than temporary one-day price spikes.

dim(terra_usd_final)
summary(terra_usd_final$depeg)

# Save dataset
write_csv(terra_usd_final, "data/TerraClassicUSD/TerraClassicUSD_model_trimmed_dataset.csv")
write_csv(terra_usd, "data/TerraClassicUSD/TerraClassicUSD_model_dataset.csv")

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# read dataset
usdc <- read_csv("data/USDC/USDC_combined.csv")

# inspect columns
names(usdc)
str(usdc)
# timeOpen is the daily date -> can be used as dataset date
# name is 2781 everywhere, likely CMC id so useless, can be dropped
# marketCap and circulatingSupply have first few values =0, let's check
summary(usdc$marketCap)
summary(usdc$circulatingSupply)
sum(usdc$marketCap == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
sum(usdc$circulatingSupply == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
# seems like only 9~10 rows are zeros. this was probably before cmc started tracking these vars properly..

# quick inspection of first few rows
head(usdc[, c("timeOpen","close","volume","marketCap","circulatingSupply")])

# using timeOpen to create date
usdc <- usdc %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# quick check
head(usdc[, c("date","close","volume","marketCap","circulatingSupply")])

# getting dataset time range now
min(usdc$date)
max(usdc$date)
## The Dai Dataset covers 2018-10-08 to 2025-12-31

# check for missing values
colSums(is.na(usdc)) # none
# check for duplicate days
sum(duplicated(usdc$date)) # none
# check for volume = 0
sum(usdc$volume == 0) # none


## IMPORTANT: Summary so far ##
# Dataset covers: 2018-10-08 to 2025-12-31
# Total observations: 2642 daily observations
# No missing values across variables
# No duplicate dates
# No zero trading volume observations
# marketCap and circulatingSupply are zero only in early observations

# Rolling 30-day cumulative trading volume used in dynamic threshold construction
usdc <- usdc %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
usdc <- usdc %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
         ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)
# Dynamic depegging thresholds following liquidity-adjusted formulation
# Larger trading volume narrows the allowed deviation from the $1 peg

# create next-day prediction variable
usdc <- usdc %>%
  mutate(next_close = lead(close), .after = close)

# create depegging binary variable
usdc <- usdc %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)
# Depegging event defined when next-day closing price breaches dynamic threshold band

# quick check for no. of depegging events
table(usdc$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(usdc$depeg, na.rm = TRUE)

# Depegging events: 222 observations (~8.5% of sample)
# USDC shows slightly more peg deviations than DAI (~7.2%) but similar to Pax (~8.8%).
# The event frequency remains moderate, making the dataset suitable for classification modelling without severe class imbalance.

# small improvement: remove invalid rows like first 29 rows (no Vmonthly) and last row (no next_close)
usdc_final <- usdc %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(next_close))

# check final dataset span
min(usdc_final$date)
max(usdc_final$date)
nrow(usdc_final)

## Final analysis dataset after feature construction
## First 29 observations removed due to rolling 30-day volume calculation
## Last observation removed due to missing next-day close price

## Final dataset span: 2018-11-06 to 2025-12-30
## Total observations used for analysis: 2612

# quick verify event counts after trimming dataset
table(usdc_final$depeg)
mean(usdc_final$depeg)

## Final event statistics after dataset trimming
## Total observations: 2612
## Depegging events: 222
## Non-depegging observations: 2390
## Event frequency: ~8.5%
## This indicates moderate class imbalance but still sufficient positive cases for ML models.

# quick check for thresholds to make sure they look reasonable
summary(usdc_final$ThreshD)
summary(usdc_final$ThreshU)
# Dynamic thresholds behave as expected.
# The typical band lies roughly around ~0.998 to ~1.002 near the $1 peg.
# Compared to PAX and DAI, USDC shows tighter bands, reflecting higher trading volume and liquidity in USDC markets.

# check abs deviation from peg
summary(abs(usdc_final$close - 1))
# Median deviation is ~0.00017 (~0.02%), indicating USDC trades extremely close to the peg. 
# The maximum deviation (~4.4%) reflects rare market stress periods where price temporarily moved further from $1.

# final dataset cols check
names(usdc_final)
# arrange cols in nice order
usdc_final <- usdc_final %>%
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

# identify largest deviations of DAI price from the $1 peg, helps visually verify that major deviations correspond to depegging events
# using close here (instead of next_close) since not tied to the pred horizon and is descriptive
usdc_check <- usdc_final %>%
  mutate(dev_from_peg = abs(close - 1))
# find largest deviations
usdc_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, volume) %>%
  head(10)
# check if these were labelled as depeg
usdc_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg) %>%
  head(10)
## These are real historical stress periods for USDC e.g., March 2020 market crash
# and the March 2023 USDC depeg during the Silicon Valley Bank crisis.
# 2023-03-11 → that’s the SVB crisis, when USDC briefly lost its peg
# 2020-03-12 / 2020-03-18 → COVID crypto liquidity shock

# Largest deviation in dataset, check for one day after
usdc_check %>%
  filter(date == "2020-03-13") %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg)
# large deviation does not always imply a depeg event.
# On 2020-03-13 USDC closed at ~1.00 and remained within the dynamic threshold band.
# The next-day price also stayed within the band, so this observation is correctly classified as a non-depegging event (depeg = 0).

# quick verification to see if all largest deviations correspond to actual threshold breaches when using next_close
usdc_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg) %>%
  head(15)
# Most large deviations correspond to depeg = 1, confirming the threshold rule works.
# Some cases (e.g., 2020-03-12 or 2018-11-26) are labeled 0 because the next-day price returned within the threshold band, indicating temporary price spikes.
# Notably, the 2023-03-11 observation captures the USDC depeg during the Silicon Valley Bank crisis, validating that the dataset reflects real events.


dim(usdc_final)
summary(usdc_final$depeg)

# Save dataset
<<<<<<< HEAD
#write_csv(usdc_final, "data/USDC/USDC_model_trimmed_dataset.csv")
#write_csv(usdc, "data/USDC/USDC_model_dataset.csv")

=======
write_csv(usdc_final, "data/USDC/USDC_model_trimmed_dataset.csv")
write_csv(usdc, "data/USDC/USDC_model_dataset.csv")
>>>>>>> 1f046856b5e91ab8570a7fc968bf06b3a9339e06

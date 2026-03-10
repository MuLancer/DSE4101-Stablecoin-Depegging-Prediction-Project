# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# read dataset
pax <- read_csv("data/Pax Dollar/Pax_Dollar_combined.csv")

# inspect columns
names(pax)
str(pax)
# timeOpen is the daily date -> can be used as dataset date
# name looks like a numeric CMC id rather than the coin name, so likely not useful
# marketCap and circulatingSupply again look suspicious at first glance, so check them before deciding anything
# dataset has 2653 daily observations, starting earlier than DAI

summary(pax$marketCap)
summary(pax$circulatingSupply)
sum(pax$marketCap == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
sum(pax$circulatingSupply == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
# seems like only first 12 rows are zeros. this was probbaly before cmc started tracking these vars properly.

# quick inspection of first few rows
head(pax[, c("timeOpen","close","volume","marketCap","circulatingSupply")])

# using timeOpen to create date
pax <- pax %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# quick check
head(pax[, c("date","close","volume","marketCap","circulatingSupply")])

# getting dataset time range now
min(pax$date)
max(pax$date)
## The Pax Dollar dataset covers 2018-09-27 to 2025-12-31

# check for missing values
colSums(is.na(pax)) # none
# check for duplicate days
sum(duplicated(pax$date)) # none
# check for volume = 0
sum(pax$volume == 0) # none

## IMPORTANT: Summary so far ##
# Dataset covers: 2018-09-27 to 2025-12-31
# Total observations: 2653 daily observations
# No missing values across variables
# No duplicate dates
# No zero trading volume observations
# marketCap and circulatingSupply are zero only in early observations

# Rolling 30-day cumulative trading volume used in dynamic threshold construction
pax <- pax %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
pax <- pax %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
         ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)
# Dynamic depegging thresholds following liquidity-adjusted formulation
# Larger trading volume narrows the allowed deviation from the $1 peg

# create next-day prediction variable
pax <- pax %>%
  mutate(next_close = lead(close), .after = close)
# create depegging binary variable
pax <- pax %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)
# Depegging event defined when next-day closing price breaches dynamic threshold band

# quick check for no. of depegging events
table(pax$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(pax$depeg, na.rm = TRUE)
# Depegging events: 232 observations (~8.8% of sample)
# Pax Dollar shows slightly higher event frequency compared to DAI (~7.2%).
# This suggests somewhat larger or more frequent short-term deviations from the $1 peg, though the class imbalance remains moderate and suitable for classification modelling without requiring extreme resampling.

# small improvemnet: remove invalid rows like first 29 rows (no Vmonthly) and last row (no next_close)
pax_final <- pax %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(next_close))

# check final dataset span
min(pax_final$date)
max(pax_final$date)
nrow(pax_final)

## Final analysis dataset after feature construction
## First 29 observations removed due to rolling 30-day volume calculation
## Last observation removed due to missing next-day close price

## Final dataset span: 2018-10-26 to 2025-12-30
## Total observations used for analysis: 2623

# quick verify event counts after trimming dataset
table(pax_final$depeg)
mean(pax_final$depeg)

## Final event statistics after dataset trimming
## Total observations: 2623
## Depegging events: 232
## Non-depegging observations: 2391
## Event frequency: ~8.8%
## This indicates moderate class imbalance (even though lower than Dai) but still sufficient positive cases for ML models.

# quick check for thresholds to make sure they look reasonable
summary(pax_final$ThreshD)
summary(pax_final$ThreshU)
# Dynamic thresholds behave as expected for Pax Dollar.
# The typical band lies roughly between ~0.988 and ~1.012 around the $1 peg.
# Compared to DAI, the thresholds are slightly wider, reflecting lower trading volume and liquidity in Pax markets.
# As intended by the liquidity-adjusted formulation, lower volume allows larger deviations before classifying a depegging event.

# check abs deviation from peg
summary(abs(pax_final$close - 1)) # The median deviation is ~0.001 (≈0.10%), indicating Pax Dollar generally trades very close to its peg.

# final daatset cols check
names(pax_final)
# arrange cols in nice order
pax_final <- pax_final %>%
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

# identify largest deviations of Pax Dollar price from the $1 peg, helps visually verify that major deviations correspond to depegging events
# using close here (instead of next_close) since not tied to the pred horizon and is descriptive
pax_check <- pax_final %>%
  mutate(dev_from_peg = abs(close - 1))
# find largest deviations
pax_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, volume) %>%
  head(10)
# check if these were labelled as depeg
pax_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg) %>%
  head(10)
# Largest deviations occur around known stress periods (e.g., March 2020 market crash).
# Most large deviations correspond to depeg = 1, confirming the threshold rule works.
# Some deviations are labeled 0 because the next-day price returned within the band, meaning they were temporary spikes rather than sustained depegs.

# Largest deviation in dataset, check for one day after
pax_check %>%
  filter(date == "2020-03-12") %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg)
# On 2020-03-12 Pax closed at 1.05 (~4.8% above the peg), the largest deviation in the dataset.
# The next-day price (0.995) fell below the lower threshold (0.996), so this observation is correctly classified as a depegging event (depeg = 1).

# quick verification to see if all largest deviations correspond to actual threshold breaches when using next_close
pax_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg) %>%
  head(15)
# Most large deviations correspond to depeg = 1, confirming the threshold rule works.
# Some cases (e.g. 2020-03-18, 2018-11-04) are labeled 0 because the next-day price returned within the threshold band, indicating temporary price dislocations rather than sustained depegging events.

dim(pax_final)
summary(pax_final$depeg)

# Save dataset
write_csv(pax_final, "data/Pax Dollar/Pax_model_dataset.csv")

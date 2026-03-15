# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# read dataset
dai <- read_csv("data/Dai/Dai_combined.csv")

# inspect columns
names(dai)
str(dai)
# timeOpen is the daily date -> can be used as dataset date
# name is 2781 everywhere, likely CMC id so useless, can be dropped
# marketCap and circulatingSupply look a bit weird, let's check
summary(dai$marketCap)
summary(dai$circulatingSupply)
sum(dai$marketCap == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
sum(dai$circulatingSupply == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
# seems like only 60 rpws are zeros. this was probbaly before cmc started tracking these vars properly..

# quick inspection of first few rows
head(dai[, c("timeOpen","close","volume","marketCap","circulatingSupply")])

# using timeOpen to create date
dai <- dai %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# quick check
head(dai[, c("date","close","volume","marketCap","circulatingSupply")])

# getting dataset time range now
min(dai$date)
max(dai$date)
## The Dai Dataset covers 2019-11-22 to 2025-12-31

# check for missing values
colSums(is.na(dai)) # none
# check for duplicate days
sum(duplicated(dai$date)) # none
# check for volume = 0
sum(dai$volume == 0) # none


## IMPORTANT: Summary so far ##
# Dataset covers: 2019-11-22 to 2025-12-31
# Total observations: 2232 daily observations
# No missing values across variables
# No duplicate dates
# No zero trading volume observations
# marketCap and circulatingSupply are zero only in early observations

# Rolling 30-day cumulative trading volume used in dynamic threshold construction
dai <- dai %>%
  mutate(V_monthly = rollapply(volume,
                          width = 30,
                          FUN = sum,
                          align = "right",
                          fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
dai <- dai %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
    ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)
# Dynamic depegging thresholds following liquidity-adjusted formulation
# Larger trading volume narrows the allowed deviation from the $1 peg

# create next-day prediction variable
dai <- dai %>%
  mutate(next_close = lead(close), .after = close)

# create depegging binary variable
dai <- dai %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)
# Depegging event defined when next-day closing price breaches dynamic threshold band

# quick check for no. of depegging events
table(dai$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(dai$depeg, na.rm = TRUE)

# Depegging events: 159 observations (~7.2% of sample)
# Class imbalance is moderate, making classification feasible without extreme resampling.

# small improvement: remove invalid rows like first 29 rows (no Vmonthly) and last row (no next_close)
dai_final <- dai %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(next_close))

# check final dataset span
min(dai_final$date)
max(dai_final$date)
nrow(dai_final)

## Final analysis dataset after feature construction
## First 29 observations removed due to rolling 30-day volume calculation
## Last observation removed due to missing next-day close price

## Final dataset span: 2019-12-21 to 2025-12-30
## Total observations used for analysis: 2202

# quick verify event counts after trimming dataset
table(dai_final$depeg)
mean(dai_final$depeg)

## Final event statistics after dataset trimming
## Total observations: 2202
## Depegging events: 159
## Non-depegging observations: 2043
## Event frequency: ~7.2%
## This indicates moderate class imbalance but still sufficient positive cases for ML models.

# quick check for thresholds to make sure they look reasonable
summary(dai_final$ThreshD)
summary(dai_final$ThreshU)
# Dynamic thresholds behave as expected.
# Typical band lies roughly between 0.995 and 1.005 (±0.5% around the $1 peg).
# When trading volume is lower, the band widens (up to ~0.974–1.026),
# allowing larger deviations before classifying a depegging event.
# This confirms that the liquidity-adjusted threshold formulation is functioning correctly.

# check abs deviation from peg
summary(abs(dai_final$close - 1))

# final daatset cols check
names(dai_final)
# arrange cols in nice order
dai_final <- dai_final %>%
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
dai_check <- dai_final %>%
  mutate(dev_from_peg = abs(close - 1))
# find largest deviations
dai_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, volume) %>%
  head(10)
# check if these were labelled as depeg
dai_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg) %>%
  head(10)
## These are real historical stress periods for DAI, especially March 2020 (COVID liquidity crisis / Black Thursday in DeFi). So, dataset is behaving realistically.

# Largest deviation in dataset, check for one day after
dai_check %>%
  filter(date == "2020-03-13") %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg)
# large deviation does not always imply a depeg event.
# On 2020-03-13 DAI closed at 1.09 (~9.3% above the $1 peg), representing the largest observed deviation in the dataset. 
# However, the next-day closing price was 0.999, which lies within the dynamic threshold band [0.989, 1.01]. 
# Since the label is defined using next-day price relative to today's thresholds, this observation is correctly classified as depeg = 0. 
# This confirms that the model captures sustained next-day peg breaks rather than temporary one-day price spikes.

# quick verification to see if all largest deviations correspond to actual threshold breaches when using next_close
dai_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg) %>%
  head(15)
# most large deviations from the $1 peg correspond to depeg = 1.
# A few large spikes (e.g. 2020-03-13) are labeled 0 because the next-day price returned within the threshold band. 
# Since labels use next_close relative to today's thresholds, only sustained deviations are classified as depegs.


dim(dai_final)
summary(dai_final$depeg)

# Save dataset
write_csv(dai_final, "data/Dai/Dai_model_trimmed_dataset.csv")
write_csv(dai, "data/Dai/Dai_model_dataset.csv")


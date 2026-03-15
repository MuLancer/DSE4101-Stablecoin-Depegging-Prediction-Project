# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# read dataset
terra <- read_csv("data/Terra Classic/Terra_Classic_combined.csv")

# inspect columns
names(terra)
str(terra)
# timeOpen is the daily date -> can be used as dataset date
# name is 2781 everywhere, likely CMC id so useless, can be dropped
# marketCap and circulatingSupply have first few values =0, let's check
summary(terra$marketCap)
summary(terra$circulatingSupply)
sum(terra$marketCap == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
sum(terra$circulatingSupply == 0, na.rm = TRUE) # quick check for how many zeros and non zeros
# seems like 81 rows are zeros. this was probably before cmc started tracking these vars properly..

# quick inspection of first few rows
head(terra[, c("timeOpen","close","volume","marketCap","circulatingSupply")])
# First few observations show prices well above the $1 level.
# This reflects Terra’s algorithmic stablecoin structure, where early prices were not tightly anchored to the peg during initial market formation.
# marketCap and circulatingSupply is zero for these.

# using timeOpen to create date
terra <- terra %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)
# quick check
head(terra[, c("date","close","volume","marketCap","circulatingSupply")])

# getting dataset time range now
min(terra$date)
max(terra$date)
## The Dai Dataset covers 2019-07-26 to 2025-12-31

# check for missing values
colSums(is.na(terra)) # none
# check for duplicate days
sum(duplicated(terra$date)) # none
# check for volume = 0
sum(terra$volume == 0) # none


## IMPORTANT: Summary so far ##
# Dataset covers: 2019-07-26 to 2025-12-31
# Total observations: 2351 daily observations
# No missing values across variables
# No duplicate dates
# No zero trading volume observations
# marketCap and circulatingSupply are zero in 81 early observations
# This likely reflects incomplete CoinMarketCap coverage during Terra’s initial trading period.
# Since price and volume data remain valid, these rows are retained to avoid unnecessary data loss.

# Restrict Terra rto pre-collapse regime

# Rolling 30-day cumulative trading volume used in dynamic threshold construction
terra <- terra %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)
# set component param
alpha <- 1/3
# Construct dyanmic threshold
terra <- terra %>%
  mutate(ThreshD = 1 - (10 / (V_monthly^alpha)),
         ThreshU = 1 + (10 / (V_monthly^alpha)), .after = V_monthly)
# Dynamic depegging thresholds following liquidity-adjusted formulation
# Larger trading volume narrows the allowed deviation from the $1 peg

# create next-day prediction variable
terra <- terra %>%
  mutate(next_close = lead(close), .after = close)

# create depegging binary variable
terra <- terra %>%
  mutate(depeg = ifelse(next_close <= ThreshD | next_close >= ThreshU, 1, 0),
         .after = next_close)
# Depegging event defined when next-day closing price breaches dynamic threshold band

# quick check for no. of depegging events
table(terra$depeg)
# mean tells event freq. this matters since depegs should be rare events
mean(terra$depeg, na.rm = TRUE)

# Depegging events: 2317 observations (~99.8% of sample).
# Terra shows an extremely high event frequency because prices are far from the $1 peg for most of the sample, especially after the May 2022 collapse of the Terra ecosystem.
# As a result, nearly all observations fall outside the liquidity-adjusted threshold band.
# Our rule assumes stablecoins normally trade near $1. That is true for: DAI, USDC, PAX, USDT
# But Terra Classic (LUNC):completely lost the peg in May 2022, traded far below $1 afterwards (≈ $0.0001)
# So our rule will mark almost every observation as a depeg.
# It reflects the permanent depeg regime.
# perhaps, bets would be to Limit Terra sample to pre-collapse (before May 2022)

# small improvement as usual: remove invalid rows like first 29 rows (no Vmonthly) and last row (no next_close)
terra_final <- terra %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(next_close))

# check final dataset span
min(terra_final$date)
max(terra_final$date)
nrow(terra_final)

## Final analysis dataset after feature construction
## First 29 observations removed due to rolling 30-day volume calculation
## Last observation removed due to missing next-day close price

## Final dataset span: 2019-08-24 to 2025-12-30
## Total observations used for analysis: 2321

# quick verify event counts after trimming dataset
table(terra_final$depeg)
mean(terra_final$depeg)

## Final event statistics after dataset trimming
## Total observations: 2321
## Depegging events: 2317
## Non-depegging observations: 4
## Event frequency: ~99.8%
## This reflects the structural collapse of Terra in May 2022, after which the token permanently traded far from the $1 peg.

# quick check for thresholds to make sure they look reasonable
summary(terra_final$ThreshD)
summary(terra_final$ThreshU)
# Dynamic thresholds appear reasonable for Terra.
# The typical band ranges roughly between 0.99 and 1.01 around the $1 peg, widening slightly when trading volume is lower.

# check abs deviation from peg
summary(abs(terra_final$close - 1))
# Deviations from the $1 peg are extremely large for Terra.
# The median deviation is ~1, indicating that prices are almost always far from $1.
# The maximum deviation exceeds 100, reflecting the extreme price collapse following the Terra ecosystem failure in May 2022.

# final dataset cols check (before we remove post depeg phase)
names(terra_final)
# arrange cols in nice order
terra_final <- terra_final %>%
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
terra_check <- terra_final %>%
  mutate(dev_from_peg = abs(close - 1))
# find largest deviations
terra_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, volume) %>%
  head(10)
# check if these were labelled as depeg
terra_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg) %>%
  head(10)
# All extreme deviations are classified as depegging events.
# Since Terra traded far outside the $1 peg for extended periods,these observations correctly fall outside the dynamic threshold band.
# Those $100+ prices correspond to LUNA before the Terra collapse, which explains why the deviation is >100 from the $1 peg.

# Largest deviation in dataset, check for one day after
terra_check %>%
  filter(date == "2022-04-04") %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg)
# On 2022-04-04 Terra traded around $116, far above the $1 reference peg. The next-day price also remained extremely high

# quick verification to see if all largest deviations correspond to actual threshold breaches when using next_close
terra_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, next_close, ThreshD, ThreshU, depeg) %>%
  head(15)
# All largest deviations are labeled depeg = 1.
# Terra traded far outside the $1 peg during these periods,so the threshold rule consistently flags them as depegging events.

dim(terra_final)
summary(terra_final$depeg)

# Cant save this dataset or use it for modelling since Terra Classic (formerly LUNA) doenst behave like a stablecoin anymore
# write_csv(terra_final, "data/terra/terra_model_dataset.csv")

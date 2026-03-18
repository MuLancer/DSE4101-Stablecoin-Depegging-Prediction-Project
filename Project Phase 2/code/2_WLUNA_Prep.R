# =============================================================================
# WLUNA Data Preparation Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# -----------------------------------------------------------------------------
# 1. READ DATA
# -----------------------------------------------------------------------------

WLUNA <- read_csv("data/WLUNA/WLUNA_combined.csv")

# inspect columns
names(WLUNA)
str(WLUNA)
# timeOpen is the WLUNAly date -> used as dataset date
# name column is CoinMarketCap internal ID, not useful -> will be dropped later
# marketCap and circulatingSupply: check for zeros


summary(WLUNA$marketCap)
summary(WLUNA$circulatingSupply)
sum(WLUNA$marketCap == 0, na.rm = TRUE) 
sum(WLUNA$circulatingSupply == 0, na.rm = TRUE) 
# Early observations (~81 rows) have zero values, likely before CMC began tracking these variables


# quick inspection 
head(WLUNA[, c("timeOpen","close","volume","marketCap","circulatingSupply")])


# -----------------------------------------------------------------------------
# 2. DATE CONSTRUCTION AND BASIC CLEANING
# -----------------------------------------------------------------------------

WLUNA <- WLUNA %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)

# quick check
head(WLUNA[, c("date","close","volume","marketCap","circulatingSupply")])

# dataset time range
min(WLUNA$date)
max(WLUNA$date)
## WLUNA dataset covers: 2019-07-26 to 2025-12-31


# check for missing values, duplicate days, zero volume
colSums(is.na(WLUNA))       # none expected
sum(duplicated(WLUNA$date)) # none expected
sum(WLUNA$volume == 0)      # none expected

# look at price around collapse
WLUNA %>%
  filter(date >= "2022-05-01" & date <= "2022-05-20") %>%
  select(date, close, volume)
# values are vv off

## SUMMARY: Data Quality Check
## Dataset covers: 2019-07-26 to 2025-12-31
## Total observations: 2351 daily observations
## No missing values, no duplicate dates, no zero trading volume
## It was never a stablecoin. Perhaps best to remove from modelling


# -----------------------------------------------------------------------------
# 3. DYNAMIC THRESHOLD CONSTRUCTION
# -----------------------------------------------------------------------------

# Rolling 30-day cumulative trading volume
# Used to construct liquidity-adjWLUNAed dynamic thresholds following Lee, Chiu, and Hsieh (2024)
# Higher trading volume narrows the allowed deviation band around the $1 peg
WLUNA <- WLUNA %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)

# Set alpha parameter
alpha <- 1/3

# Construct dynamic threshold bounds
# ThreshD = 1 - 10 / V_monthly^alpha  (lower bound)
# ThreshU = 1 + 10 / V_monthly^alpha  (upper bound)
WLUNA <- WLUNA %>%
  mutate(
    ThreshD = 1 - (10 / (V_monthly^alpha)),
    ThreshU = 1 + (10 / (V_monthly^alpha)),
    .after = V_monthly
  )


# -----------------------------------------------------------------------------
# 4. MULTI-HORIZON DEPEG LABEL CONSTRUCTION
# -----------------------------------------------------------------------------

## METHODOLOGY NOTE (for report):
## -----------------------------------------------------------------------------
## We construct four binary depeg labels corresponding to forecast horizons of 1, 3, 5, and 7 days ahead. 
## Following Lee, Chiu, and Hsieh (2024), a depegging event is defined as occurring when the lowest closing price (PL) or highest closing price (PH) within the prediction period breaches the dynamic threshold band [ThreshD, ThreshU]. 
## Specifically, depeg = 1 if PL <= ThreshD or PH >= ThreshU, and 0 otherwise. 
## For the 1-day horizon, PL and PH collapse to the single next-day closing price. 
## For 3-, 5-, and 7-day horizons, PL and PH are computed as the rolling minimum and maximum closing price over the respective forward window using lead() and rollapply(). 
## Importantly, PL and PH are used solely to construct the target labels during training and are never included as input features, ensuring no look-ahead bias is introduced into the model.
## At prediction time, the trained model generates a depeg probability using only features observable on the current day.
## -----------------------------------------------------------------------------

WLUNA <- WLUNA %>%
  mutate(
    # --- 1-day horizon ---
    # PL and PH over 1 day = next day's closing price
    depeg_1d = ifelse(lead(close, 1) <= ThreshD | 
                        lead(close, 1) >= ThreshU, 1, 0),
    # --- 3-day horizon ---
    # PL = minimum closing price over next 3 days
    # PH = maximum closing price over next 3 days
    PL_3 = rollapply(lead(close, 1), width = 3, FUN = min, align = "left", fill = NA),
    PH_3 = rollapply(lead(close, 1), width = 3, FUN = max, align = "left", fill = NA),
    depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
    # --- 5-day horizon ---
    PL_5 = rollapply(lead(close, 1), width = 5, FUN = min, align = "left", fill = NA),
    PH_5 = rollapply(lead(close, 1), width = 5, FUN = max, align = "left", fill = NA),
    depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
    # --- 7-day horizon ---
    PL_7 = rollapply(lead(close, 1), width = 7, FUN = min, align = "left", fill = NA),
    PH_7 = rollapply(lead(close, 1), width = 7, FUN = max, align = "left", fill = NA),
    depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)
  )


# -----------------------------------------------------------------------------
# 5. EVENT FREQUENCY CHECK ACROSS HORIZONS
# -----------------------------------------------------------------------------

# depeg event counts and frequencies per horizon
# Expected: event frequency increases with horizon length since longer windows give more opportunities for price to breach threshold
cat("--- Depeg Event Frequency by Horizon ---\n")
cat("1-day:  ", mean(WLUNA$depeg_1d, na.rm = TRUE), "\n") # ~99.8% of sample
cat("3-day:  ", mean(WLUNA$depeg_3d, na.rm = TRUE), "\n") # ~100% of sample
cat("5-day:  ", mean(WLUNA$depeg_5d, na.rm = TRUE), "\n") # ~100% of sample
cat("7-day:  ", mean(WLUNA$depeg_7d, na.rm = TRUE), "\n") # ~100% of sample

# then check pre-collapse period only
cat("--- Pre-Collapse Only (up to 2022-05-08) ---\n")
WLUNA %>%
  filter(date <= "2022-05-08") %>%
  summarise(
    rate_1d = mean(depeg_1d, na.rm=TRUE), # ~99.6% of sample
    rate_3d = mean(depeg_3d, na.rm=TRUE), # ~100% of sample
    rate_5d = mean(depeg_5d, na.rm=TRUE), # ~100% of sample
    rate_7d = mean(depeg_7d, na.rm=TRUE), # ~100% of sample
    n = n()
  )

# We decide to not use this dataset since it never was a stablecoin but was only tied to UST in its relationship
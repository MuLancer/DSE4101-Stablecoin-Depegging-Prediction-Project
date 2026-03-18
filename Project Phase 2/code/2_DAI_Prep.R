# =============================================================================
# DAI Data Preparation Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# -----------------------------------------------------------------------------
# 1. READ DATA
# -----------------------------------------------------------------------------

dai <- read_csv("data/DAI/Dai_combined.csv")

# inspect columns
names(dai)
str(dai)
# timeOpen is the daily date -> used as dataset date
# name column is CoinMarketCap internal ID, not useful -> will be dropped later
# marketCap and circulatingSupply: check for zeros


summary(dai$marketCap)
summary(dai$circulatingSupply)
sum(dai$marketCap == 0, na.rm = TRUE) 
sum(dai$circulatingSupply == 0, na.rm = TRUE) 
# Early observations (~60 rows) have zero values, likely before CMC began tracking these variables

# quick inspection 
head(dai[, c("timeOpen","close","volume","marketCap","circulatingSupply")])


# -----------------------------------------------------------------------------
# 2. DATE CONSTRUCTION AND BASIC CLEANING
# -----------------------------------------------------------------------------

dai <- dai %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)

# quick check
head(dai[, c("date","close","volume","marketCap","circulatingSupply")])

# dataset time range
min(dai$date)
max(dai$date)
## DAI dataset covers: 2019-11-22 to 2025-12-31


# check for missing values, duplicate days, zero volume
colSums(is.na(dai))       # none expected
sum(duplicated(dai$date)) # none expected
sum(dai$volume == 0)      # none expected

## SUMMARY: Data Quality Check
## Dataset covers: 2019-11-22 to 2025-12-31
## Total observations: 2232 daily observations
## No missing values, no duplicate dates, no zero trading volume
## marketCap and circulatingSupply zero only in earliest observations (pre-CMC tracking)


# -----------------------------------------------------------------------------
# 3. DYNAMIC THRESHOLD CONSTRUCTION
# -----------------------------------------------------------------------------

# Rolling 30-day cumulative trading volume
# Used to construct liquidity-adjusted dynamic thresholds following Lee, Chiu, and Hsieh (2024)
# Higher trading volume narrows the allowed deviation band around the $1 peg
dai <- dai %>%
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
dai <- dai %>%
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

dai <- dai %>%
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
cat("1-day:  ", mean(dai$depeg_1d, na.rm = TRUE), "\n") # ~7.2% of sample
cat("3-day:  ", mean(dai$depeg_3d, na.rm = TRUE), "\n") # ~12.2% of sample
cat("5-day:  ", mean(dai$depeg_5d, na.rm = TRUE), "\n") # ~14.6% of sample
cat("7-day:  ", mean(dai$depeg_7d, na.rm = TRUE), "\n") # ~16.1% of sample

table(dai$depeg_1d) # 159 depegs
table(dai$depeg_3d) # 268 depegs
table(dai$depeg_5d) # 321 depegs
table(dai$depeg_7d) # 354 depegs


# -----------------------------------------------------------------------------
# 6. TRIM DATASET
# -----------------------------------------------------------------------------

# Remove first 29 rows: no V_monthly due to 30-day rolling window
# Remove last 7 rows: no forward prices available for 7-day label construction
dai_final <- dai %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(depeg_7d))   # most restrictive horizon, covers all shorter ones too

# verify final dataset span
min(dai_final$date)
max(dai_final$date)
nrow(dai_final)

# verify event counts after trimming
cat("--- Post-Trim Event Frequency ---\n")
cat("1-day:  ", mean(dai_final$depeg_1d, na.rm = TRUE), "\n")
cat("3-day:  ", mean(dai_final$depeg_3d, na.rm = TRUE), "\n")
cat("5-day:  ", mean(dai_final$depeg_5d, na.rm = TRUE), "\n")
cat("7-day:  ", mean(dai_final$depeg_7d, na.rm = TRUE), "\n")

# Final trimmed dataset: 2196 observations (2019-12-21 to 2025-12-24)
# First 29 rows removed: insufficient history for 30-day rolling volume (V_monthly)
# Last 7 rows removed: insufficient forward prices for 7-day label construction

# Post-trim event frequencies:
# 1-day:  7.24%  -> severe class imbalance, SMOTE will be important
# 3-day:  12.20% -> moderate imbalance
# 5-day:  14.62% -> moderate imbalance
# 7-day:  16.12% -> most balanced, but longest horizon


# -----------------------------------------------------------------------------
# 7. THRESHOLD SANITY CHECK
# -----------------------------------------------------------------------------

summary(dai_final$ThreshD)
summary(dai_final$ThreshU)
# Dynamic thresholds should show typical band of roughly ±0.5% around $1 peg
# Lower volume periods widen the band, higher volume periods narrow it
# This is economically intuitive: thin markets are more volatile, so wider tolerance is warranted.


# -----------------------------------------------------------------------------
# 8. COLUMN SELECTION AND ORDERING
# -----------------------------------------------------------------------------
dai_final <- dai_final %>%
  select(
    date,
    open, high, low, close,
    depeg_1d, depeg_3d, depeg_5d, depeg_7d,
    PL_3, PH_3,
    PL_5, PH_5,
    PL_7, PH_7,
    volume,
    V_monthly,
    ThreshD, ThreshU,
    marketCap,
    circulatingSupply
  )
dai <- dai %>%
  select(
    date,
    open, high, low, close,
    depeg_1d, depeg_3d, depeg_5d, depeg_7d,
    PL_3, PH_3,
    PL_5, PH_5,
    PL_7, PH_7,
    volume,
    V_monthly,
    ThreshD, ThreshU,
    marketCap,
    circulatingSupply
  )


# -----------------------------------------------------------------------------
# 9. LARGEST DEVIATION CHECK (DESCRIPTIVE VERIFICATION)
# -----------------------------------------------------------------------------

# Identify largest historical deviations from the $1 peg for sanity checking
# Using close (not forward prices) since this is purely descriptive
dai_check <- dai_final %>%
  mutate(dev_from_peg = abs(close - 1))

dai_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg_1d, depeg_7d) %>%
  head(10)

# Large deviations should correspond to known historical stress periods (e.g. March 2020 COVID crash)
# Note: a large close deviation on day T does not guarantee depeg_1d = 1, since the label is based on forward prices, not current price


# -----------------------------------------------------------------------------
# 10. SAVE DATASETS
# -----------------------------------------------------------------------------

dim(dai_final)

write_csv(dai_final, "data/DAI/DAI_short_dataset.csv")
write_csv(dai, "data/DAI/DAI_full_dataset.csv")

cat("DAI data preparation complete.\n")
cat("Final dataset dimensions:", nrow(dai_final), "rows x", ncol(dai_final), "cols\n")


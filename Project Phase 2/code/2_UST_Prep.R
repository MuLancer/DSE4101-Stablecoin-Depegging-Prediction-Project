# =============================================================================
# UST Data Preparation Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# -----------------------------------------------------------------------------
# 1. READ DATA
# -----------------------------------------------------------------------------

UST <- read_csv("data/UST/UST_combined.csv")

# inspect columns
names(UST)
str(UST)
# timeOpen is the USTly date -> used as dataset date
# name column is CoinMarketCap internal ID, not useful -> will be dropped later
# marketCap and circulatingSupply: check for zeros


summary(UST$marketCap)
summary(UST$circulatingSupply)
sum(UST$marketCap == 0, na.rm = TRUE) 
sum(UST$circulatingSupply == 0, na.rm = TRUE) 

# quick inspection 
head(UST[, c("timeOpen","close","volume","marketCap","circulatingSupply")])


# -----------------------------------------------------------------------------
# 2. DATE CONSTRUCTION AND BASIC CLEANING
# -----------------------------------------------------------------------------

UST <- UST %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)

# quick check
head(UST[, c("date","close","volume","marketCap","circulatingSupply")])

# dataset time range
min(UST$date)
max(UST$date)
## UST dataset covers: 2020-11-25 to 2025-12-31


# check for missing values, duplicate days, zero volume
colSums(is.na(UST))       # none expected
sum(duplicated(UST$date)) # none expected
sum(UST$volume == 0)      # none expected

# Further checks since the coin lost its depeg completely and forever in May 2022
summary(UST$close)
summary(UST$volume)

# look at price around collapse
UST %>%
  filter(date >= "2022-05-01" & date <= "2022-05-20") %>%
  select(date, close, volume)

## SUMMARY: Data Quality Check
## Dataset covers: 2020-11-25 to 2025-12-31
## Total observations: 1863 USTly observations
## No missing values, no duplicate dates, no zero trading volume
## After the coin collapsed in MAy 2022, it never recovered


# -----------------------------------------------------------------------------
# 3. DYNAMIC THRESHOLD CONSTRUCTION
# -----------------------------------------------------------------------------

# Rolling 30-day cumulative trading volume
# Used to construct liquidity-adjusted dynamic thresholds following Lee, Chiu, and Hsieh (2024)
# Higher trading volume narrows the allowed deviation band around the $1 peg
UST <- UST %>%
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
UST <- UST %>%
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

UST <- UST %>%
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
cat("1-day:  ", mean(UST$depeg_1d, na.rm = TRUE), "\n") # ~74.2% of sample
cat("3-day:  ", mean(UST$depeg_3d, na.rm = TRUE), "\n") # ~75.6% of sample
cat("5-day:  ", mean(UST$depeg_5d, na.rm = TRUE), "\n") # ~76.8% of sample
cat("7-day:  ", mean(UST$depeg_7d, na.rm = TRUE), "\n") # ~77.8% of sample

table(UST$depeg_1d) # 1361 depegs
table(UST$depeg_3d) # 1384 depegs
table(UST$depeg_5d) # 1404 depegs
table(UST$depeg_7d) # 1422 depegs

# then check pre-collapse period only
cat("--- Pre-Collapse Only (up to 2022-05-08) ---\n")
UST %>%
  filter(date <= "2022-05-08") %>%
  summarise(
    rate_1d = mean(depeg_1d, na.rm=TRUE), # ~5.8% of sample
    rate_3d = mean(depeg_3d, na.rm=TRUE), # ~10.8% of sample
    rate_5d = mean(depeg_5d, na.rm=TRUE), # ~15.2% of sample
    rate_7d = mean(depeg_7d, na.rm=TRUE), # ~19.2% of sample
    n = n()
  )

# Truncate at last pre-collapse day
# Post-May 8 2022, UST permanently lost its peg and never recovered
# Retaining post-collapse data would inflate depeg rates to ~74% and cause models to learn collapse dynamics rather than early warning signals
UST <- UST %>% filter(date <= "2022-05-08")

# verify
nrow(UST) # 530
min(UST$date) # starts 202-11-25
max(UST$date) # ends 2022-05-08

# Dataset truncated at 2022-05-08 (last day UST traded near $1 peg)
# Full dataset ran 2020-11-25 to 2025-12-31 (1833 obs) but post-collapse data showed ~74% depeg rate, driven by UST's permanent collapse from May 9 2022 onward.
# Post-collapse UST traded at $0.01-0.02, never recovering its peg.
# Retaining this data would cause models to learn collapse dynamics, not early warnings.
# Truncated dataset: 530 observations (2020-11-25 to 2022-05-08)

# Post-truncation event frequencies:
# 1-day:  5.79%  -> severe class imbalance, SMOTE will be important
# 3-day:  10.78% -> moderate imbalance
# 5-day:  15.17% -> moderate imbalance
# 7-day:  19.16% -> most balanced, but longest horizon


# -----------------------------------------------------------------------------
# 6. TRIM DATASET
# -----------------------------------------------------------------------------

# Remove first 29 rows: no V_monthly due to 30-day rolling window
# Already removed last 7 rows where no forward prices were available for 7-day label construction
UST_final <- UST %>%
  filter(!is.na(V_monthly)) 

# verify final dataset span
min(UST_final$date)
max(UST_final$date)
nrow(UST_final)

# verify event counts after trimming
cat("--- Post-Trim Event Frequency ---\n")
cat("1-day:  ", mean(UST_final$depeg_1d, na.rm = TRUE), "\n")
cat("3-day:  ", mean(UST_final$depeg_3d, na.rm = TRUE), "\n")
cat("5-day:  ", mean(UST_final$depeg_5d, na.rm = TRUE), "\n")
cat("7-day:  ", mean(UST_final$depeg_7d, na.rm = TRUE), "\n")

# Final trimmed dataset: 501 observations (2020-12-24 to 2022-05-08)
# First 29 rows removed: insufficient history for 30-day rolling volume (V_monthly)

# Post-trim event frequencies:
# 1-day:  5.79%  -> severe class imbalance, SMOTE will be important
# 3-day:  10.78% -> moderate imbalance
# 5-day:  15.17% -> moderate imbalance
# 7-day:  19.16% -> most balanced, but longest horizon


# -----------------------------------------------------------------------------
# 7. THRESHOLD SANITY CHECK
# -----------------------------------------------------------------------------

summary(UST_final$ThreshD)
summary(UST_final$ThreshU)
# Dynamic thresholds should show typical band of roughly ±0.5% around $1 peg
# Lower volume periods widen the band, higher volume periods narrow it
# This is economically intuitive: thin markets are more volatile, so wider tolerance is warranted.


# -----------------------------------------------------------------------------
# 8. COLUMN SELECTION AND ORDERING
# -----------------------------------------------------------------------------
UST_final <- UST_final %>%
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
UST <- UST %>%
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


  #   --------------------------------------------------------------------------
  # 9. LARGEST DEVIATION CHECK (DESCRIPTIVE VERIFICATION)
  # -----------------------------------------------------------------------------

# Identify largest historical deviations from the $1 peg for sanity checking
# Using close (not forward prices) since this is purely descriptive
UST_check <- UST_final %>%
  mutate(dev_from_peg = abs(close - 1))

UST_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg_1d, depeg_7d) %>%
  head(10)

# Large deviations correspond to known stress periods (e.g. Jan-Feb 2021 crypto volatility, May 2021 market selloff).
# Note: a large close deviation on day T does not guarantee depeg_1d = 1, since the label is based on forward prices, not current price.
# e.g. 2020-12-30 shows 14.9% deviation but depeg_1d = 0, price recovered within threshold by next day.

# -----------------------------------------------------------------------------
# 10. SAVE DATASETS
# -----------------------------------------------------------------------------

dim(UST_final)

write_csv(UST_final, "data/UST/UST_short_dataset.csv")
write_csv(UST, "data/UST/UST_full_dataset.csv")

cat("UST data preparation complete.\n")
cat("Final dataset dimensions:", nrow(UST_final), "rows x", ncol(UST_final), "cols\n")













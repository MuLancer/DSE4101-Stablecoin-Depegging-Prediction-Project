# =============================================================================
# USDT Data Preparation Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(zoo)

# -----------------------------------------------------------------------------
# 1. READ DATA
# -----------------------------------------------------------------------------

USDT <- read_csv("data/USDT/USDT_combined.csv")

# inspect columns
names(USDT)
str(USDT)
# timeOpen is the USDTly date -> used as dataset date
# name column is CoinMarketCap internal ID, not useful -> will be dropped later
# marketCap and circulatingSupply: check for zeros


summary(USDT$marketCap)
summary(USDT$circulatingSupply)
sum(USDT$marketCap == 0, na.rm = TRUE) 
sum(USDT$circulatingSupply == 0, na.rm = TRUE) 
# only 1 circulatingSupply obs=0, possibly on start day of coin

# quick inspection 
head(USDT[, c("timeOpen","close","volume","marketCap","circulatingSupply")])


# -----------------------------------------------------------------------------
# 2. DATE CONSTRUCTION AND BASIC CLEANING
# -----------------------------------------------------------------------------

USDT <- USDT %>%
  mutate(date = as.Date(timeOpen), .before = timeOpen) %>%
  arrange(date)

# quick check
head(USDT[, c("date","close","volume","marketCap","circulatingSupply")])

# dataset time range
min(USDT$date)
max(USDT$date)
## USDT dataset covers: 2017-11-27 to 2025-12-31


# check for missing values, duplicate days, zero volume
colSums(is.na(USDT))       # none expected
sum(duplicated(USDT$date)) # none expected
sum(USDT$volume == 0)      # none expected

## SUMMARY: Data Quality Check
## Dataset covers: 2017-11-27 to 2025-12-31
## Total observations: 2957 daily observations
## No missing values, no duplicate dates, no zero trading volume
## circulatingSupply zero only in one observation


# -----------------------------------------------------------------------------
# 3. DYNAMIC THRESHOLD CONSTRUCTION
# -----------------------------------------------------------------------------

# Rolling 30-day cumulative trading volume
# Used to construct liquidity-adjusted dynamic thresholds following Lee, Chiu, and Hsieh (2024)
# Higher trading volume narrows the allowed deviation band around the $1 peg
USDT <- USDT %>%
  mutate(V_monthly = rollapply(volume,
                               width = 30,
                               FUN = sum,
                               align = "right",
                               fill = NA), .after = volume)

# USDT: Fixed threshold applied (ThreshD = 0.995, ThreshU = 1.005)
# The dynamic threshold formula from Lee et al. (2024) is volume-sensitive by design: ThreshD = 1 - 10 / V_monthly^(1/3), ThreshU = 1 + 10 / V_monthly^(1/3)
# USDT's median 30-day cumulative volume (~1.47 trillion USD) is several orders of magnitude larger than other coins in the sample (e.g. DAI ~6 billion), causing the dynamic band to collapse to an economically implausible ±0.09% around the $1 peg.
# This results in depeg rates exceeding 35%, driven by normal daily price noise rather than genuine peg instability. 
# A fixed ±0.5% threshold is therefore applied, consistent with thresholds commonly used in stablecoin literature. 
# This yields a 1-day depeg rate of ~11.3%, which is slightly elevated relative to DAI (~7.2%) and PAX (~8.9%) but economically plausible given USDT's well-documented history of reserve controversies and heightened sensitivity to broader crypto market stress events.
# The fixed threshold is retained consistently across all four forecast horizons.
USDT <- USDT %>%
  mutate(
    ThreshD = 0.995,
    ThreshU = 1.005,
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

USDT <- USDT %>%
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
cat("1-day:  ", mean(USDT$depeg_1d, na.rm = TRUE), "\n") # ~11.3% of sample
cat("3-day:  ", mean(USDT$depeg_3d, na.rm = TRUE), "\n") # ~18.3% of sample
cat("5-day:  ", mean(USDT$depeg_5d, na.rm = TRUE), "\n") # ~22.1% of sample
cat("7-day:  ", mean(USDT$depeg_7d, na.rm = TRUE), "\n") # ~24.8% of sample

table(USDT$depeg_1d) # 333 depegs
table(USDT$depeg_3d) # 540 depegs
table(USDT$depeg_5d) # 653 depegs
table(USDT$depeg_7d) # 731 depegs


# -----------------------------------------------------------------------------
# 6. TRIM DATASET
# -----------------------------------------------------------------------------

# Remove first 29 rows: no V_monthly due to 30-day rolling window
# Remove last 7 rows: no forward prices available for 7-day label construction
USDT_final <- USDT %>%
  filter(!is.na(V_monthly)) %>%
  filter(!is.na(depeg_7d))   # most restrictive horizon, covers all shorter ones too

# verify final dataset span
min(USDT_final$date)
max(USDT_final$date)
nrow(USDT_final)

# verify event counts after trimming
cat("--- Post-Trim Event Frequency ---\n")
cat("1-day:  ", mean(USDT_final$depeg_1d, na.rm = TRUE), "\n")
cat("3-day:  ", mean(USDT_final$depeg_3d, na.rm = TRUE), "\n")
cat("5-day:  ", mean(USDT_final$depeg_5d, na.rm = TRUE), "\n")
cat("7-day:  ", mean(USDT_final$depeg_7d, na.rm = TRUE), "\n")

# Final trimmed dataset: 2921 observations (2017-12-26 to 2025-12-24)
# First 29 rows removed: insufficient history for 30-day rolling volume (V_monthly)
# Last 7 rows removed: insufficient forward prices for 7-day label construction

# Post-trim event frequencies:
# 1-day:  10.65% -> moderate-severe class imbalance, SMOTE will be important
# 3-day:  17.56% -> moderate imbalance
# 5-day:  21.36% -> moderate imbalance
# 7-day:  24.03% -> most balanced, but longest horizon


# -----------------------------------------------------------------------------
# 7. THRESHOLD SANITY CHECK
# -----------------------------------------------------------------------------

summary(USDT_final$ThreshD)
summary(USDT_final$ThreshU)
# Fixed threshold: ThreshD = 0.995, ThreshU = 1.005 throughout
# Represents a ±0.5% band around the $1 peg, consistent with stablecoin literature
# Unlike DAI/PAX/USDC, band does not vary with volume by design


# -----------------------------------------------------------------------------
# 8. COLUMN SELECTION AND ORDERING
# -----------------------------------------------------------------------------
USDT_final <- USDT_final %>%
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
USDT <- USDT %>%
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
USDT_check <- USDT_final %>%
  mutate(dev_from_peg = abs(close - 1))

USDT_check %>%
  arrange(desc(dev_from_peg)) %>%
  select(date, close, dev_from_peg, ThreshD, ThreshU, depeg_1d, depeg_7d) %>%
  head(10)

# Largest deviations confirm threshold captures genuine stress events, not routine noise.
# Top deviations cluster around COVID Black Thursday (Mar 2020) and the 2018 bear market, both well-documented periods of crypto market stress and USDT reserve scrutiny.


# -----------------------------------------------------------------------------
# 10. SAVE DATASETS
# -----------------------------------------------------------------------------

dim(USDT_final)

write_csv(USDT_final, "data/USDT/USDT_short_dataset.csv")
write_csv(USDT, "data/USDT/USDT_full_dataset.csv")

cat("USDT data preparation complete.\n")
cat("Final dataset dimensions:", nrow(USDT_final), "rows x", ncol(USDT_final), "cols\n")




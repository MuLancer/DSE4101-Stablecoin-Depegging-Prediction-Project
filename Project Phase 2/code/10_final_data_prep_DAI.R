# =============================================================================
# DAI Stablecoin - Data Loading, Inspection & Cleaning
# Objective: Predict early warning signs of depegging (binary classification)
# setwd("~/DSE4101/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)   
library(lubridate)   
library(naniar)      # NA visualisation (optional but useful)

# --- Load Dataset ------------------------------------------------------------
df <- read_csv("data/DAI/DAI_onchain_features.csv")

# --- Initial Inspection ------------------------------------------------------
dim(df)           # rows x cols
glimpse(df)       # col types + first values
head(df, 5)

# Date range
range(df$date)

# Depeg label distribution (before cleaning)
df %>%
  count(depeg_1d) %>% print()

df %>%
  count(depeg_3d) %>% print()

# --- NA Overview -------------------------------------------------------------
# Total NAs per column (only show cols with at least 1 NA)
na_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  arrange(desc(na_count))

print(na_summary, n = Inf)

# --- Notes on NAs ------------------------------------------------------------
# close_ust      : ~1702 NAs — UST only existed ~2021-2022; drop this column
# depeg_Xd cols  : ~30-36 NAs at the tail — future labels that can't be
#                  computed; drop these rows
# volatility_Xd,
# ma_Xd, rsi_14,
# bb_pctb, etc.  : warm-up NAs at the start of the series due to rolling
#                  window calculations; forward-fill these
# First row      : after ffill, row 0 still has NAs in 30d rolling cols
#                  (no prior row to fill from); drop this single row
# Total data loss: ~37 rows out of 2232 (~1.7%) — acceptable

# --- Cleaning ----------------------------------------------------------------

df_clean <- df %>%
  
  # 1. Convert date column to Date type
  mutate(date = as.Date(date)) %>%
  
  # 2. Drop close_ust (mostly missing, UST-specific cross-coin feature)
  select(-close_ust) %>%
  
  # 3. Drop rows where depeg labels are NA (tail of series, unlabelable)
  drop_na(depeg_1d, depeg_3d, depeg_5d, depeg_7d) %>%
  
  # 4. Sort by date (important before forward-fill)
  arrange(date) %>%
  
  # 5. Forward-fill remaining warm-up NAs (rolling window artifacts)
  fill(everything(), .direction = "down") %>%
  
  # 6. Drop the very first row (30d rolling cols still NA, nothing to fill from)
  drop_na()

# --- Post-Cleaning Checks ----------------------------------------------------
dim(df_clean)
range(df_clean$date)

# Confirm zero NAs remain
sum(is.na(df_clean))

# Depeg label distribution after cleaning
df_clean %>% count(depeg_1d)
df_clean %>% count(depeg_3d)
df_clean %>% count(depeg_5d)
df_clean %>% count(depeg_7d)

# --- Train / Test Splits -----------------------------------------------------

# Period 1: captures May 2022 UST crash + DAI stress event
# Train: Dec 2019 - Dec 2021
# Test:  Jan 2022 - Dec 2022

train_p1 <- df_clean %>% filter(date <= as.Date("2021-12-31"))
test_p1  <- df_clean %>% filter(date >= as.Date("2022-01-01") &
                                  date <= as.Date("2022-12-31"))

cat("Period 1 - Train:", nrow(train_p1), "rows |",
    as.character(min(train_p1$date)), "to", as.character(max(train_p1$date)), "\n")
cat("Period 1 - Test: ", nrow(test_p1),  "rows |",
    as.character(min(test_p1$date)),  "to", as.character(max(test_p1$date)),  "\n")

# Period 2: post-crash longer horizon (DAI is fiat-backed/crypto-backed)
# Train: Dec 2019 - Dec 2023
# Test:  Jan 2024 - Dec 2025

train_p2 <- df_clean %>% filter(date <= as.Date("2023-12-31"))
test_p2  <- df_clean %>% filter(date >= as.Date("2024-01-01"))

cat("Period 2 - Train:", nrow(train_p2), "rows |",
    as.character(min(train_p2$date)), "to", as.character(max(train_p2$date)), "\n")
cat("Period 2 - Test: ", nrow(test_p2),  "rows |",
    as.character(min(test_p2$date)),  "to", as.character(max(test_p2$date)),  "\n")

# --- Save Cleaned Splits -----------------------------------------------------

write_csv(df_clean,  "data/DAI/DAI_final_clean.csv")
write_csv(train_p1,  "data/DAI/DAI_train_period1.csv")
write_csv(test_p1,   "data/DAI/DAI_test_period1.csv")
write_csv(train_p2,  "data/DAI/DAI_train_period2.csv")
write_csv(test_p2,   "data/DAI/DAI_test_period2.csv")

cat("All files saved to data/DAI/\n")

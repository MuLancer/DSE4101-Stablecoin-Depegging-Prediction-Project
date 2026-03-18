# =============================================================================
# Price and Technical Indicator Feature Engineering Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(dplyr)
library(zoo)
library(TTR)
library(tidyr)
library(readr)

# -----------------------------------------------------------------------------
# 1. READ DATASETS
# -----------------------------------------------------------------------------

DAI   <- read_csv("data/DAI/DAI_full_dataset.csv")
PAX   <- read_csv("data/PAX/PAX_full_dataset.csv")
USDC  <- read_csv("data/USDC/USDC_full_dataset.csv")
USDT  <- read_csv("data/USDT/USDT_full_dataset.csv")
UST   <- read_csv("data/UST/UST_full_dataset.csv")

# -----------------------------------------------------------------------------
# 2. SELECT RELEVANT COLUMNS
# -----------------------------------------------------------------------------

# Retain OHLC prices, depeg labels across all horizons, liquidity measures,
# dynamic thresholds, market cap, circulating supply, and WLUNA contagion feature
keep_cols <- c(
  "date", "open", "high", "low", "close",
  "depeg_1d", "depeg_3d", "depeg_5d", "depeg_7d",
  "volume", "V_monthly", "ThreshD", "ThreshU",
  "marketCap", "circulatingSupply", "WLUNA_close"
)

keep_stablecoin_cols <- function(df) {
  df[, keep_cols, drop = FALSE]
}

DAI   <- keep_stablecoin_cols(DAI)
PAX   <- keep_stablecoin_cols(PAX)
USDC  <- keep_stablecoin_cols(USDC)
USDT  <- keep_stablecoin_cols(USDT)
UST   <- keep_stablecoin_cols(UST)

# -----------------------------------------------------------------------------
# 3. ENSURE DATE FORMAT AND SORT
# -----------------------------------------------------------------------------

DAI$date   <- as.Date(DAI$date)
PAX$date   <- as.Date(PAX$date)
USDC$date  <- as.Date(USDC$date)
USDT$date  <- as.Date(USDT$date)
UST$date   <- as.Date(UST$date)

DAI   <- DAI[order(DAI$date), ]
PAX   <- PAX[order(PAX$date), ]
USDC  <- USDC[order(USDC$date), ]
USDT  <- USDT[order(USDT$date), ]
UST   <- UST[order(UST$date), ]

# -----------------------------------------------------------------------------
# 4. BASIC PRICE INDICATORS
# -----------------------------------------------------------------------------

# Log returns capture proportional price changes
# Lagged prices capture short and medium term momentum
# Absolute peg deviation measures distance from $1
# Intraday range captures daily price stress
add_basic_indicators <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      log_return         = log(close / lag(close, 1)),
      close_lag1d        = lag(close, 1),
      close_lag7d        = lag(close, 7),
      abs_peg_deviation  = abs(close - 1),
      intraday_range     = (high - low) / close
    )
}

DAI   <- add_basic_indicators(DAI)
PAX   <- add_basic_indicators(PAX)
USDC  <- add_basic_indicators(USDC)
USDT  <- add_basic_indicators(USDT)
UST   <- add_basic_indicators(UST)

# -----------------------------------------------------------------------------
# 5. ROLLING VOLATILITY
# -----------------------------------------------------------------------------

# Rolling standard deviation of log returns across three windows
# Captures short, medium, and long term volatility buildup prior to depegging
add_volatility <- function(df) {
  df %>%
    mutate(
      volatility_3d  = rollapply(log_return, 3,  sd, fill = NA, align = "right"),
      volatility_7d  = rollapply(log_return, 7,  sd, fill = NA, align = "right"),
      volatility_30d = rollapply(log_return, 30, sd, fill = NA, align = "right")
    )
}

DAI   <- add_volatility(DAI)
PAX   <- add_volatility(PAX)
USDC  <- add_volatility(USDC)
USDT  <- add_volatility(USDT)
UST   <- add_volatility(UST)

# -----------------------------------------------------------------------------
# 6. TREND AND BAND INDICATORS
# -----------------------------------------------------------------------------

# Moving averages capture trend direction
# EMA gives more weight to recent observations
# RSI identifies overbought/oversold conditions
# Bollinger bands capture price deviation relative to recent volatility
# bb_pctb: position of price within the Bollinger band (0 = lower, 1 = upper)
# dist_from_lower_band: how far price is from the lower Bollinger band
# Helper columns (bb_mid, bb_sd, bb_upper, bb_lower) dropped after construction
add_trend_band_indicators <- function(df) {
  df %>%
    mutate(
      ma_7d                = zoo::rollmean(close, 7,  fill = NA, align = "right"),
      ma_30d               = zoo::rollmean(close, 30, fill = NA, align = "right"),
      dist_from_ma_7d      = (close - ma_7d) / ma_7d,
      ema_7d               = TTR::EMA(close, n = 7),
      rsi_14               = TTR::RSI(close, n = 14),
      bb_mid               = TTR::SMA(close, n = 20),
      bb_sd                = zoo::rollapply(close, 20, sd, fill = NA, align = "right"),
      bb_upper             = bb_mid + 2 * bb_sd,
      bb_lower             = bb_mid - 2 * bb_sd,
      bb_pctb              = (close - bb_lower) / (bb_upper - bb_lower),
      dist_from_lower_band = (close - bb_lower) / bb_lower
    )
}

DAI   <- add_trend_band_indicators(DAI)
PAX   <- add_trend_band_indicators(PAX)
USDC  <- add_trend_band_indicators(USDC)
USDT  <- add_trend_band_indicators(USDT)
UST   <- add_trend_band_indicators(UST)

# Drop Bollinger band helper columns after construction
drop_helper_cols <- function(df) {
  df %>% select(-bb_mid, -bb_sd, -bb_upper, -bb_lower)
}

DAI   <- drop_helper_cols(DAI)
PAX   <- drop_helper_cols(PAX)
USDC  <- drop_helper_cols(USDC)
USDT  <- drop_helper_cols(USDT)
UST   <- drop_helper_cols(UST)

# -----------------------------------------------------------------------------
# 7. MISSING VALUE CHECK AND CLEANING
# -----------------------------------------------------------------------------

# Rolling window calculations introduce NAs in early rows
# drop_na() removes all rows with any NA across all feature columns
# This ensures a fully balanced feature matrix for downstream modelling
colSums(is.na(DAI))
colSums(is.na(UST))   # UST may have fewer NAs due to shorter sample

clean_rows <- function(df) {
  df %>% drop_na()
}

DAI   <- clean_rows(DAI)
PAX   <- clean_rows(PAX)
USDC  <- clean_rows(USDC)
USDT  <- clean_rows(USDT)
UST   <- clean_rows(UST)

# ~31 rows dropped per coin due to rolling window initialisation
# UST may differ due to shorter sample and truncation at collapse date

# -----------------------------------------------------------------------------
# 8. DATASET SPAN CHECK
# -----------------------------------------------------------------------------

get_date_range <- function(df, coin_name) {
  data.frame(
    coin     = coin_name,
    min_date = min(df$date, na.rm = TRUE),
    max_date = max(df$date, na.rm = TRUE),
    n_obs    = nrow(df)
  )
}

date_ranges <- dplyr::bind_rows(
  get_date_range(DAI,   "DAI"),
  get_date_range(PAX,   "PAX"),
  get_date_range(USDC,  "USDC"),
  get_date_range(USDT,  "USDT"),
  get_date_range(UST,   "UST")
)

date_ranges
# Stablecoin datasets cover different time spans depending on when each coin was introduced and whether it survived the sample period.
# USDT has the longest history (since 2017), while UST has the shortest sample ending in May 2022 due to its permanent collapse.
# DAI, PAX, and USDC begin around 2018-2019 and run until end of 2025.
# WLUNA begins in July 2019 and runs until end of 2025.
# Models are trained separately per coin to account for differences in sample size and historical market conditions.

# -----------------------------------------------------------------------------
# 9. SAVE DATASETS
# -----------------------------------------------------------------------------

write_csv(DAI,   "data/DAI/DAI_technical_features.csv")
write_csv(PAX,   "data/PAX/PAX_technical_features.csv")
write_csv(USDC,  "data/USDC/USDC_technical_features.csv")
write_csv(USDT,  "data/USDT/USDT_technical_features.csv")
write_csv(UST,   "data/UST/UST_technical_features.csv")

cat("Technical feature datasets saved.\n")

# =============================================================================
# FEATURE SUMMARY
# =============================================================================
#
# This dataset contains daily OHLC price data and engineered technical
# indicators used to capture price momentum, volatility buildup,
# peg deviations, and trading range dynamics prior to potential
# stablecoin depegging events.
#
# Features constructed:
# - log_return:             daily log return
# - close_lag1d/7d:         lagged closing prices (momentum)
# - abs_peg_deviation:      absolute distance from $1 peg
# - intraday_range:         (high - low) / close, daily price stress
# - volatility_3d/7d/30d:   rolling std dev of log returns
# - ma_7d/30d:              simple moving averages
# - dist_from_ma_7d:        % distance from 7-day moving average
# - ema_7d:                 exponential moving average (7-day)
# - rsi_14:                 relative strength index (14-day)
# - bb_pctb:                Bollinger band %B position
# - dist_from_lower_band:   distance from lower Bollinger band
#
# Depeg labels retained across all horizons:
# - depeg_1d, depeg_3d, depeg_5d, depeg_7d
#
# Cross-market contagion feature:
# - WLUNA_close: WLUNA closing price (systemic risk indicator)
#
# Missing observations from rolling windows removed via drop_na()
# =============================================================================







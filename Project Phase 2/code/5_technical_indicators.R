# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(dplyr)
library(zoo)
library(TTR)
library(tidyr)
library(readr)

dai <- read_csv("data/Dai/Dai_model_dataset.csv")
pax <- read_csv("data/Pax Dollar/Pax_model_dataset.csv")
usdc <- read_csv("data/USDC/USDC_model_dataset.csv")
usdt <- read_csv("data/Tether USDt/Tether_model_dataset.csv")
terra_usd <- read_csv("data/TerraClassicUSD/TerraClassicUSD_model_dataset.csv")

# names(dai)
# names(pax)
# names(usdc)
# names(usdt)
# names(terra_usd)

# dim(dai)
# dim(pax)
# dim(usdc)
# dim(usdt)
# dim(terra_usd)

keep_cols <- c(
  "date", "open", "high", "low", "close", "next_close",
  "depeg", "volume", "V_monthly", "ThreshD", "ThreshU",
  "marketCap", "circulatingSupply")
keep_stablecoin_cols <- function(df) {
  df[, keep_cols, drop = FALSE]
}

dai <- keep_stablecoin_cols(dai)
pax <- keep_stablecoin_cols(pax)
usdc <- keep_stablecoin_cols(usdc)
usdt <- keep_stablecoin_cols(usdt)
terra_usd <- keep_stablecoin_cols(terra_usd)

# names(dai)
# dim(dai)
# names(pax)
# dim(pax)
# names(usdc)
# dim(usdc)
# names(usdt)
# dim(usdt)
# names(terra_usd)
# dim(terra_usd)

dai$date <- as.Date(dai$date)
pax$date <- as.Date(pax$date)
usdc$date <- as.Date(usdc$date)
usdt$date <- as.Date(usdt$date)
terra_usd$date <- as.Date(terra_usd$date)

dai <- dai[order(dai$date), ]
pax <- pax[order(pax$date), ]
usdc <- usdc[order(usdc$date), ]
usdt <- usdt[order(usdt$date), ]
terra_usd <- terra_usd[order(terra_usd$date), ]

# head(dai$date)
# tail(dai$date)

add_basic_indicators <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      log_return = log(close / lag(close, 1)),
      close_lag1d = lag(close, 1),
      close_lag7d = lag(close, 7),
      abs_peg_deviation = abs(close - 1),
      Intraday_range = (high - low) / close)
}
dai <- add_basic_indicators(dai)
pax <- add_basic_indicators(pax)
usdc <- add_basic_indicators(usdc)
usdt <- add_basic_indicators(usdt)
terra_usd <- add_basic_indicators(terra_usd)

# names(dai)
# head(dai[, c("date", "close", "log_return", "close_lag1d", "close_lag7d", "abs_peg_deviation", "Intraday_range")])

add_volatility <- function(df) {
  df %>%
    mutate(
      volatility_3d  = rollapply(log_return, 3, sd, fill = NA, align = "right"),
      volatility_7d  = rollapply(log_return, 7, sd, fill = NA, align = "right"),
      volatility_30d = rollapply(log_return, 30, sd, fill = NA, align = "right"))
}
dai <- add_volatility(dai)
pax <- add_volatility(pax)
usdc <- add_volatility(usdc)
usdt <- add_volatility(usdt)
terra_usd <- add_volatility(terra_usd)

# head(dai[, c("log_return","volatility_3d","volatility_7d","volatility_30d")], 10)

add_trend_band_indicators <- function(df) {
  df %>%
    mutate(
      ma_7d = zoo::rollmean(close, 7, fill = NA, align = "right"),
      ma_30d = zoo::rollmean(close, 30, fill = NA, align = "right"),
      dist_from_ma_7d = (close - ma_7d) / ma_7d,
      ema_7d = TTR::EMA(close, n = 7),
      rsi_14 = TTR::RSI(close, n = 14),
      bb_mid = TTR::SMA(close, n = 20),
      bb_sd = zoo::rollapply(close, 20, sd, fill = NA, align = "right"),
      bb_upper = bb_mid + 2 * bb_sd,
      bb_lower = bb_mid - 2 * bb_sd,
      bb_pctb = (close - bb_lower) / (bb_upper - bb_lower),
      dist_from_lower_band = (close - bb_lower) / bb_lower)
}
# added temporary helper columns bb_mid, bb_sd, bb_upper, bb_lower. We can drop them after checking
dai <- add_trend_band_indicators(dai)
pax <- add_trend_band_indicators(pax)
usdc <- add_trend_band_indicators(usdc)
usdt <- add_trend_band_indicators(usdt)
terra_usd <- add_trend_band_indicators(terra_usd)

# head(dai[, c("close","ma_7d","ma_30d","dist_from_ma_7d","ema_7d", "rsi_14", "bb_pctb","dist_from_lower_band")], 25)

drop_helper_cols <- function(df) {
  df %>%
    select(-bb_mid, -bb_sd, -bb_upper, -bb_lower)
}
dai <- drop_helper_cols(dai)
pax <- drop_helper_cols(pax)
usdc <- drop_helper_cols(usdc)
usdt <- drop_helper_cols(usdt)
terra_usd <- drop_helper_cols(terra_usd)

# names(dai)

# check for missing values
colSums(is.na(dai))

clean_rows <- function(df){
  df %>% drop_na()
}
dai <- clean_rows(dai)
pax <- clean_rows(pax)
usdc <- clean_rows(usdc)
usdt <- clean_rows(usdt)
terra_usd <- clean_rows(terra_usd)
# 31 rows dropped for each coin

# sum(is.na(dai))
# dim(dai)

# table(dai$depeg)
# prop.table(table(dai$depeg))

get_date_range <- function(df, coin_name) {
  data.frame(
    coin = coin_name,
    min_date = min(df$date, na.rm = TRUE),
    max_date = max(df$date, na.rm = TRUE),
    n_obs = nrow(df))
}
date_ranges <- dplyr::bind_rows(
  get_date_range(dai, "DAI"),
  get_date_range(pax, "PAX"),
  get_date_range(usdc, "USDC"),
  get_date_range(usdt, "USDT"),
  get_date_range(terra_usd, "TERRA_USD"))

date_ranges
# Stablecoin datasets cover different time spans depending on when each coin was introduced and whether it survived the sample period. 
# USDT has the longest history (since 2017), while TerraUSD has the shortest sample ending in May 2022 due to its collapse. 
# DAI, PAX, and USDC begin around 2018–2019 and run until the end of 2025. 
# Models are therefore trained separately for each coin to account for differences in sample size and historical market conditions.

# summary(diff(dai$date)) to make sure diff is 1 day
# summary(diff(pax$date))
# summary(diff(usdc$date))
# summary(diff(usdt$date))
# summary(diff(terra_usd$date))

# Save datasets
write.csv(dai, "data/Dai/dai_price_technical_features.csv", row.names = FALSE)
write.csv(pax, "data/Pax Dollar/pax_price_technical_features.csv", row.names = FALSE)
write.csv(usdc, "data/USDC/usdc_price_technical_features.csv", row.names = FALSE)
write.csv(usdt, "data/Tether USDt/usdt_price_technical_features.csv", row.names = FALSE)
write.csv(terra_usd, "data/TerraClassicUSD/terra_usd_price_technical_features.csv", row.names = FALSE)

# ------------------------------------------------------------------
# Stablecoin Price + Technical Indicator Dataset
#
# This dataset contains daily OHLC price data and engineered technical
# indicators used to capture price momentum, volatility buildup,
# peg deviations, and trading range dynamics prior to potential
# stablecoin depegging events.
#
# Indicators include:
# - Returns and lagged prices
# - Rolling volatility (3d, 7d, 30d)
# - Moving averages, EMA and RSI
# - Bollinger band indicators
# - Peg deviation metrics
# - Intraday price range
#
# Missing observations created by rolling windows were removed to
# ensure a fully balanced feature matrix for downstream modeling.
# ------------------------------------------------------------------


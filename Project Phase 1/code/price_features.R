library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(purrr)
library(zoo) 
library(dplyr)
library(TTR) # For technical indicators like RSI or Volatility

#Config
# set current wd as project name (".../DSE4101-Stablecoin-Depegging-Prediction-Project/")
df_combined <- read_csv("data/prices_with_events_cleaned.csv")

# Standard daily windows
LAGS <- c(1, 7, 30) # lag windows
VOL_W <- c(3, 7, 30) # volatility weight for short-term and longer-term volatilities
MA_W  <- c(7, 14, 30) # moving averages
BB_N  <- 20 # bollinger bands window
RSI_N <- 14 # days chosen for RSA

df_combined <- df_combined %>%
  arrange(stablecoin, timestamp) %>%
  group_by(stablecoin) %>%
  mutate(
    # 1) Daily log return
    log_return = log(close / lag(close)),
    
    # 2) Lagged prices (1d/7d/30d)
    close_lag1d  = lag(close, LAGS[1]),
    close_lag7d  = lag(close, LAGS[2]),
    close_lag30d = lag(close, LAGS[3]),
    
    # 3) Rolling volatility of returns (3d/7d/30d)
    volatility_3d  = zoo::rollapply(log_return, width = VOL_W[1], FUN = sd, fill = NA, align = "right"),
    volatility_7d  = zoo::rollapply(log_return, width = VOL_W[2], FUN = sd, fill = NA, align = "right"),
    volatility_30d = zoo::rollapply(log_return, width = VOL_W[3], FUN = sd, fill = NA, align = "right"),
    
    # 4) Peg deviation 
    peg_deviation     = close - 1.0,
    abs_peg_deviation = abs(close - 1.0),
    
    # 5) Moving averages + distance (7d/30d)
    ma_7d       = zoo::rollmean(close, k = MA_W[1], fill = NA, align = "right"),
    ma_14d      = zoo::rollmean(close, k = MA_W[2], fill = NA, align = "right"),
    ma_30d      = zoo::rollmean(close, k = MA_W[3], fill = NA, align = "right"),
    dist_from_ma_7d  = close - ma_7d,
    dist_from_ma_14d  = close - ma_14d,
    dist_from_ma_30d = close - ma_30d,
    
    # 6) Daily candle range
    intraday_range = high - low,
    
    # Technical indicators
    # no need for sma because same as ma here
    ema_7d  = TTR::EMA(close, n = 7), # exponentially weighted MA (gives more weight to recent vals)
    ema_14d  = TTR::EMA(close, n = 14),
    ema_30d = TTR::EMA(close, n = 30),
    
    # RSI (relative strength index)
    rsi_14d = TTR::RSI(close, n = RSI_N), # momentum indicator (too fast means overbuying and vice versa)
    
    # Bollinger Bands (20d classic, captures vol+stress+trend)
    bb_up   = TTR::BBands(close, n = BB_N, sd = 2)[, "up"],
    bb_dn   = TTR::BBands(close, n = BB_N, sd = 2)[, "dn"],
    bb_pctb = TTR::BBands(close, n = BB_N, sd = 2)[, "pctB"],
    dist_from_lower_band = close - bb_dn
  ) %>%
  ungroup()

write_csv(df_combined, "data/prices_with_events_pricefeatures.csv")

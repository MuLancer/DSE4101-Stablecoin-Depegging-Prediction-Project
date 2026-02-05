library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(purrr)
library(zoo) 
library(TTR) # For technical indicators like RSI or Volatility

#Config
setwd("/Users/yizhouhang/Documents/Y4S2/DSE4101/src")
DATA_DIR <- "../data/ERC20-stablecoins/"
df_combined <- read_csv(file.path(DATA_DIR, "prices_with_events_cleaned.csv"))

# Standard daily windows
LAGS <- c(1, 7, 30)
VOL_W <- c(7, 30)
MA_W  <- c(7, 30)
BB_N  <- 20
RSI_N <- 14

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
    
    # 3) Rolling volatility of returns (7d/30d)
    volatility_7d  = zoo::rollapply(log_return, width = VOL_W[1], FUN = sd, fill = NA, align = "right"),
    volatility_30d = zoo::rollapply(log_return, width = VOL_W[2], FUN = sd, fill = NA, align = "right"),
    
    # 4) Peg deviation 
    peg_deviation     = close - 1.0,
    abs_peg_deviation = abs(close - 1.0),
    
    # 5) Moving averages + distance (7d/30d)
    ma_7d       = zoo::rollmean(close, k = MA_W[1], fill = NA, align = "right"),
    ma_30d      = zoo::rollmean(close, k = MA_W[2], fill = NA, align = "right"),
    dist_from_ma_7d  = close - ma_7d,
    dist_from_ma_30d = close - ma_30d,
    
    # 6) Daily candle range
    intraday_range = high - low,
    
    # --- Technical indicators (daily) ---
    sma_7d  = TTR::SMA(close, n = 7),
    sma_30d = TTR::SMA(close, n = 30),
    
    ema_7d  = TTR::EMA(close, n = 7),
    ema_30d = TTR::EMA(close, n = 30),
    
    # RSI
    rsi_14d = TTR::RSI(close, n = RSI_N),
    
    # Bollinger Bands (20d, classic)
    bb = list(TTR::BBands(close, n = BB_N, sd = 2)),
    bb_up   = bb[[1]][, "up"],
    bb_dn   = bb[[1]][, "dn"],
    bb_pctb = bb[[1]][, "pctB"],
    dist_from_lower_band = close - bb_dn
  ) %>%
  ungroup() %>%
  tidyr::drop_na()

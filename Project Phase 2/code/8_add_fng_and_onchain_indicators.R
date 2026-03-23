# =============================================================================
# FEATURE ENGINEERING PIPELINE
# Builds the full feature set for each stablecoin (DAI, PAX, USDC, USDT, UST) by combining technical indicators, on-chain metrics, and macro signals.
# Set working directory to: Project Phase 2
# setwd("~/DSE4101/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")
# =============================================================================

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(zoo)
library(TTR)
library(tidyr)


# -----------------------------------------------------------------------------
# 1. LOAD RAW TECHNICAL FEATURE DATA
# Each CSV contains OHLCV + pre-computed technical indicators per stablecoin
# -----------------------------------------------------------------------------
dai  <- read_csv("data/Dai/DAI_technical_features.csv")
pax  <- read_csv("data/PAX/PAX_technical_features.csv")
usdc <- read_csv("data/USDC/USDC_technical_features.csv")
usdt <- read_csv("data/USDT/USDT_technical_features.csv")
ust  <- read_csv("data/UST/UST_technical_features.csv")

# Parse and sort by date ascending
dai$date  <- as.Date(dai$date)
pax$date  <- as.Date(pax$date)
usdc$date <- as.Date(usdc$date)
usdt$date <- as.Date(usdt$date)
ust$date  <- as.Date(ust$date)

dai  <- dai[order(dai$date), ]
pax  <- pax[order(pax$date), ]
usdc <- usdc[order(usdc$date), ]
usdt <- usdt[order(usdt$date), ]
ust  <- ust[order(ust$date), ]


# -----------------------------------------------------------------------------
# 2. MERGE FEAR & GREED INDEX
# Macro sentiment signal — proxy for broader crypto market stress
# -----------------------------------------------------------------------------
fng <- read.csv("data/fear_and_greed_index_2.csv") %>%
  mutate(date = as.Date(date))

add_fng <- function(df) {
  df %>% left_join(fng, by = "date")
}

dai  <- add_fng(dai)
pax  <- add_fng(pax)
usdc <- add_fng(usdc)
usdt <- add_fng(usdt)
ust  <- add_fng(ust)


# -----------------------------------------------------------------------------
# 3. PRICE MOMENTUM — PERCENTAGE CHANGES
# Captures short-to-medium term price movement across OHLC columns
# Windows: 1d (daily), 3d (short), 7d (weekly), 30d (monthly)
# -----------------------------------------------------------------------------
add_pct_change <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      # 1-day
      pct_open_1d  = (open  - lag(open,  1)) / lag(open,  1),
      pct_high_1d  = (high  - lag(high,  1)) / lag(high,  1),
      pct_low_1d   = (low   - lag(low,   1)) / lag(low,   1),
      pct_close_1d = (close - lag(close, 1)) / lag(close, 1),
      
      # 3-day
      pct_open_3d  = (open  - lag(open,  3)) / lag(open,  3),
      pct_high_3d  = (high  - lag(high,  3)) / lag(high,  3),
      pct_low_3d   = (low   - lag(low,   3)) / lag(low,   3),
      pct_close_3d = (close - lag(close, 3)) / lag(close, 3),
      
      # 7-day
      pct_open_7d  = (open  - lag(open,  7)) / lag(open,  7),
      pct_high_7d  = (high  - lag(high,  7)) / lag(high,  7),
      pct_low_7d   = (low   - lag(low,   7)) / lag(low,   7),
      pct_close_7d = (close - lag(close, 7)) / lag(close, 7),
      
      # 30-day
      pct_open_30d  = (open  - lag(open,  30)) / lag(open,  30),
      pct_high_30d  = (high  - lag(high,  30)) / lag(high,  30),
      pct_low_30d   = (low   - lag(low,   30)) / lag(low,   30),
      pct_close_30d = (close - lag(close, 30)) / lag(close, 30)
    )
}

dai  <- add_pct_change(dai)
pax  <- add_pct_change(pax)
usdc <- add_pct_change(usdc)
usdt <- add_pct_change(usdt)
ust  <- add_pct_change(ust)


# -----------------------------------------------------------------------------
# 4. PARKINSON'S VOLATILITY ESTIMATOR
# Uses intraday high-low range to estimate volatility (more efficient than
# close-to-close). Formula: sqrt(1/(4*ln2) * ln(High/Low)^2)
#   - parkinson_daily : single-day raw intraday range
#   - parkinson_7d    : 7-day rolling estimate of the weekly volatility regime
# -----------------------------------------------------------------------------
add_parkinson_metrics <- function(df, window = 7) {
  df %>%
    arrange(date) %>%
    mutate(
      k = 1 / (4 * log(2)),  # Parkinson's constant
      
      parkinson_daily = sqrt(k * (log(high / low)^2)),
      
      parkinson_7d = rollapplyr(
        log(high / low)^2,
        width = window,
        FUN   = function(x) sqrt(sum(x) / (4 * window * log(2))),
        fill  = NA
      )
    ) %>%
    select(-k)
}

dai  <- add_parkinson_metrics(dai)
pax  <- add_parkinson_metrics(pax)
usdc <- add_parkinson_metrics(usdc)
usdt <- add_parkinson_metrics(usdt)
ust  <- add_parkinson_metrics(ust)


# -----------------------------------------------------------------------------
# 5. RELATIVE VOLUME (RVOL)
# Compares today's volume against its 20-day moving average.
# RVOL > 1 = unusually high activity; may signal depeg stress or news events.
# -----------------------------------------------------------------------------
add_rvol_metric <- function(df, window = 20) {
  df %>%
    arrange(date) %>%
    mutate(
      volume_ma_20d = rollmean(volume, k = window, fill = NA, align = "right"),
      rvol          = volume / volume_ma_20d
    ) %>%
    select(-volume_ma_20d)
}

dai  <- add_rvol_metric(dai)
pax  <- add_rvol_metric(pax)
usdc <- add_rvol_metric(usdc)
usdt <- add_rvol_metric(usdt)
ust  <- add_rvol_metric(ust)     
 

# -----------------------------------------------------------------------------
# 6. CROSS-COIN SPILLOVER EFFECTS
# Adds the close prices of all other stablecoins as features.
# Captures contagion risk — e.g. UST depegging affecting USDT/USDC confidence.
# -----------------------------------------------------------------------------
all_closes <- bind_rows(
  dai  %>% mutate(coin = "dai"),
  pax  %>% mutate(coin = "pax"),
  usdc %>% mutate(coin = "usdc"),
  usdt %>% mutate(coin = "usdt"),
  ust  %>% mutate(coin = "ust")
) %>%
  select(date, coin, close) %>%
  pivot_wider(names_from = coin, values_from = close, names_prefix = "close_")

# Join all closes, then drop the coin's own close (already present as `close`)
add_spillovers <- function(df, current_coin_name) {
  df %>%
    left_join(all_closes, by = "date") %>%
    select(-all_of(paste0("close_", current_coin_name)))
}

dai  <- add_spillovers(dai,  "dai")
pax  <- add_spillovers(pax,  "pax")
usdc <- add_spillovers(usdc, "usdc")
usdt <- add_spillovers(usdt, "usdt")
ust  <- add_spillovers(ust,  "ust")

# Rename WLUNA_close to close_wluna for naming consistency with other spillover columns
# and move to last column
rename_wluna <- function(df) {
  df %>%
    rename(close_wluna = WLUNA_close) %>%
    relocate(close_wluna, .after = last_col())
}

dai  <- rename_wluna(dai)
pax  <- rename_wluna(pax)
usdc <- rename_wluna(usdc)
usdt <- rename_wluna(usdt)
ust  <- rename_wluna(ust)


# -----------------------------------------------------------------------------
# 7. ON-CHAIN METRICS
# Loads Ethereum on-chain data (entropy, gini, net flows, PIN, whale activity)and merges it into each stablecoin's dataframe.
# -----------------------------------------------------------------------------
df_onchain <- read.csv(
  "data/onchain_metrics_all.csv",
  colClasses = c("token_address" = "character")
)

# Map contract addresses to human-readable token names
token_mapping <- data.frame(
  token_address = c(
    '0x6b175474e89094c44da98b954eedeac495271d0f', # DAI
    '0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48', # USDC
    '0xdac17f958d2ee523a2206206994597c13d831ec7', # USDT
    '0x89d24a6b4ccb1b6faa2625fe562bdd9a23260359', # PAX
    '0x0000000000085d4780b73119b644ae5ecd22b376'  # UST
  ),
  token_name = c("DAI", "USDC", "USDT", "PAX", "UST"),
  stringsAsFactors = FALSE
)

df_onchain <- df_onchain %>%
  mutate(
    date             = as.Date(date),
    gini_coefficient = abs(gini_coefficient),      # Ensure non-negative
    token_address    = as.character(token_address)
  ) %>%
  left_join(token_mapping, by = "token_address")



# -----------------------------------------------------------------------------
# 8. ON-CHAIN FEATURE ENGINEERING
# For each metric, computes:
#   - 7-day rolling average  : smoothed baseline trend
#   - log-difference         : rate of change / shock detection
# Metrics: Shannon Entropy, Gini, Net Swap Flow, PIN Proxy, Big Player Vol %
# -----------------------------------------------------------------------------
clean_onchain_features <- function(price_df, name_label) {
  
  token_onchain <- df_onchain %>%
    filter(token_name == name_label) %>%
    arrange(date)
  
  price_df %>%
    left_join(token_onchain, by = "date") %>%
    arrange(date) %>%
    mutate(
      # Entropy — wallet diversity
      entropy_7d_avg   = rollapply(shannon_entropy, 7, mean, fill = NA, align = "right"),
      log_diff_entropy = log(1 + shannon_entropy / (lag(shannon_entropy) + 0.0001)),
      
      # Gini — transaction size inequality
      gini_7d_avg      = rollapply(gini_coefficient, 7, mean, fill = NA, align = "right"),
      log_diff_gini    = log(1 + gini_coefficient / (lag(gini_coefficient) + 0.0001)),
      
      # Net swap flow — net capital direction (positive = inflow, negative = outflow)
      net_flow_7d_avg   = rollapply(net_swap_flow, 7, mean, fill = NA, align = "right"),
      log_diff_net_flow = log(1 + abs(net_swap_flow) / (lag(abs(net_swap_flow)) + 1)),
      
      # PIN proxy — order imbalance / information asymmetry
      pin_7d_avg   = rollapply(pin_proxy, 7, mean, fill = NA, align = "right"),
      log_diff_pin = log(1 + pin_proxy / (lag(pin_proxy) + 0.0001)),
      
      # Big player activity — % volume from whale/institutional wallets
      big_player_7d_avg = rollapply(big_players_vol_pct, 7, mean, fill = NA, align = "right")
    ) %>%
    select(-any_of("X")) %>%
    # Reorder columns to keep derived features adjacent to their source metric
    relocate(token_name, .after = token_address) %>%
    relocate(entropy_7d_avg, log_diff_entropy,   .after = shannon_entropy) %>%
    relocate(gini_7d_avg, log_diff_gini,         .after = gini_coefficient) %>%
    relocate(net_flow_7d_avg, log_diff_net_flow, .after = net_swap_flow) %>%
    relocate(pin_7d_avg, log_diff_pin,           .after = pin_proxy) %>%
    relocate(big_player_7d_avg,                  .after = big_players_vol_pct)
}

dai  <- clean_onchain_features(dai,  "DAI")
pax  <- clean_onchain_features(pax,  "PAX")
usdc <- clean_onchain_features(usdc, "USDC")
usdt <- clean_onchain_features(usdt, "USDT")
ust  <- clean_onchain_features(ust,  "UST")

# -----------------------------------------------------------------------------
# 9. SAVE FINAL FEATURE DATASETS
# -----------------------------------------------------------------------------
write.csv(dai,  "data/Dai/DAI_onchain_features.csv",   row.names = FALSE)
write.csv(pax,  "data/PAX/PAX_onchain_features.csv",   row.names = FALSE)
write.csv(usdc, "data/USDC/USDC_onchain_features.csv", row.names = FALSE)
write.csv(usdt, "data/USDT/USDT_onchain_features.csv", row.names = FALSE)
write.csv(ust,  "data/UST/UST_onchain_features.csv",   row.names = FALSE)

# --- ON-CHAIN METRIC DEFINITIONS ---

# 1. Shannon's Entropy (shannon_entropy)
# Measures "Wallet Diversity." 
# High = Volume is spread across many different users (Decentralized/Organic).
# Low = Volume is concentrated in just a few wallets (Centralized/Manipulated).

# 2. Gini Coefficient (gini_coefficient)
# Measures "Size Inequality" of transactions.
# 0 = All transfers are roughly the same size.
# 1 = One single transaction accounts for almost all volume (Whale dominance).

# 3. PIN Proxy / Order Imbalance (pin_proxy)
# Measures "Information Asymmetry" or "Panic."
# Based on the imbalance between "buys" (above median size) and "sells" (below median size).
# High = Extreme lopsided trading, often seen during price discovery or depegs.

# 4. Shark Volume Pct (shark_volume_pct)
# Measures "Smart Money Dominance."
# The percentage of total daily volume coming from "Sharks" (Top 10% Vol + Top 25% Frequency).
# High = Institutional/Sophisticated players are controlling the movement of the coin.

# 5. Net Swap Flow (net_swap_flow)
# Measures "Net Liquidity Direction."
# (Total Inflows to non-zero addresses) - (Total Outflows from non-zero addresses).
# Positive = Capital entering the ecosystem; Negative = Capital exiting (Flight to Safety).

# 6. Log-Differences (log_diff_...)
# Measures "Momentum/Acceleration."
# These calculate the rate of change from yesterday to today.
# High values indicate a "Shock" or sudden move that might trigger a depeg alert.

# 7. 7-Day Averages (..._7d_avg)
# Measures "Baseline Trends."
# These smooth out the daily "noise" to show the underlying health of the stablecoin
# over a rolling week, making it easier to spot long-term decay vs. temporary spikes.

# -----------------------------------------------------------------------------------------------
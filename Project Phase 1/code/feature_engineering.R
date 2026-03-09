library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(zoo)
library(tidyr)

df_transactions <- read_csv("data/token_transfers_cleaned.csv")
df_prices <- read_csv("data/prices_with_events_pricefeatures.csv")

# Add percentage change features
df_prices <- df_prices %>%
  group_by(stablecoin) %>%
  arrange(timestamp) %>%
  mutate(
    # 1-day percentage changes
    pct_open_1d = (open - lag(open, 1)) / lag(open, 1),
    pct_high_1d = (high - lag(high, 1)) / lag(high, 1),
    pct_low_1d = (low - lag(low, 1)) / lag(low, 1),
    pct_close_1d = (close - lag(close, 1)) / lag(close, 1),
    
    # 3-day percentage changes
    pct_open_3d = (open - lag(open, 3)) / lag(open, 3),
    pct_high_3d = (high - lag(high, 3)) / lag(high, 3),
    pct_low_3d = (low - lag(low, 3)) / lag(low, 3),
    pct_close_3d = (close - lag(close, 3)) / lag(close, 3),
    
    # 7-day percentage changes
    pct_open_7d = (open - lag(open, 7)) / lag(open, 7),
    pct_high_7d = (high - lag(high, 7)) / lag(high, 7),
    pct_low_7d = (low - lag(low, 7)) / lag(low, 7),
    pct_close_7d = (close - lag(close, 7)) / lag(close, 7),
    
    # 30-day percentage changes
    pct_open_30d = (open - lag(open, 30)) / lag(open, 30),
    pct_high_30d = (high - lag(high, 30)) / lag(high, 30),
    pct_low_30d = (low - lag(low, 30)) / lag(low, 30),
    pct_close_30d = (close - lag(close, 30)) / lag(close, 30)
  ) %>%
  ungroup()

# Shannon's Entropy
df_entropy <- df_transactions %>%
  group_by(date, token_name) %>%
  summarise(
    shannon_entropy = {
      if (n() > 1 && sum(value) > 0) {
        probs <- value / sum(value)
        probs <- probs[probs > 0]
        -sum(probs * log2(probs))
      } else {
        NA_real_
      }
    },
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(log_diff_entropy = log(1 + shannon_entropy / lag(shannon_entropy))) %>%
  ungroup()

# Gini Coefficient
calculate_gini <- function(x) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  if (n == 0) return(NA_real_)
  x <- sort(x)
  gini <- (2 * sum((1:n) * x)) / (n * sum(x)) - (n + 1) / n
  return(gini)
}

df_gini <- df_transactions %>%
  group_by(date, token_name) %>%
  summarise(
    gini_coefficient = calculate_gini(value),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(log_diff_gini = log(1 + gini_coefficient / lag(gini_coefficient))) %>%
  ungroup()

# Net Swap Flows
df_flows <- df_transactions %>%
  group_by(date, token_name) %>%
  summarise(
    total_inflow = sum(value[to_address != "0x0000000000000000000000000000000000000000"], na.rm = TRUE),
    total_outflow = sum(value[from_address != "0x0000000000000000000000000000000000000000"], na.rm = TRUE),
    net_swap_flow = total_inflow - total_outflow,
    n_transactions = n(),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(
    log_diff_net_flow = log(1 + abs(net_swap_flow) / lag(abs(net_swap_flow) + 1))
  ) %>%
  ungroup()


# Shark Trades
df_sharks <- df_transactions %>%
  group_by(from_address, token_name) %>%
  summarise(
    total_volume = sum(value, na.rm = TRUE),
    n_trades = n(),
    avg_trade_size = mean(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  mutate(
    is_shark = (total_volume > quantile(total_volume, 0.9, na.rm = TRUE)) & 
      (n_trades > quantile(n_trades, 0.75, na.rm = TRUE))
  ) %>%
  ungroup()

df_shark_activity <- df_transactions %>%
  left_join(df_sharks %>% select(from_address, token_name, is_shark), 
            by = c("from_address", "token_name")) %>%
  group_by(date, token_name) %>%
  summarise(
    shark_volume = sum(value[is_shark == TRUE], na.rm = TRUE),
    total_volume = sum(value, na.rm = TRUE),
    shark_volume_pct = shark_volume / total_volume,
    n_shark_trades = sum(is_shark == TRUE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(
    log_diff_shark_pct = log(1 + shark_volume_pct / lag(shark_volume_pct))
  ) %>%
  ungroup()

# Combine all features and remove duplicates
df_all <- df_entropy %>%
  left_join(df_prices, by = c("date" = "timestamp", "token_name" = "stablecoin")) %>%
  left_join(df_gini, by = c("date", "token_name")) %>%
  left_join(df_flows, by = c("date", "token_name")) %>%
  left_join(df_shark_activity, by = c("date", "token_name")) %>%
  select(-pct_close_1d)  # Remove duplicates

# Spillover - add price data from other tokens
create_spillover <- function(df) {
  tokens <- unique(df$token_name)
  df_result <- df
  
  for (other_token in tokens) {
    # Get other token's OHLC data
    other_ohlc <- df %>%
      filter(token_name == other_token) %>%
      select(date, open, high, low, close) %>%
      rename(
        !!paste0(other_token, "_open") := open,
        !!paste0(other_token, "_high") := high,
        !!paste0(other_token, "_low") := low,
        !!paste0(other_token, "_close") := close
      )
    
    df_result <- df_result %>%
      left_join(other_ohlc, by = "date")
  }
  
  # Set own token's spillover to NA (do this after all joins)
  for (token in tokens) {
    col_names <- c(
      paste0(token, "_open"),
      paste0(token, "_high"),
      paste0(token, "_low"),
      paste0(token, "_close")
    )
    
    for (col_name in col_names) {
      if (col_name %in% colnames(df_result)) {
        df_result <- df_result %>%
          mutate(!!col_name := if_else(token_name == token, NA_real_, !!sym(col_name)))
      }
    }
  }
  
  return(df_result)
}

df_all <- create_spillover(df_all)
colnames(df_all)

write_csv(df_all, "data/df_all.csv")

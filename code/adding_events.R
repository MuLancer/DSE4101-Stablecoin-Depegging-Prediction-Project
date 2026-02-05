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
df_prices <- read_csv(file.path(DATA_DIR, "prices_cleaned.csv"))
df_events <- read_csv(file.path(DATA_DIR, "event_data.csv"))

#adding events data
df_events_processed <- df_events %>%
  mutate(
    timestamp = as_datetime(timestamp),
    event_feature_name = paste0(stablecoin, "_events_", type)
  ) %>%
  select(timestamp, event_feature_name) %>%
  group_by(timestamp, event_feature_name) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = event_feature_name,
    values_from = count,
    values_fill = list(count = 0) # Fill missing intervals with 0
  ) %>%
  mutate(across(-timestamp, ~ ifelse(. > 0, 1, 0)))

# 3. Combine with Price Data
df_combined <- df_prices %>%
  left_join(df_events_processed, by = "timestamp") %>%
  # Replace NAs (timestamps with no events) with 0 for all event columns
  mutate(across(ends_with("_positive") | ends_with("_negative"), ~replace_na(., 0)))

write_csv(df_combined, file.path(DATA_DIR, "prices_with_events_cleaned.csv"))

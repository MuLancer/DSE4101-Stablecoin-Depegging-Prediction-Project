library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(purrr)
library(zoo) 
library(TTR) # For technical indicators like RSI or Volatility

#Config
# set current wd as project name (".../DSE4101-Stablecoin-Depegging-Prediction-Project/")
df_prices <- read_csv("data/prices_cleaned.csv")
df_events <- read_csv("data/event_data.csv")

# adding events data
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
  mutate(across(-timestamp, ~ ifelse(. > 0, 1, 0))) #just an extra step to ensure consistency even tho the values are already 0 or 1

# sorting col names
df_events_processed <- df_events_processed %>%
  select(sort(names(.))) %>%
  select(timestamp, everything()) %>%
  mutate(usdt_events_negative = 0, .before = usdt_events_positive)

# 3. Combine with Price Data
df_combined <- df_prices %>%
  left_join(df_events_processed, by = "timestamp") %>%
  # Replace NAs (timestamps with no events) with 0 for all event columns
  mutate(across(ends_with("_positive") | ends_with("_negative"), ~replace_na(., 0)))

write_csv(df_combined, "data/prices_with_events_cleaned.csv")

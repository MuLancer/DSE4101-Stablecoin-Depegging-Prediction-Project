library(dplyr)
library(lubridate)
library(readr)
library(purrr)

# Configuration
# Using abs ref just once since the data file is too big to be pushed to GitHub
setwd("/Users/mulan/NUS/AY25_Y4S2/DSE4101/code/DSE4101-Stablecoin-Depegging-Prediction-Project/data/ERC20-stablecoins/")
transaction_file_path <- "token_transfers_V3.0.0.csv"
price_data_dir <- "price_data/"
##

# Token mapping
token_mappings <- c(
  "0xdac17f958d2ee523a2206206994597c13d831ec7" = "USDT",
  "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48" = "USDC",
  "0x6b175474e89094c44da98b954eedeac495271d0f" = "DAI",
  "0xa47c8bf37f92abed4a126bda807a7b7498661acd" = "UST",
  "0x8e870d67f660d95d5be530380d0ec0bd388289e1" = "PAX",
  "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9" = "WLUNA"
)

# Load transactions
df_transactions <- read_csv(transaction_file_path)

# Convert timestamps and extract features
df_transactions <- df_transactions %>%
  mutate(
    timestamp = as_datetime(time_stamp),
    date = as_date(timestamp),
    hour = hour(timestamp),
    day_of_week = wday(timestamp, week_start = 1) - 1, # 0 = Monday
    token_name = token_mappings[contract_address]
  )

# Price files mapping
price_files <- c(
  "dai_price_data.csv" = "DAI",
  "pax_price_data.csv" = "PAX",
  "usdc_price_data.csv" = "USDC",
  "usdt_price_data.csv" = "USDT",
  "ustc_price_data.csv" = "UST",
  "wluna_price_data.csv" = "WLUNA"
)

# Load all price data
df_prices <- map2_df(names(price_files), price_files, ~ {
  df <- read_csv(file.path(price_data_dir, .x))
  df %>%
    mutate(
      stablecoin = .y,
      date = as_date(timestamp)
    )
})

# Save cleaned data
# Reset wd as project name now (".../DSE4101-Stablecoin-Depegging-Prediction-Project/")
write_csv(df_transactions, "data/transactions_cleaned.csv")
write_csv(df_prices, "data/prices_cleaned.csv")


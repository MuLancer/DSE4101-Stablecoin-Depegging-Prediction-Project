library(dplyr)
library(lubridate)
library(readr)
library(purrr)

# Configuration
# The dataset for token transfers is stored locally due to its size (~5GB). The data is excluded via .gitignore to avoid exceeding repository size limits.
transaction_file_path <- "Project Phase 2/data/token_transfers_V3.0.0.csv"

# Token mapping
token_mappings <- c(
  "0xdac17f958d2ee523a2206206994597c13d831ec7" = "USDT",
  "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48" = "USDC",
  "0x6b175474e89094c44da98b954eedeac495271d0f" = "DAI",
  "0xa47c8bf37f92abed4a126bda807a7b7498661acd" = "UST",
  "0x8e870d67f660d95d5be530380d0ec0bd388289e1" = "PAX",
  "0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9" = "WLUNA"
)

df_transactions <- read_csv(transaction_file_path)

# Convert timestamps and extract features
df_transactions <- df_transactions %>%
  mutate(
    timestamp = as_datetime(time_stamp),
    date = as_date(timestamp),
    hour = hour(timestamp),
    day_of_week = wday(timestamp, week_start = 1) - 1, # 0 = Monday
    token_name = token_mappings[contract_address]
  ) %>%
  select(-time_stamp) 


# Save cleaned data
# Reset wd as project name now (".../DSE4101-Stablecoin-Depegging-Prediction-Project/")
#write_csv(df_transactions, "Project Phase 2/data/token_transfers_cleaned.csv")

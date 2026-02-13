library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(zoo)
library(tidyr)
library(binancer)
library(yahoofinancer)
library(quantmod)
library(reticulate)
library(coinmarketcapr)
library(data.table)
library(ggplot2)

coins <- c("DAI", "PAXG", "USDC", 
           "USDT", 
           "UST", "USTC", "LUNA")

#using cmc

# file paths per coin (have 6 CSVs per coin)
files <- list(
  DAI  = list.files("./data/cmc/", pattern = "Dai", full.names = TRUE),
  PAXG = list.files("./data/cmc/", pattern = "PAX Gold", full.names = TRUE),
  USDC = list.files("./data/cmc/", pattern = "USDC", full.names = TRUE),
  USDT = list.files("./data/cmc/", pattern = "Tether USDt", full.names = TRUE),
  UST  = list.files("./data/cmc/", pattern = "TerraClassicUSD", full.names = TRUE),
  LUNA = list.files("./data/cmc/", pattern = "Terra Classic", full.names = TRUE)
)

all_data <- list()

for (symbol in names(files)) {
  # read all CSVs for this coin
  coin_data <- lapply(files[[symbol]], function(f) {
    fread(
      f,
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE,
      quote = ""
    )
  }) %>%
    bind_rows() %>%
    distinct()   # remove duplicates from overlapping CSVs
  
  # add symbol column
  coin_data$symbol <- symbol
  
  # store
  all_data[[symbol]] <- coin_data
}

# combine all coins into one dataframe
combined_df <- bind_rows(all_data)

# Clean the combined_df
cleaned_df <- combined_df %>%
  # remove all extra quotes from character columns
  mutate(across(where(is.character), ~ gsub('"', '', .))) %>%
  
  # convert the 5 time columns to POSIXct using lubridate
  mutate(
    timeOpen  = ymd_hms(timeOpen, tz = "UTC"),
    timeClose = ymd_hms(timeClose, tz = "UTC"),
    timeHigh  = ymd_hms(timeHigh, tz = "UTC"),
    timeLow   = ymd_hms(timeLow, tz = "UTC"),
    timestamp = ymd_hms(timestamp, tz = "UTC")
  ) %>%
  
  # convert numeric columns that may have been read as character
  mutate(
    open               = as.numeric(open),
    high               = as.numeric(high),
    low                = as.numeric(low),
    close              = as.numeric(close),
    volume             = as.numeric(volume),
    marketCap          = as.numeric(marketCap),
    circulatingSupply  = as.numeric(circulatingSupply)
  ) %>%
  
  # reorder columns
  select(
    symbol,
    timeOpen, open,
    timeClose, close,
    timeHigh, high,
    timeLow, low,
    volume, marketCap, circulatingSupply, timestamp
  ) %>%
  
  #reorder rows
  arrange(symbol, timeOpen)

# check
dim(cleaned_df)
head(cleaned_df)
str(cleaned_df)

#write.csv(cleaned_df, file="./data/historical_prices_cmc.csv")

# compare coins at one time
compare_df = cleaned_df %>%
  arrange(timestamp)


#visualisation

#prices of stablecoins minus gold, terrausd and luna
df_plot <- cleaned_df %>% filter(!symbol %in% c("PAXG","LUNA","UST"))

ggplot(df_plot, aes(x = timeClose, y = close, color = symbol)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Close Prices of Stablecoins Over Time",
    x = "Date",
    y = "Close Price (USD)",
    color = "Coin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#indiv prices
ggplot(cleaned_df, aes(x = timeClose, y = close)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  facet_wrap(~ symbol, scales = "free_y", ncol = 3) +  
  labs(
    title = "Close Prices of Coins Over Time",
    x = "Date",
    y = "Close Price (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )


#from 2022-2024 to see terrausd crash
df_plot2 = df_plot %>% filter(year(timeOpen)>2022 & year(timeOpen)<2024)
ggplot(df_plot2, aes(x = timeClose, y = close, color = symbol)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Close Prices of stablcoins from 2022-2024",
    x = "Date",
    y = "Close Price (USD)",
    color = "Coin"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )



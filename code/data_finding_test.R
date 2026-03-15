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


df_all = read.csv("./data/df_all.csv")


###binancer###

symbols_df <- data.frame(symbol = binance_symbols())

coins <- c("DAI", "PAXG", "USDC", 
           #"USDT", 
           "UST", "USTC", "LUNA")

filtered_symbols_df = symbols_df %>%
  filter(
    grepl(paste(coins, collapse = "|"), symbol),
    grepl("USDT$", symbol)
  )%>%
  pull(symbol)  # get as vector

# Date range
start_date <- as_datetime("2022-04-01")
end_date   <- as_datetime("2022-11-01")

# Function to safely fetch historical klines
fetch_ohlc <- function(sym, start_date, end_date, interval="1d") {
  message("Fetching ", sym)
  klines <- tryCatch({
    binance_klines(
      symbol = sym,
      interval = interval,
      start_time = start_date,
      end_time   = end_date
    ) %>%
      mutate(symbol = sym)
  }, error = function(e) {
    message("No data for ", sym, " or API error.")
    return(NULL)
  })
  return(klines)
}

# Loop through symbols for 1d
ohlc_list <- lapply(filtered_symbols_df, fetch_ohlc, start_date, end_date)

# Remove NULL results and bind
ohlc_df <- bind_rows(ohlc_list) 

# Convert timestamps
if(nrow(ohlc_df) > 0) {
  ohlc_df <- ohlc_df %>%
    mutate(open_time  = as_datetime(open_time),
           close_time = as_datetime(close_time)) 
}

ohlc_df <- ohlc_df %>%
  mutate(date = as.Date(open_time)) %>%   # <-- just the day
  arrange(date)


head(ohlc_df)


# Date range
start_date <- as_datetime("2024-04-01")
end_date   <- as_datetime("2024-05-01")

# Loop through symbols for 1d
ohlc_list_1m <- lapply(filtered_symbols_df, fetch_ohlc, start_date, end_date, "1m")

# Remove NULL results and bind
ohlc_df_1m <- bind_rows(ohlc_list_1m) 

# Convert timestamps
if(nrow(ohlc_df_1m) > 0) {
  ohlc_df_1m <- ohlc_df_1m %>%
    mutate(open_time  = as_datetime(open_time),
           close_time = as_datetime(close_time)) 
}

ohlc_df_1m <- ohlc_df_1m %>%
  mutate(date = as.Date(open_time)) %>%   # <-- just the day
  arrange(date)


head(ohlc_df_1m)


## 5m
# Date range
start_date <- as_datetime("2023-03-06")
end_date   <- as_datetime("2024-04-20")

# Loop through symbols for 1d
ohlc_list_5m <- lapply(filtered_symbols_df, fetch_ohlc, start_date, end_date, "5m")

# Remove NULL results and bind
ohlc_df_5m <- bind_rows(ohlc_list_5m) 

# Convert timestamps
if(nrow(ohlc_df_5m) > 0) {
  ohlc_df_5m <- ohlc_df_5m %>%
    mutate(open_time  = as_datetime(open_time),
           close_time = as_datetime(close_time)) 
}

ohlc_df_5m <- ohlc_df_5m %>%
  mutate(date = as.Date(open_time)) %>%   # <-- just the day
  arrange(open_time)


head(ohlc_df_5m)




# FULL
fetch_ohlc_full <- function(sym, start_date, end_date, interval = "5m") {
  
  message("Fetching full history for ", sym)
  
  all_data <- list()
  current_start <- start_date
  
  repeat {
    
    message("  Pulling from ", current_start)
    
    klines <- tryCatch({
      binance_klines(
        symbol = sym,
        interval = interval,
        start_time = current_start,
        end_time   = end_date,
        limit = 1000
      )
    }, error = function(e) {
      message("Error for ", sym)
      return(NULL)
    })
    
    if (is.null(klines) || nrow(klines) == 0) break
    
    klines <- klines %>%
      mutate(symbol = sym)
    
    all_data[[length(all_data) + 1]] <- klines
    
    # Get last open_time
    last_time <- max(klines$open_time)
    
    # Move forward by one interval (5 minutes)
    current_start <- last_time + 5 * 60
    
    # Stop if we passed end_date
    if (current_start >= end_date) break
    
    Sys.sleep(0.3)  # avoid rate limits
  }
  
  bind_rows(all_data)
}


start_date <- as_datetime("2021-01-01")
end_date   <- Sys.time()

ohlc_list_5m_full <- lapply(filtered_symbols_df, 
                            fetch_ohlc_full, 
                            start_date, 
                            end_date, 
                            "5m")

ohlc_df_5m_full <- bind_rows(ohlc_list_5m_full)

ohlc_df_5m_full <- ohlc_df_5m_full %>%
  mutate(open_time = as_datetime(open_time)) %>%
  arrange(open_time, symbol)


###quantmod###
coins <- c("DAI-USD", "PAXG-USD", "USDC-USD", "UST-USD", "LUNA-USD")

ticker_exists <- function(symbol) {
  tryCatch({
    getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

results <- sapply(coins, ticker_exists)

data.frame(symbol = coins, exists = results)

#test for available intervals
test_interval <- function(symbol, interval) {
  tryCatch({
    x <- getSymbols(symbol,
                    src = "yahoo",
                    periodicity = "intraday",
                    interval = interval,
                    auto.assign = FALSE)
    paste("SUCCESS:", interval)
  }, error = function(e) {
    paste("FAILED:", interval)
  })
}

intervals <- c("1m","2m","5m","15m","30m","60m","90m")

sapply(intervals, function(i) test_interval("DAI-USD", i))

#all failed, no intraday


###ccxt coinbase###
use_python("C:/Users/65932/anaconda3/python.exe", required = TRUE)
py_config() 
py_install("ccxt", pip = TRUE)
builtins <- import("builtins")

ccxt <- import("ccxt")
coinbase <- ccxt$coinbase() 
binance <- ccxt$binance()

symbol <- "USDT/USD"
timeframe <- "15m"
# 2021-01-01 UTC -> POSIXct
since_r <- as.POSIXct("2021-01-01 00:00:00", tz="UTC")
# convert to integer milliseconds
since <- as.numeric(since_r) * 1000

ohlcv <- coinbase$fetch_ohlcv(symbol, timeframe, since, limit=1000)

desired_pairs <- paste0(coins, "/USD")
markets <- coinbase$load_markets()
ccxt_mkts = desired_pairs[desired_pairs %in% names(markets)]
#coinbase delisted LUNA after the collapse, no UST ever, no USDC

binance$fetch_ohlcv("UST/USDT", timeframe = "15m", 
                    since = since,
                    limit = 1000)
#cannot bc delisted, cannot use API


###coinmarketcapr###
setup(api_key = "bac538d522404968842cca23f9bbf65c")
#only have daily, intraday behind paywall

coin_map <- get_crypto_map()

coins <- c("DAI", "PAXG", "USDC", "UST", "USTC", "LUNA")

# keep only your coins
my_coins <- coin_map[coin_map$symbol %in% coins, ]

my_coins[, c("id", "name", "symbol")]

dai_data <- get_crypto_ohlcv(
  id = my_coins$id[my_coins$symbol == "DAI"],
  convert = "USD",
  interval = "daily",
  time_start = "2021-01-01",
  time_end   = "2024-12-31"
)





###binance vision###
months <- seq(as.Date("2021-12-01"), as.Date("2022-05-01"), by = "month")
symbol <- "USTUSDT"
interval <- "15m"

all_data <- list()

for (m in months) {
  ym_str <- format(m, "%Y-%m")
  url <- sprintf(
    "https://data.binance.vision/data/spot/monthly/klines/%s/%s/%s-%s.csv.gz",
    symbol, interval, symbol, interval, ym_str
  )
  
  tmp <- tryCatch({
    suppressWarnings(
      suppressMessages(
        fread(
          url,
          header = FALSE,
          colClasses = "character",
          showProgress = FALSE,
          verbose = FALSE
        )
      )
    )
  }, error = function(e) {
    message("Failed to download: ", url)
    NULL
  })
  
  if (!is.null(tmp)) {
    colnames(tmp) <- c("open_time","open","high","low","close","volume",
                       "close_time","quote_asset_volume","trades",
                       "taker_buy_base_volume","taker_buy_quote_volume","ignore")
    tmp <- tmp %>%
      mutate(
        open_time = as_datetime(as.numeric(open_time)/1000),
        close_time = as_datetime(as.numeric(close_time)/1000),
        open  = as.numeric(open),
        high  = as.numeric(high),
        low   = as.numeric(low),
        close = as.numeric(close),
        volume = as.numeric(volume),
        symbol = symbol
      )
    all_data[[length(all_data) + 1]] <- tmp
  }
}

ohlc_df <- bind_rows(all_data)


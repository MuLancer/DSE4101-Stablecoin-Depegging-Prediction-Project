# setwd("/Users/paridhiagarwal/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")
library(readr)
library(dplyr)

# get names of all data files
files <- list.files(
  "data/CoinMarketCap pulled data",
  pattern = "\\.csv$",
  full.names = TRUE
)
length(files) # should be 44

# extract stablecoin names
coins <- sub("_1_1_.*", "", basename(files))
unique(coins) # quick check

# group files by coin
files_by_coin <- split(files, coins)

# read and combine yearly files
coin_data <- lapply(files_by_coin, function(f) {
  
  df <- bind_rows(lapply(f, function(x) read_delim(x, delim = ";", show_col_types = FALSE)))
  
  df %>%
    mutate(timeOpen = as.POSIXct(timeOpen)) %>%
    arrange(timeOpen) %>%
    distinct(timeOpen, .keep_all = TRUE)
  
})

# quick sanity checks
names(coin_data)
names(coin_data[["USDC"]])
head(coin_data[["USDC"]])

# save files now
for (coin in names(coin_data)) {
  write_csv(
    coin_data[[coin]],
    file.path("data", coin, paste0(gsub(" ", "_", coin), "_combined.csv"))
  )
}






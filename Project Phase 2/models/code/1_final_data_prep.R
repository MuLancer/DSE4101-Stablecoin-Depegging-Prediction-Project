library(tidyverse)
library(zoo)

rm(list = ls())
set.seed(99)

#################
### Load Data ###
#################
# set wd at Project Phase 2
# setwd("~/DSE4101/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

# Helper to load a CSV and redefine depeg columns
load_and_prep <- function(filepath) {
  read.csv(filepath) %>%
    mutate(date = as.Date(date)) %>%
    mutate(
      # 1-day horizon: does next day's price breach either threshold?
      depeg_1d = ifelse(lead(low, 1) <= ThreshD | lead(high, 1) >= ThreshU, 1, 0),
      
      # 3-day horizon: does the min low / max high over next 3 days breach?
      PL_3 = rollapply(lead(low, 1),  width = 3, FUN = min, align = "left", fill = NA),
      PH_3 = rollapply(lead(high, 1), width = 3, FUN = max, align = "left", fill = NA),
      depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
      
      # 5-day horizon
      PL_5 = rollapply(lead(low, 1),  width = 5, FUN = min, align = "left", fill = NA),
      PH_5 = rollapply(lead(high, 1), width = 5, FUN = max, align = "left", fill = NA),
      depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
      
      # 7-day horizon
      PL_7 = rollapply(lead(low, 1),  width = 7, FUN = min, align = "left", fill = NA),
      PH_7 = rollapply(lead(high, 1), width = 7, FUN = max, align = "left", fill = NA),
      depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)
    ) %>%
    select(-PL_3, -PH_3, -PL_5, -PH_5, -PL_7, -PH_7)
}

# Data from 2019-11-22 to 2025-12-31
data_DAI  <- load_and_prep("data/Dai/DAI_onchain_features.csv")

# Data from 2018-09-27 to 2025-12-31
data_PAX  <- load_and_prep("data/PAX/PAX_onchain_features.csv")

# Data from 2018-10-08 to 2025-12-31
data_USDC <- load_and_prep("data/USDC/USDC_onchain_features.csv")

# Data from 2017-11-27 to 2025-12-31
data_USDT <- load_and_prep("data/USDT/USDT_onchain_features.csv")

# Data from 2020-11-25 to 2022-05-08
data_UST  <- load_and_prep("data/UST/UST_onchain_features.csv")

##################
### Save Files ###
##################

write.csv(data_DAI,  "data/DAI/DAI_onchain_features.csv",  row.names = FALSE)
write.csv(data_PAX,  "data/PAX/PAX_onchain_features.csv",  row.names = FALSE)
write.csv(data_USDC, "data/USDC/USDC_onchain_features.csv", row.names = FALSE)
write.csv(data_USDT, "data/USDT/USDT_onchain_features.csv", row.names = FALSE)
write.csv(data_UST,  "data/UST/UST_onchain_features.csv",  row.names = FALSE)

cat("All 5 datasets saved to data/\n")

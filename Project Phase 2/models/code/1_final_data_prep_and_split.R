library(tidyverse)
library(zoo)

rm(list = ls())
set.seed(99)

#################
### Load Data ###
#################
# set wd at Project Phase 2
# setwd("~/DSE4101/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

data_DAI <- read.csv("data/Dai/DAI_onchain_features.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(depeg_1d = ifelse(lead(low, 1) <= ThreshD | lead(high, 1) >= ThreshU, 1, 0),
         
         PL_3 = rollapply(lead(low, 1), width = 3, FUN = min, align = "left", fill = NA),
         PH_3 = rollapply(lead(high, 1), width = 3, FUN = max, align = "left", fill = NA),
         depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
         
         PL_5 = rollapply(lead(low, 1), width = 5, FUN = min, align = "left", fill = NA),
         PH_5 = rollapply(lead(high, 1), width = 5, FUN = max, align = "left", fill = NA),
         depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
         
         PL_7 = rollapply(lead(low, 1), width = 7, FUN = min, align = "left", fill = NA),
         PH_7 = rollapply(lead(high, 1), width = 7, FUN = max, align = "left", fill = NA),
         depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)) %>%
  select(-PL_3, -PH_3, -PL_5, -PH_5, -PL_7, -PH_7)

data_PAX <- read.csv("data/PAX/PAX_onchain_features.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(depeg_1d = ifelse(lead(low, 1) <= ThreshD | lead(high, 1) >= ThreshU, 1, 0),
         
         PL_3 = rollapply(lead(low, 1), width = 3, FUN = min, align = "left", fill = NA),
         PH_3 = rollapply(lead(high, 1), width = 3, FUN = max, align = "left", fill = NA),
         depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
         
         PL_5 = rollapply(lead(low, 1), width = 5, FUN = min, align = "left", fill = NA),
         PH_5 = rollapply(lead(high, 1), width = 5, FUN = max, align = "left", fill = NA),
         depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
         
         PL_7 = rollapply(lead(low, 1), width = 7, FUN = min, align = "left", fill = NA),
         PH_7 = rollapply(lead(high, 1), width = 7, FUN = max, align = "left", fill = NA),
         depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)) %>%
  select(-PL_3, -PH_3, -PL_5, -PH_5, -PL_7, -PH_7)

data_USDC <- read.csv("data/USDC/USDC_onchain_features.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(depeg_1d = ifelse(lead(low, 1) <= ThreshD | lead(high, 1) >= ThreshU, 1, 0),
         
         PL_3 = rollapply(lead(low, 1), width = 3, FUN = min, align = "left", fill = NA),
         PH_3 = rollapply(lead(high, 1), width = 3, FUN = max, align = "left", fill = NA),
         depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
         
         PL_5 = rollapply(lead(low, 1), width = 5, FUN = min, align = "left", fill = NA),
         PH_5 = rollapply(lead(high, 1), width = 5, FUN = max, align = "left", fill = NA),
         depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
         
         PL_7 = rollapply(lead(low, 1), width = 7, FUN = min, align = "left", fill = NA),
         PH_7 = rollapply(lead(high, 1), width = 7, FUN = max, align = "left", fill = NA),
         depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)) %>%
  select(-PL_3, -PH_3, -PL_5, -PH_5, -PL_7, -PH_7)

data_USDT <- read.csv("data/USDT/USDT_onchain_features.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(depeg_1d = ifelse(lead(low, 1) <= ThreshD | lead(high, 1) >= ThreshU, 1, 0),
         
         PL_3 = rollapply(lead(low, 1), width = 3, FUN = min, align = "left", fill = NA),
         PH_3 = rollapply(lead(high, 1), width = 3, FUN = max, align = "left", fill = NA),
         depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
         
         PL_5 = rollapply(lead(low, 1), width = 5, FUN = min, align = "left", fill = NA),
         PH_5 = rollapply(lead(high, 1), width = 5, FUN = max, align = "left", fill = NA),
         depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
         
         PL_7 = rollapply(lead(low, 1), width = 7, FUN = min, align = "left", fill = NA),
         PH_7 = rollapply(lead(high, 1), width = 7, FUN = max, align = "left", fill = NA),
         depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)) %>%
  select(-PL_3, -PH_3, -PL_5, -PH_5, -PL_7, -PH_7)

data_UST <- read.csv("data/UST/UST_onchain_features.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(depeg_1d = ifelse(lead(low, 1) <= ThreshD | lead(high, 1) >= ThreshU, 1, 0),
         
         PL_3 = rollapply(lead(low, 1), width = 3, FUN = min, align = "left", fill = NA),
         PH_3 = rollapply(lead(high, 1), width = 3, FUN = max, align = "left", fill = NA),
         depeg_3d = ifelse(PL_3 <= ThreshD | PH_3 >= ThreshU, 1, 0),
         
         PL_5 = rollapply(lead(low, 1), width = 5, FUN = min, align = "left", fill = NA),
         PH_5 = rollapply(lead(high, 1), width = 5, FUN = max, align = "left", fill = NA),
         depeg_5d = ifelse(PL_5 <= ThreshD | PH_5 >= ThreshU, 1, 0),
         
         PL_7 = rollapply(lead(low, 1), width = 7, FUN = min, align = "left", fill = NA),
         PH_7 = rollapply(lead(high, 1), width = 7, FUN = max, align = "left", fill = NA),
         depeg_7d = ifelse(PL_7 <= ThreshD | PH_7 >= ThreshU, 1, 0)) %>%
  select(-PL_3, -PH_3, -PL_5, -PH_5, -PL_7, -PH_7)


########################
### Train/Test Split ###
########################

train_test_split <- function(data, train_start, train_end, test_start, test_end) {
  train_start <- as.Date(train_start)
  train_end   <- as.Date(train_end)
  test_start  <- as.Date(test_start)
  test_end    <- as.Date(test_end)
  
  train_data <- data %>% filter(date >= train_start & date <= train_end)
  test_data  <- data %>% filter(date >= test_start  & date <= test_end)
  
  cat("Training data:", nrow(train_data), "observations (",
      min(train_data$date), "to", max(train_data$date), ")\n")
  cat("Test data:", nrow(test_data), "observations (",
      min(test_data$date), "to", max(test_data$date), ")\n")
  
  return(list(train = train_data, test = test_data))
}

# Window 1 coins (includes UST)
coin_dfw1 <- list(DAI = data_DAI, PAX = data_PAX, USDC = data_USDC,
                  USDT = data_USDT, UST = data_UST)

# Window 2 coins (excludes UST — data ends 2022-05-08)
coin_dfw2 <- list(DAI = data_DAI, PAX = data_PAX, USDC = data_USDC,
                  USDT = data_USDT)


##############################################
### Window 1: Train 2020-2021, Test 2022   ###
##############################################

splits_w1 <- list()

for (coin in names(coin_dfw1)) {
  cat("\n--- Window 1:", coin, "---\n")
  splits_w1[[coin]] <- train_test_split(
    coin_dfw1[[coin]],
    train_start = "2020-11-25", train_end = "2021-12-31",
    test_start  = "2022-01-01", test_end  = "2022-05-08"
  )
}


###################################################
### Window 2: Train 2019-2023, Test 2024-2025   ###
###################################################

splits_w2 <- list()

for (coin in names(coin_dfw2)) {
  cat("\n--- Window 2:", coin, "---\n")
  splits_w2[[coin]] <- train_test_split(
    coin_dfw2[[coin]],
    train_start = "2019-11-22", train_end = "2023-12-31",
    test_start  = "2024-01-01", test_end  = "2025-12-31"
  )
}


##################
### Save Files ###
##################

# Window 1
for (coin in names(splits_w1)) {
  write.csv(splits_w1[[coin]]$train,
            file = paste0("models/data/w1_", coin, "_train.csv"), row.names = FALSE)
  write.csv(splits_w1[[coin]]$test,
            file = paste0("models/data/w1_", coin, "_test.csv"),  row.names = FALSE)
  cat("Saved Window 1 train/test for", coin, "\n")
}

# Window 2
for (coin in names(splits_w2)) {
  write.csv(splits_w2[[coin]]$train,
            file = paste0("models/data/w2_", coin, "_train.csv"), row.names = FALSE)
  write.csv(splits_w2[[coin]]$test,
            file = paste0("models/data/w2_", coin, "_test.csv"),  row.names = FALSE)
  cat("Saved Window 2 train/test for", coin, "\n")
}

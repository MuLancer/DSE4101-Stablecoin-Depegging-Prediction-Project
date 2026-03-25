
library(randomForest)
library(xgboost)
library(pls)
library(tidyverse)
library(gridExtra)
library(pROC)
library(smotefamily)

library(scales)
library(zoo)

rm(list=ls())

#################
### Load Data ###
#################

## Working directory should be set to "Project Phase 2/models/code"
## Load in data and convert "date" column into date type (previously character)

set.seed(99)

# Data from 2019-11-22 to 2025-12-31
data_DAI <- read.csv("../../data/Dai/DAI_onchain_features.csv") %>%
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
  select(-PL_3, -PH_3,-PL_5, -PH_5,-PL_7, -PH_7)

# Data from 2018-09-27 to 2025-12-31
data_PAX <- read.csv("../../data/PAX/PAX_onchain_features.csv") %>%
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
  select(-PL_3, -PH_3,-PL_5, -PH_5,-PL_7, -PH_7)

# Data from 2018-10-08 to 2025-12-31
data_USDC <- read.csv("../../data/USDC/USDC_onchain_features.csv") %>%
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
  select(-PL_3, -PH_3,-PL_5, -PH_5,-PL_7, -PH_7)

# Data from 2017-11-27 to 2025-12-31
data_USDT <- read.csv("../../data/USDT/USDT_onchain_features.csv") %>%
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
  select(-PL_3, -PH_3,-PL_5, -PH_5,-PL_7, -PH_7)

# Data from 2020-11-25 to 2022-05-08 
data_UST <- read.csv("../../data/UST/UST_onchain_features.csv") %>%
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
  select(-PL_3, -PH_3,-PL_5, -PH_5,-PL_7, -PH_7)


###############################
### Load relevant functions ###
###############################

# load depeg metrics function
source("func-depeg_metrics.R")

# load in plotting functions
source("func-plots.R")


########################
### Data Preparation ###
########################

train_test_split <-function(data, train_start, train_end, test_start, test_end){
  
  # convert to character to date objects
  train_start <- as.Date(train_start)
  train_end <- as.Date(train_end)
  test_start <- as.Date(test_start)
  test_end <- as.Date(test_end)
  
  # split into train and test datasets
  train_data <- data %>% filter(date >= train_start & date <= train_end)
  test_data <- data %>% filter(date >= test_start & date <= test_end)
  
  # Print split information
  cat("Training data:", nrow(train_data), "observations (", 
      min(train_data$date), "to", max(train_data$date), ")\n")
  cat("Test data:", nrow(test_data), "observations (", 
      min(test_data$date), "to", max(test_data$date), ")\n")
  
  return(list(train = train_data, test = test_data))
}

prep_features <- function(data, target_col, remove_col, smote = FALSE){
  
  # creates a vector of column names
  # should all be numeric features except those excluded
  feature_cols <- names(data)[!names(data) %in% remove_col]
  
  cleaned_data <- data %>% 
    filter(!is.na(!!sym(target_col))) %>%  # remove only rows where the target variable is NA
    drop_na(all_of(feature_cols))          # remove NA in features 
  
  # split into feature matrix and target variable vector
  X <- cleaned_data %>% select(all_of(feature_cols))
  y <- cleaned_data %>% pull(!!sym(target_col)) %>% as.factor()
  
  if(smote){
    # Class balance before and after smote (only for train set)
    cat("TRAIN before SMOTE:\n")
    print(table(y))
    cat("Proportion of depegs:", 
        round(sum(y == 1) / length(y) * 100, 2), "%\n")
    
    # Note: duplicate size - 1 or 2, nearest neighbours - 3 or 5 
    smote_out <- SMOTE(X, as.numeric(y)-1, dup_size = 2, K = 3)
    smote_data <- smote_out$data
    
    X <- smote_data[, 1:ncol(X), drop = FALSE]
    y <- as.factor(smote_data[, ncol(smote_data)])
    
    cat("TRAIN after SMOTE:\n")
    print(table(y))
    cat("Proportion of depegs:", 
        round(sum(y == 1) / length(y) * 100, 2), "%\n")
    
  } else {
    # Class distribution for test set (not using SMOTE)
    cat("\nTEST class distribution", target_col, ":\n")
    print(table(y))
    cat("Proportion of depegs:", 
        round(sum(y == 1) / length(y) * 100, 2), "%\n \n")
  }
  
  return(list(X = X, y = y, dates = cleaned_data$date,
              feature_names = feature_cols))
}

horizons <- c("depeg_1d", "depeg_3d", "depeg_5d", "depeg_7d")

remove_col = c("date", "open", "high", "low", "close", 
               "ThreshD", "ThreshU", "value_classification",
               "depeg_1d", "depeg_3d", "depeg_5d", "depeg_7d",
               "token_name", "close_ust")

coin_dfw1 <- list(DAI = data_DAI, PAX = data_PAX, USDC = data_USDC,
                  USDT = data_USDT, UST = data_UST)

coin_dfw2 <- list(DAI = data_DAI, PAX = data_PAX, USDC = data_USDC,
                  USDT = data_USDT)

##############################################
### Window 1: Train 2020 - 2021, Test 2022 ###
##############################################

w1 <- function(){
  dfw1 <- list()
  
  for(coin in names(coin_dfw1)) {
    cat(paste(rep("=", 10), collapse = ""), "\n")
    cat("Processing:", coin, "\n")
    cat(paste(rep("=", 10), collapse = ""), "\n")
    
    # split each coin into train/test
    dataset <- coin_dfw1[[coin]]
    split <- train_test_split(dataset, 
                              train_start = "2020-11-25", 
                              train_end = "2021-12-31",
                              test_start = "2022-01-01", 
                              test_end = "2022-05-08")
    
    # create a mini list for each horizon (for each coin)
    temp <- list()
    
    for(h in horizons) {
      train_prep <- prep_features(split$train, h, remove_col, smote = TRUE)
      test_prep <- prep_features(split$test, h, remove_col, smote = FALSE)
      
      # Store results
      temp[[h]] <- list(
        train = list(
          X = train_prep$X,
          y = train_prep$y,
          dates = train_prep$dates,
          features = train_prep$feature_names),
        test = list(
          X = test_prep$X,
          y = test_prep$y,
          dates = test_prep$dates,
          features = train_prep$feature_names))
    }
    
    dfw1[[coin]] <- temp
  }
  return(dfw1)
}

dfw1 <- w1()

###################################################
### Window 2: Train 2019 - 2023, Test 2024-2025 ###
###################################################

w2 <- function(){
  dfw2 <- list()
  
  for(coin in names(coin_dfw2)) {
    cat(paste(rep("=", 10), collapse = ""), "\n")
    cat("Processing:", coin, "\n")
    cat(paste(rep("=", 10), collapse = ""), "\n")
    
    # split each coin into train/test
    dataset <- coin_dfw2[[coin]]
    split <- train_test_split(dataset, 
                              train_start = "2019-11-22",
                              train_end = "2023-12-31",
                              test_start = "2024-01-01",
                              test_end = "2025-12-31")
    
    # create a mini list for each horizon (for each coin)
    temp <- list()
    
    for(h in horizons) {
      train_prep <- prep_features(split$train, h, remove_col, smote = TRUE)
      test_prep <- prep_features(split$test, h, remove_col, smote = FALSE)
      
      # Store results
      temp[[h]] <- list(
        train = list(
          X = train_prep$X,
          y = train_prep$y,
          dates = train_prep$dates,
          features = train_prep$feature_names),
        test = list(
          X = test_prep$X,
          y = test_prep$y,
          dates = test_prep$dates,
          features = train_prep$feature_names))
    }
    
    dfw2[[coin]] <- temp
  }
  return(dfw2)
}

dfw2 <- w2()

#####################
### Random Forest ###
#####################
source("func-rf.R")

## WINDOW 1 ====================================================================
# run random forest on all coins for all horizons
rfw1 = run_rf_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)


# ----------- DAI -----------
rfw1_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, rfw1$DAI$depeg_1d$pred_class)
rfw1_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, rfw1$DAI$depeg_3d$pred_class)
rfw1_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, rfw1$DAI$depeg_5d$pred_class)
rfw1_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, rfw1$DAI$depeg_7d$pred_class)

plot_rfw1_DAI_data <- list(depeg_1d = list(test = dfw1$DAI$depeg_1d$test,
                                         pred = rfw1$DAI$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw1$DAI$depeg_3d$test,
                                         pred = rfw1$DAI$depeg_3d$pred_class),
                         depeg_5d = list(test = dfw1$DAI$depeg_5d$test,
                                         pred = rfw1$DAI$depeg_5d$pred_class),
                         depeg_7d = list(test = dfw1$DAI$depeg_7d$test,
                                         pred = rfw1$DAI$depeg_7d$pred_class))
plot_rfw1_DAI <- plot_results(coin_data = plot_rfw1_DAI_data,org_data = data_DAI,
                            title = "Window 1: DAI Depeg Predictions")
plot_rfw1_DAI
                         
# AUC has error here since all of it is just class 0
#auc_rfw1_DAI_1 <- plot_auc(dfw1$DAI$depeg_1d$test$y, rfw1$DAI$depeg_1d$pred_class, 
#                         title = "ROC: DAI depeg_1d",
#                         add_ci = TRUE, add_optimal = TRUE)
# auc_rfw1_DAI_1

auc_rfw1_DAI_3 <- plot_auc(dfw1$DAI$depeg_3d$test$y, rfw1$DAI$depeg_3d$pred_class, 
                         title = "ROC: DAI depeg_3d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_DAI_3

auc_rfw1_DAI_5 <- plot_auc(dfw1$DAI$depeg_5d$test$y, rfw1$DAI$depeg_5d$pred_class, 
                         title = "ROC: DAI depeg_5d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_DAI_5

auc_rfw1_DAI_7 <- plot_auc(dfw1$DAI$depeg_7d$test$y, rfw1$DAI$depeg_7d$pred_class, 
                         title = "ROC: DAI depeg_7d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_DAI_7
# ---------------------------

# ----------- PAX -----------
rfw1_PAX_1_metrics <- depeg_metrics(dfw1$PAX$depeg_1d$test$y, rfw1$PAX$depeg_1d$pred_class)
rfw1_PAX_3_metrics <- depeg_metrics(dfw1$PAX$depeg_3d$test$y, rfw1$PAX$depeg_3d$pred_class)
rfw1_PAX_5_metrics <- depeg_metrics(dfw1$PAX$depeg_5d$test$y, rfw1$PAX$depeg_5d$pred_class)
rfw1_PAX_7_metrics <- depeg_metrics(dfw1$PAX$depeg_7d$test$y, rfw1$PAX$depeg_7d$pred_class)

plot_rfw1_PAX_data <- list(depeg_1d = list(test = dfw1$PAX$depeg_1d$test,
                                         pred = rfw1$PAX$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw1$PAX$depeg_3d$test,
                                         pred = rfw1$PAX$depeg_3d$pred_class),
                         depeg_5d = list(test = dfw1$PAX$depeg_5d$test,
                                         pred = rfw1$PAX$depeg_5d$pred_class),
                         depeg_7d = list(test = dfw1$PAX$depeg_7d$test,
                                         pred = rfw1$PAX$depeg_7d$pred_class))
plot_rfw1_PAX <- plot_results(coin_data = plot_rfw1_PAX_data,org_data = data_PAX,
                            title = "Window 1: PAX Depeg Predictions")
plot_rfw1_PAX

# AUC has error here since all of it is just class 0
# auc_rfw1_PAX_1 <- plot_auc(dfw1$PAX$depeg_1d$test$y, rfw1$PAX$depeg_1d$pred_class, 
#                          title = "ROC: PAX depeg_1d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw1_PAX_1
# 
# auc_rfw1_PAX_3 <- plot_auc(dfw1$PAX$depeg_3d$test$y, rfw1$PAX$depeg_3d$pred_class, 
#                          title = "ROC: PAX depeg_3d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw1_PAX_3
# 
# auc_rfw1_PAX_5 <- plot_auc(dfw1$PAX$depeg_5d$test$y, rfw1$PAX$depeg_5d$pred_class, 
#                          title = "ROC: PAX depeg_5d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw1_PAX_5
# 
# auc_rfw1_PAX_7 <- plot_auc(dfw1$PAX$depeg_7d$test$y, rfw1$PAX$depeg_7d$pred_class, 
#                          title = "ROC: PAX depeg_7d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw1_PAX_7
# ---------------------------

# ----------- USDC -----------
rfw1_USDC_1_metrics <- depeg_metrics(dfw1$USDC$depeg_1d$test$y, rfw1$USDC$depeg_1d$pred_class)
rfw1_USDC_3_metrics <- depeg_metrics(dfw1$USDC$depeg_3d$test$y, rfw1$USDC$depeg_3d$pred_class)
rfw1_USDC_5_metrics <- depeg_metrics(dfw1$USDC$depeg_5d$test$y, rfw1$USDC$depeg_5d$pred_class)
rfw1_USDC_7_metrics <- depeg_metrics(dfw1$USDC$depeg_7d$test$y, rfw1$USDC$depeg_7d$pred_class)

plot_rfw1_USDC_data <- list(depeg_1d = list(test = dfw1$USDC$depeg_1d$test,
                                         pred = rfw1$USDC$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw1$USDC$depeg_3d$test,
                                         pred = fw1$USDC$depeg_3d$pred_class),
                         depeg_5d = list(test = dfw1$USDC$depeg_5d$test,
                                         pred = rfw1$USDC$depeg_5d$pred_class),
                         depeg_7d = list(test = dfw1$USDC$depeg_7d$test,
                                         pred = rfw1$USDC$depeg_7d$pred_class))
plot_rfw1_USDC <- plot_results(coin_data = plot_rfw1_USDC_data,org_data = data_USDC,
                            title = "Window 1: USDC Depeg Predictions")
plot_rfw1_USDC


auc_rfw1_USDC_1 <- plot_auc(dfw1$USDC$depeg_1d$test$y, rfw1$USDC$depeg_1d$pred_class, 
                         title = "ROC: USDC depeg_1d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_USDC_1

auc_rfw1_USDC_3 <- plot_auc(dfw1$USDC$depeg_3d$test$y, rfw1$USDC$depeg_3d$pred_class, 
                         title = "ROC: USDC depeg_3d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_USDC_3

auc_rfw1_USDC_5 <- plot_auc(dfw1$USDC$depeg_5d$test$y, rfw1$USDC$depeg_5d$pred_class, 
                         title = "ROC: USDC depeg_5d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_USDC_5

auc_rfw1_USDC_7 <- plot_auc(dfw1$USDC$depeg_7d$test$y, rfw1$USDC$depeg_7d$pred_class, 
                         title = "ROC: USDC depeg_7d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_USDC_7
# ---------------------------


# ----------- USDT -----------

# ---------------------------


# ----------- UST -----------
rfw1_UST_1_metrics <- depeg_metrics(dfw1$UST$depeg_1d$test$y, rfw1$UST$depeg_1d$pred_class)
rfw1_UST_3_metrics <- depeg_metrics(dfw1$UST$depeg_3d$test$y, rfw1$UST$depeg_3d$pred_class)
rfw1_UST_5_metrics <- depeg_metrics(dfw1$UST$depeg_5d$test$y, rfw1$UST$depeg_5d$pred_class)
rfw1_UST_7_metrics <- depeg_metrics(dfw1$UST$depeg_7d$test$y, rfw1$UST$depeg_7d$pred_class)

plot_rfw1_UST_data <- list(depeg_1d = list(test = dfw1$UST$depeg_1d$test,
                                          pred = rfw1$UST$depeg_1d$pred_class),
                          depeg_3d = list(test = dfw1$UST$depeg_3d$test,
                                          pred = rfw1$UST$depeg_3d$pred_class),
                          depeg_5d = list(test = dfw1$UST$depeg_5d$test,
                                          pred = rfw1$UST$depeg_5d$pred_class),
                          depeg_7d = list(test = dfw1$UST$depeg_7d$test,
                                          pred = rfw1$UST$depeg_7d$pred_class))
plot_rfw1_UST <- plot_results(coin_data = plot_rfw1_UST_data,org_data = data_UST,
                             title = "Window 1: UST Depeg Predictions")
plot_rfw1_UST


auc_rfw1_UST_1 <- plot_auc(dfw1$UST$depeg_1d$test$y, rfw1$UST$depeg_1d$pred_class, 
                          title = "ROC: UST depeg_1d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_UST_1

auc_rfw1_UST_3 <- plot_auc(dfw1$UST$depeg_3d$test$y, rfw1$UST$depeg_3d$pred_class, 
                          title = "ROC: UST depeg_3d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_UST_3

auc_rfw1_UST_5 <- plot_auc(dfw1$UST$depeg_5d$test$y, rfw1$UST$depeg_5d$pred_class, 
                          title = "ROC: UST depeg_5d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_UST_5

auc_rfw1_UST_7 <- plot_auc(dfw1$UST$depeg_7d$test$y, rfw1$UST$depeg_7d$pred_class, 
                          title = "ROC: UST depeg_7d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw1_UST_7
# ---------------------------
# ==============================================================================


## WINDOW 2 ====================================================================
# run random forest on all coins for all horizons
rfw2 = run_rf_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# ----------- DAI -----------
rfw2_DAI_1_metrics <- depeg_metrics(dfw2$DAI$depeg_1d$test$y, rfw2$DAI$depeg_1d$pred_class)
rfw2_DAI_3_metrics <- depeg_metrics(dfw2$DAI$depeg_3d$test$y, rfw2$DAI$depeg_3d$pred_class)
rfw2_DAI_5_metrics <- depeg_metrics(dfw2$DAI$depeg_5d$test$y, rfw2$DAI$depeg_5d$pred_class)
rfw2_DAI_7_metrics <- depeg_metrics(dfw2$DAI$depeg_7d$test$y, rfw2$DAI$depeg_7d$pred_class)

plot_rfw2_DAI_data <- list(depeg_1d = list(test = dfw2$DAI$depeg_1d$test,
                                         pred = rfw2$DAI$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw2$DAI$depeg_3d$test,
                                         pred = rfw2$DAI$depeg_3d$pred_class),
                         depeg_5d = list(test = dfw2$DAI$depeg_5d$test,
                                         pred = rfw2$DAI$depeg_5d$pred_class),
                         depeg_7d = list(test = dfw2$DAI$depeg_7d$test,
                                         pred = rfw2$DAI$depeg_7d$pred_class))
plot_rfw2_DAI <- plot_results(coin_data = plot_rfw2_DAI_data,org_data = data_DAI,
                            title = "Window 2: DAI Depeg Predictions")
plot_rfw2_DAI

# AUC has error here since all of it is just class 0
#auc_rfw2_DAI_1 <- plot_auc(dfw2$DAI$depeg_1d$test$y, dfw2$DAI$depeg_1d$pred_class, 
#                         title = "ROC: DAI depeg_1d",
#                         add_ci = TRUE, add_optimal = TRUE)
# auc_rfw2_DAI_1

auc_rfw2_DAI_3 <- plot_auc(dfw2$DAI$depeg_3d$test$y, dfw2$DAI$depeg_3d$pred_class, 
                         title = "ROC: DAI depeg_3d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_DAI_3

auc_rfw2_DAI_5 <- plot_auc(dfw2$DAI$depeg_5d$test$y, dfw2$DAI$depeg_5d$pred_class, 
                         title = "ROC: DAI depeg_5d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_DAI_5

auc_rfw2_DAI_7 <- plot_auc(dfw2$DAI$depeg_7d$test$y, dfw2$DAI$depeg_7d$pred_class, 
                         title = "ROC: DAI depeg_7d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_DAI_7
# ---------------------------

# ----------- PAX -----------
rfw2_PAX_1_metrics <- depeg_metrics(dfw2$PAX$depeg_1d$test$y, rfw2$PAX$depeg_1d$pred_class)
rfw2_PAX_3_metrics <- depeg_metrics(dfw2$PAX$depeg_3d$test$y, rfw2$PAX$depeg_3d$pred_class)
rfw2_PAX_5_metrics <- depeg_metrics(dfw2$PAX$depeg_5d$test$y, rfw2$PAX$depeg_5d$pred_class)
rfw2_PAX_7_metrics <- depeg_metrics(dfw2$PAX$depeg_7d$test$y, rfw2$PAX$depeg_7d$pred_class)

plot_rfw2_PAX_data <- list(depeg_1d = list(test = dfw2$PAX$depeg_1d$test,
                                         pred = rfw2$PAX$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw2$PAX$depeg_3d$test,
                                         pred = rfw2$PAX$depeg_3d$pred_class),
                         depeg_5d = list(test = dfw2$PAX$depeg_5d$test,
                                         pred = rfw2$PAX$depeg_5d$pred_class),
                         depeg_7d = list(test = dfw2$PAX$depeg_7d$test,
                                         pred = rfw2$PAX$depeg_7d$pred_class))
plot_rfw2_PAX <- plot_results(coin_data = plot_rfw2_PAX_data,org_data = data_PAX,
                            title = "Window 2: PAX Depeg Predictions")
plot_rfw2_PAX

# AUC has error here since all of it is just class 0
# auc_rfw2_PAX_1 <- plot_auc(dfw2$PAX$depeg_1d$test$y, dfw2$PAX$depeg_1d$pred_class, 
#                          title = "ROC: PAX depeg_1d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw2_PAX_1
# 
# auc_rfw2_PAX_3 <- plot_auc(dfw2$PAX$depeg_3d$test$y, dfw2$PAX$depeg_3d$pred_class, 
#                          title = "ROC: PAX depeg_3d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw2_PAX_3
# 
# auc_rfw2_PAX_5 <- plot_auc(dfw2$PAX$depeg_5d$test$y, dfw2$PAX$depeg_5d$pred_class, 
#                          title = "ROC: PAX depeg_5d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw2_PAX_5
# 
# auc_rfw2_PAX_7 <- plot_auc(dfw2$PAX$depeg_7d$test$y, dfw2$PAX$depeg_7d$pred_class, 
#                          title = "ROC: PAX depeg_7d",
#                          add_ci = TRUE, add_optimal = TRUE)
# auc_rfw2_PAX_7
# ---------------------------

# ----------- USDC -----------
rfw2_USDC_1_metrics <- depeg_metrics(dfw2$USDC$depeg_1d$test$y, rfw2$USDC$depeg_1d$pred_class)
rfw2_USDC_3_metrics <- depeg_metrics(dfw2$USDC$depeg_3d$test$y, rfw2$USDC$depeg_3d$pred_class)
rfw2_USDC_5_metrics <- depeg_metrics(dfw2$USDC$depeg_5d$test$y, rfw2$USDC$depeg_5d$pred_class)
rfw2_USDC_7_metrics <- depeg_metrics(dfw2$USDC$depeg_7d$test$y, rfw2$USDC$depeg_7d$pred_class)

plot_rfw2_USDC_data <- list(depeg_1d = list(test = dfw2$USDC$depeg_1d$test,
                                          pred = rfw2$USDC$depeg_1d$pred_class),
                          depeg_3d = list(test = dfw2$USDC$depeg_3d$test,
                                          pred = rfw2$USDC$depeg_3d$pred_class),
                          depeg_5d = list(test = dfw2$USDC$depeg_5d$test,
                                          pred = rfw2$USDC$depeg_5d$pred_class),
                          depeg_7d = list(test = dfw2$USDC$depeg_7d$test,
                                          pred = rfw2$USDC$depeg_7d$pred_class))
plot_rfw2_USDC <- plot_results(coin_data = plot_rfw2_USDC_data,org_data = data_USDC,
                             title = "Window 2: USDC Depeg Predictions")
plot_rfw2_USDC


auc_rfw2_USDC_1 <- plot_auc(dfw2$USDC$depeg_1d$test$y, dfw2$USDC$depeg_1d$pred_class, 
                          title = "ROC: USDC depeg_1d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_USDC_1

auc_rfw2_USDC_3 <- plot_auc(dfw2$USDC$depeg_3d$test$y, dfw2$USDC$depeg_3d$pred_class, 
                          title = "ROC: USDC depeg_3d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_USDC_3

auc_rfw2_USDC_5 <- plot_auc(dfw2$USDC$depeg_5d$test$y, dfw2$USDC$depeg_5d$pred_class, 
                          title = "ROC: USDC depeg_5d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_USDC_5

auc_rfw2_USDC_7 <- plot_auc(dfw2$USDC$depeg_7d$test$y, dfw2$USDC$depeg_7d$pred_class, 
                          title = "ROC: USDC depeg_7d",
                          add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_USDC_7
# ---------------------------


# ----------- USDT -----------

# ---------------------------


# ----------- UST -----------
rfw2_UST_1_metrics <- depeg_metrics(dfw2$UST$depeg_1d$test$y, rfw2$UST$depeg_1d$pred_class)
rfw2_UST_3_metrics <- depeg_metrics(dfw2$UST$depeg_3d$test$y, rfw2$UST$depeg_3d$pred_class)
rfw2_UST_5_metrics <- depeg_metrics(dfw2$UST$depeg_5d$test$y, rfw2$UST$depeg_5d$pred_class)
rfw2_UST_7_metrics <- depeg_metrics(dfw2$UST$depeg_7d$test$y, rfw2$UST$depeg_7d$pred_class)

plot_rfw2_UST_data <- list(depeg_1d = list(test = dfw2$UST$depeg_1d$test,
                                         pred = rfw2$UST$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw2$UST$depeg_3d$test,
                                         pred = rfw2$UST$depeg_3d$pred_class),
                         depeg_5d = list(test = dfw2$UST$depeg_5d$test,
                                         pred = rfw2$UST$depeg_5d$pred_class),
                         depeg_7d = list(test = dfw2$UST$depeg_7d$test,
                                         pred = rfw2$UST$depeg_7d$pred_class))
plot_rfw2_UST <- plot_results(coin_data = plot_rfw2_UST_data,org_data = data_UST,
                            title = "Window 2: UST Depeg Predictions")
plot_rfw2_UST


auc_rfw2_UST_1 <- plot_auc(dfw2$UST$depeg_1d$test$y, dfw2$UST$depeg_1d$pred_class, 
                         title = "ROC: UST depeg_1d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_UST_1

auc_rfw2_UST_3 <- plot_auc(dfw2$UST$depeg_3d$test$y, dfw2$UST$depeg_3d$pred_class, 
                         title = "ROC: UST depeg_3d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_UST_3

auc_rfw2_UST_5 <- plot_auc(dfw2$UST$depeg_5d$test$y, dfw2$UST$depeg_5d$pred_class, 
                         title = "ROC: UST depeg_5d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_UST_5

auc_rfw2_UST_7 <- plot_auc(dfw2$UST$depeg_7d$test$y, dfw2$UST$depeg_7d$pred_class, 
                         title = "ROC: UST depeg_7d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_rfw2_UST_7
# ---------------------------
# ==============================================================================


# ++++++++++++++++++++++++++++++++++++++++++++
#     OLD FEATURE IMPT CODE: DO NOT RUN
# ++++++++++++++++++++++++++++++++++++++++++++

# maybe include a heatmap of feature importance against probability of depeg

plot_rf_list <- list(plot_rf_DAI, plot_rf_PAX, plot_rf_USDC, plot_rf_USDT, plot_rf_UST)
rf_grid <- grid.arrange(grobs = plot_rf_list, nrow = 2, ncol = 3)
ggsave("../../plots/RF_model_OOS.png", rf_grid, width = 12, height = 8)

# ---- RF Feature Importance Block (rolling-window averaged) ----
rf_feature_importance_block <- function(rf_obj, top_n = 20, method = c("auto", "IncNodePurity", "%IncMSE")) {
  method <- match.arg(method)
  
  # rf_obj is the output of rf.rolling.window(), e.g. rf_DAI
  imps <- rf_obj$save.importance
  imps <- imps[!sapply(imps, is.null)]
  if (length(imps) == 0) stop("No importance found in rf_obj$save.importance")
  
  # Each element is a matrix: rows = features, cols = importance metrics
  # Stack them into a 3D-like structure using names intersection
  common_vars <- Reduce(intersect, lapply(imps, rownames))
  if (length(common_vars) == 0) stop("No common variables across importance matrices")
  
  imps <- lapply(imps, function(m) m[common_vars, , drop = FALSE])
  imp_cols <- colnames(imps[[1]])
  
  # Choose method
  chosen <- method
  if (method == "auto") {
    if ("%IncMSE" %in% imp_cols) chosen <- "%IncMSE"
    else if ("IncNodePurity" %in% imp_cols) chosen <- "IncNodePurity"
    else chosen <- imp_cols[1]
  }
  
  # Build matrix [features x iterations] for chosen metric
  M <- sapply(imps, function(m) m[, chosen])
  avg <- rowMeans(M, na.rm = TRUE)
  sdv <- apply(M, 1, sd, na.rm = TRUE)
  
  out <- data.frame(
    feature = names(avg),
    mean_importance = as.numeric(avg),
    sd_importance = as.numeric(sdv),
    stringsAsFactors = FALSE
  )
  
  out <- out[order(out$mean_importance, decreasing = TRUE), ]
  out_top <- head(out, top_n)
  
  list(
    method = chosen,
    table = out_top,
    full_table = out
  )
}


imp_rf_USDT <- rf_feature_importance_block(rf_USDT, top_n = 15)

imp_df <- imp_rf_USDT$table

imp_df$feature <- factor(
  imp_df$feature,
  levels = imp_df$feature[order(imp_df$mean_importance)]
)

plot_imp_rf_USDT <- ggplot(imp_df, aes(x = feature, y = mean_importance)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_importance - sd_importance,
                    ymax = mean_importance + sd_importance),
                width = 0.2) +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance (USDT)",
    subtitle = paste("Mean Permutation Importance (± SD) | Method:", imp_rf_USDT$method),
    x = "",
    y = "Mean Importance"
  ) +
  theme_minimal()

ggsave("../../plots/RF_USDT_feature_importance.png", plot_imp_rf_USDT,
       width = 8, height = 6)

# ++++++++++++++++++++++++++++++++++++++++++++
#              END OF OLD CODE 
# ++++++++++++++++++++++++++++++++++++++++++++




#########################
### Gradient Boosting ###
#########################
source("func-gb.R")

# ===============================================================================
## Window 1: DAI
gb_DAI_1 <- rungb(dfw1$DAI$depeg_1d$train, dfw1$DAI$depeg_1d$test, "DAI depeg_1d")
gb_DAI_3 <- rungb(dfw1$DAI$depeg_3d$train, dfw1$DAI$depeg_3d$test, "DAI depeg_3d")
gb_DAI_5 <- rungb(dfw1$DAI$depeg_5d$train, dfw1$DAI$depeg_5d$test, "DAI depeg_5d")
gb_DAI_7 <- rungb(dfw1$DAI$depeg_7d$train, dfw1$DAI$depeg_7d$test, "DAI depeg_7d")

gb_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, gb_DAI_1$pred_class)
gb_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, gb_DAI_3$pred_class)
gb_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, gb_DAI_5$pred_class)
gb_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, gb_DAI_7$pred_class)

plot_gb_DAI_data <- list(depeg_1d = list(test = dfw1$DAI$depeg_1d$test,
                                         pred = gb_DAI_1$pred_class),
                         depeg_3d = list(test = dfw1$DAI$depeg_3d$test,
                                         pred = gb_DAI_3$pred_class),
                         depeg_5d = list(test = dfw1$DAI$depeg_5d$test,
                                         pred = gb_DAI_5$pred_class),
                         depeg_7d = list(test = dfw1$DAI$depeg_7d$test,
                                         pred = gb_DAI_7$pred_class))
plot_gb_DAI <- plot_results(coin_data = plot_gb_DAI_data,org_data = data_DAI,
                            title = "Window 1: DAI Depeg Predictions")
plot_gb_DAI


# AUC has error here since all of it is just class 0
#auc_gb_DAI_1 <- plot_auc(dfw1$DAI$depeg_1d$test$y, gb_DAI_1$pred_class, 
#                         title = "ROC: DAI depeg_1d",
#                         add_ci = TRUE, add_optimal = TRUE)
# auc_gb_DAI_1

auc_gb_DAI_3 <- plot_auc(dfw1$DAI$depeg_3d$test$y, gb_DAI_3$pred_class, 
                         title = "ROC: DAI depeg_3d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_gb_DAI_3

auc_gb_DAI_5 <- plot_auc(dfw1$DAI$depeg_5d$test$y, gb_DAI_5$pred_class, 
                         title = "ROC: DAI depeg_5d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_gb_DAI_5

auc_gb_DAI_7 <- plot_auc(dfw1$DAI$depeg_7d$test$y, gb_DAI_7$pred_class, 
                         title = "ROC: DAI depeg_7d",
                         add_ci = TRUE, add_optimal = TRUE)
auc_gb_DAI_7
# ===============================================================================

#plot_gb_list <- list(plot_gb_DAI, plot_gb_PAX, plot_gb_USDC, plot_gb_USDT, plot_gb_UST)
#gb_grid <- grid.arrange(grobs = plot_gb_list, nrow = 2, ncol = 3)
#ggsave("../../plots/GB_model_OOS.png", gb_grid, width = 12, height = 8)


######################################
### Principal Component Regression ###
######################################
#source("func-pcr.R") # uncomment for old splitting logic
source("func-pcr_edited.R") 
#source("func-pcr_fixed.R") # uses pls::pcr, no CV (due to Invalid ncomp error)

pcr_DAI = pcr.rolling.window(data_DAI_num,nprev,6,1)
pcr_PAX = pcr.rolling.window(data_PAX_num,nprev,6,1)
pcr_USDC = pcr.rolling.window(data_USDC_num,nprev,6,1)
pcr_USDT = pcr.rolling.window(data_USDT_num,nprev,6,1)
pcr_UST = pcr.rolling.window(data_UST_num,nprev,6,1)

# PCR RMSE's
pcr_rmse_DAI=pcr_DAI$errors[1]
pcr_rmse_PAX=pcr_PAX$errors[1]
pcr_rmse_USDC=pcr_USDC$errors[1]
pcr_rmse_USDT=pcr_USDT$errors[1]
pcr_rmse_UST=pcr_UST$errors[1]

# Directional Predictability
#pcr_depeg_DAI <- depeg_metrics(oos_DAI[[5]], pcr_DAI$pred, threshold)

# Plot OOS results
plot_pcr_DAI <- plot_results(data_DAI, oos_DAI[[8]], nprev, pcr_DAI, pcr_rmse_DAI, 
                             "PCR (DAI Close)")
plot_pcr_PAX <- plot_results(data_PAX, oos_PAX[[8]], nprev, pcr_PAX, pcr_rmse_PAX, 
                             "PCR (PAX Close)")
plot_pcr_USDC <- plot_results(data_USDC, oos_USDC[[8]], nprev, pcr_USDC, pcr_rmse_USDC, 
                              "PCR (USDC Close)")
plot_pcr_USDT <- plot_results(data_USDT, oos_USDT[[8]], nprev, pcr_USDT, pcr_rmse_USDT, 
                              "PCR (USDT Close)")
plot_pcr_UST <- plot_results(data_UST, oos_UST[[8]], nprev, pcr_UST, pcr_rmse_UST, 
                             "PCR (UST Close)")

plot_pcr_list <- list(plot_pcr_DAI, plot_pcr_PAX, plot_pcr_USDC, plot_pcr_USDT, plot_pcr_UST)
pcr_grid <- grid.arrange(grobs = plot_pcr_list, nrow = 2, ncol = 3)
ggsave("../../plots/PCR_model_OOS.png", pcr_grid, width = 12, height = 8)


# Some additional functions: show PCR components, plot CV error curve
pcr_result_DAI <- runpcr(data_DAI_num, indice=6, lag=1)
summary(pcr_result_DAI$model)
print(paste("No. of components for DAI:", pcr_result_DAI$ncomp))
show_pcr_comps(pcr_result_DAI, top_n = 5)
plot_cv_curve(pcr_result_DAI$cv_errors, title = "DAI: PCR Cross-Validation Error")

pcr_result_PAX <- runpcr(data_PAX_num, indice=6, lag=1)
summary(pcr_result_PAX$model)
print(paste("No. of components for PAX:", pcr_result_PAX$ncomp))
show_pcr_comps(pcr_result_PAX, top_n = 5)
plot_cv_curve(pcr_result_PAX$cv_errors, title = "PAX: PCR Cross-Validation Error")

pcr_result_USDC <- runpcr(data_USDC_num, indice=6, lag=1)
summary(pcr_result_USDC$model)
print(paste("No. of components for USDC:", pcr_result_USDC$ncomp))
show_pcr_comps(pcr_result_USDC, top_n = 5)
plot_cv_curve(pcr_result_USDC$cv_errors, title = "USDC: PCR Cross-Validation Error")

pcr_result_USDT <- runpcr(data_USDT_num, indice=6, lag=1)
summary(pcr_result_USDT$model)
print(paste("No. of components for USDT:", pcr_result_USDT$ncomp))
show_pcr_comps(pcr_result_USDT, top_n = 5)
plot_cv_curve(pcr_result_USDT$cv_errors, title = "USDT: PCR Cross-Validation Error")

pcr_result_UST <- runpcr(data_UST_num, indice=6, lag=1)
summary(pcr_result_UST$model)
print(paste("No. of components for UST:", pcr_result_UST$ncomp))
show_pcr_comps(pcr_result_UST, top_n = 5)
plot_cv_curve(pcr_result_UST$cv_errors, title = "UST: PCR Cross-Validation Error")

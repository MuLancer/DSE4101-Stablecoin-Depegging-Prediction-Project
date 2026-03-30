library(randomForest)
library(xgboost)
library(pls)
library(tidyverse)
library(gridExtra)
library(pROC)
library(smotefamily)
library(scales)
library(zoo)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(forcats)
library(tidytext)

rm(list=ls())
set.seed(99)

#################
### Load Data ###
#################

## Working directory should be set to "Project Phase 2/models/code"
## Load in preprocessed data (depeg labels already defined in data_prep.R)

data_DAI  <- read.csv("../../data/DAI/DAI_onchain_features.csv")  %>% mutate(date = as.Date(date))
data_PAX  <- read.csv("../../data/PAX/PAX_onchain_features.csv")  %>% mutate(date = as.Date(date))
data_USDC <- read.csv("../../data/USDC/USDC_onchain_features.csv") %>% mutate(date = as.Date(date))
data_USDT <- read.csv("../../data/USDT/USDT_onchain_features.csv") %>% mutate(date = as.Date(date))
data_UST  <- read.csv("../../data/UST/UST_onchain_features.csv")  %>% mutate(date = as.Date(date))


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
    cat("TRAIN before SMOTE", target_col, ":\n")
    print(table(y))
    cat("Proportion of depegs:", 
        round(sum(y == 1) / length(y) * 100, 2), "%\n")
    
    # Note: duplicate size - 1 or 2, nearest neighbours - 3 or 5 
    smote_out <- SMOTE(X, as.numeric(y)-1, dup_size = 1, K = 3)
    smote_data <- smote_out$data
    
    X <- smote_data[, 1:ncol(X), drop = FALSE]
    y <- as.factor(smote_data[, ncol(smote_data)])
    
    cat("TRAIN after SMOTE", target_col, ":\n")
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
                              train_end = "2021-11-25",
                              test_start = "2021-11-26", 
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
                              train_end = "2022-12-31",
                              test_start = "2023-01-01",
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
rfw1 = runrf_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
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
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
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
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
rfw1_USDC_1_metrics <- depeg_metrics(dfw1$USDC$depeg_1d$test$y, rfw1$USDC$depeg_1d$pred_class)
rfw1_USDC_3_metrics <- depeg_metrics(dfw1$USDC$depeg_3d$test$y, rfw1$USDC$depeg_3d$pred_class)
rfw1_USDC_5_metrics <- depeg_metrics(dfw1$USDC$depeg_5d$test$y, rfw1$USDC$depeg_5d$pred_class)
rfw1_USDC_7_metrics <- depeg_metrics(dfw1$USDC$depeg_7d$test$y, rfw1$USDC$depeg_7d$pred_class)

plot_rfw1_USDC_data <- list(depeg_1d = list(test = dfw1$USDC$depeg_1d$test,
                                         pred = rfw1$USDC$depeg_1d$pred_class),
                         depeg_3d = list(test = dfw1$USDC$depeg_3d$test,
                                         pred = rfw1$USDC$depeg_3d$pred_class),
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
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- UST ----------------------------------------
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
# ------------------------------------------------------------------------------
# ==============================================================================


## WINDOW 2 ====================================================================
# run random forest on all coins for all horizons
rfw2 = runrf_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
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

auc_rfw2_DAI_3 <- plot_auc(dfw2$DAI$depeg_3d$pred_class, dfw2$DAI$depeg_3d$pred_class, 
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
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
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
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
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
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------
# ==============================================================================


############################################################
### LOG-LOSS PERMUTATION FEATURE IMPORTANCE FOR RF / GB ####
############################################################

########################
### 1) Helper funcs ####
########################

# Binary log loss
log_loss_bin <- function(y_true, p_hat, eps = 1e-15) {
  # Convert y to numeric 0/1 robustly
  if (is.factor(y_true)) {
    y_num <- as.numeric(as.character(y_true))
  } else {
    y_num <- as.numeric(y_true)
  }
  
  p_hat <- pmin(pmax(p_hat, eps), 1 - eps)
  -mean(y_num * log(p_hat) + (1 - y_num) * log(1 - p_hat))
}

# Safely extract fitted model object
get_model_obj <- function(model_entry) {
  # Most likely your fitted object is stored as $model
  if (!is.null(model_entry$model)) return(model_entry$model)
  if (!is.null(model_entry$fit)) return(model_entry$fit)
  stop("Could not find fitted model object. Expected $model or $fit.")
}

# Robust probability prediction for binary class = 1
# This tries a few common cases:
# - randomForest/ranger/caret style: predict(..., type='prob')
# - xgboost style: predict(model, matrix)
# - glm style: predict(..., type='response')
get_prob_pred <- function(model_obj, X_new) {
  
  # xgboost-style models often need a matrix
  try_xgb <- try({
    p <- predict(model_obj, as.matrix(X_new))
    if (is.numeric(p) && length(p) == nrow(X_new)) return(as.numeric(p))
    NULL
  }, silent = TRUE)
  if (!inherits(try_xgb, "try-error") && !is.null(try_xgb)) return(try_xgb)
  
  # probability output
  try_prob <- try({
    p <- predict(model_obj, newdata = X_new, type = "prob")
    
    # if matrix/data.frame with class columns
    if (is.matrix(p) || is.data.frame(p)) {
      cn <- colnames(p)
      if (!is.null(cn)) {
        if ("1" %in% cn) return(as.numeric(p[, "1"]))
        if ("yes" %in% tolower(cn)) return(as.numeric(p[, which(tolower(cn) == "yes")[1]]))
        if ("true" %in% tolower(cn)) return(as.numeric(p[, which(tolower(cn) == "true")[1]]))
      }
      # fallback to second column if 2-class output
      if (ncol(p) == 2) return(as.numeric(p[, 2]))
    }
    
    # if vector already
    if (is.numeric(p) && length(p) == nrow(X_new)) return(as.numeric(p))
    NULL
  }, silent = TRUE)
  if (!inherits(try_prob, "try-error") && !is.null(try_prob)) return(try_prob)
  
  # response output (e.g. glm)
  try_resp <- try({
    p <- predict(model_obj, newdata = X_new, type = "response")
    if (is.numeric(p) && length(p) == nrow(X_new)) return(as.numeric(p))
    NULL
  }, silent = TRUE)
  if (!inherits(try_resp, "try-error") && !is.null(try_resp)) return(try_resp)
  
  stop("Could not get predicted probabilities from model.")
}

# Permutation importance for one fitted model entry
perm_importance_logloss_one <- function(model_entry, test_X, test_y,
                                        n_repeats = 10, seed = 123) {
  set.seed(seed)
  
  model_obj <- get_model_obj(model_entry)
  
  # baseline log loss
  p_base <- get_prob_pred(model_obj, test_X)
  base_ll <- log_loss_bin(test_y, p_base)
  
  feats <- colnames(test_X)
  
  imp_tbl <- lapply(feats, function(feat) {
    perm_ll <- numeric(n_repeats)
    
    for (r in seq_len(n_repeats)) {
      X_perm <- test_X
      X_perm[[feat]] <- sample(X_perm[[feat]], replace = FALSE)
      p_perm <- get_prob_pred(model_obj, X_perm)
      perm_ll[r] <- log_loss_bin(test_y, p_perm)
    }
    
    data.frame(
      feature = feat,
      baseline_logloss = base_ll,
      perm_logloss_mean = mean(perm_ll),
      perm_logloss_sd = sd(perm_ll),
      importance_logloss = mean(perm_ll) - base_ll
    )
  }) %>% bind_rows()
  
  imp_tbl %>% arrange(desc(importance_logloss))
}

############################################
### 2) Run for all coins and all horizons ###
############################################

# model_results: e.g. rfw1, rfw2, gbw1, gbw2
# dfw: e.g. dfw1, dfw2
# coin_list: vector of coin names, e.g. names(dfw1)
# horizons: e.g. c("depeg_1d","depeg_3d","depeg_5d","depeg_7d")

run_logloss_importance_all <- function(model_results, dfw, coin_list, horizons,
                                       n_repeats = 10, seed = 123,
                                       model_name = "RF", window_name = "Window 1") {
  
  out <- list()
  k <- 1
  
  for (coin in coin_list) {
    for (h in horizons) {
      cat("Running:", model_name, "|", window_name, "|", coin, "|", h, "\n")
      
      test_X <- dfw[[coin]][[h]]$test$X
      test_y <- dfw[[coin]][[h]]$test$y
      model_entry <- model_results[[coin]][[h]]
      
      imp_tbl <- perm_importance_logloss_one(
        model_entry = model_entry,
        test_X = test_X,
        test_y = test_y,
        n_repeats = n_repeats,
        seed = seed
      )
      
      imp_tbl$model <- model_name
      imp_tbl$window <- window_name
      imp_tbl$coin <- coin
      imp_tbl$horizon <- h
      
      out[[k]] <- imp_tbl
      k <- k + 1
    }
  }
  
  bind_rows(out) %>%
    select(model, window, coin, horizon, feature,
           baseline_logloss, perm_logloss_mean, perm_logloss_sd, importance_logloss)
}

#####################################################
### 3) Summaries for plotting / reporting ###########
#####################################################

# Mean importance by coin-horizon-feature
summarise_importance <- function(imp_all) {
  imp_all %>%
    group_by(model, window, coin, horizon, feature) %>%
    summarise(
      mean_importance = mean(importance_logloss, na.rm = TRUE),
      sd_importance = sd(importance_logloss, na.rm = TRUE),
      .groups = "drop"
    )
}

# Top N features within each coin/horizon
get_top_features_each_group <- function(imp_summary, top_n = 10) {
  imp_summary %>%
    group_by(model, window, coin, horizon) %>%
    slice_max(order_by = mean_importance, n = top_n, with_ties = FALSE) %>%
    ungroup()
}

###################################
### 4) Plotting helper functions ###
###################################

# A) Heatmap: average importance by coin x horizon
plot_importance_heatmap <- function(imp_all, model_name = NULL, window_name = NULL,
                                    top_n_features = 15) {
  df <- imp_all
  
  if (!is.null(model_name)) df <- df %>% filter(model == model_name)
  if (!is.null(window_name)) df <- df %>% filter(window == window_name)
  
  # keep globally important features to reduce clutter
  keep_feats <- df %>%
    group_by(feature) %>%
    summarise(global_imp = mean(importance_logloss, na.rm = TRUE), .groups = "drop") %>%
    slice_max(order_by = global_imp, n = top_n_features, with_ties = FALSE) %>%
    pull(feature)
  
  plot_df <- df %>%
    filter(feature %in% keep_feats) %>%
    group_by(model, window, coin, horizon, feature) %>%
    summarise(mean_importance = mean(importance_logloss, na.rm = TRUE), .groups = "drop") %>%
    mutate(coin_horizon = paste(coin, horizon, sep = " | "))
  
  ggplot(plot_df, aes(x = fct_reorder(feature, mean_importance, .fun = mean),
                      y = fct_rev(coin_horizon),
                      fill = mean_importance)) +
    geom_tile() +
    labs(
      title = "Log-loss permutation importance heatmap",
      subtitle = paste0(
        ifelse(is.null(model_name), "All models", model_name), " | ",
        ifelse(is.null(window_name), "All windows", window_name)
      ),
      x = "Feature",
      y = "Coin | Horizon",
      fill = "Δ Log Loss"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# B) Bar plot: top features for one coin/horizon
plot_top_features_one <- function(imp_all, coin_name, horizon_name,
                                  model_name, window_name, top_n = 15) {
  
  plot_df <- imp_all %>%
    filter(
      coin == coin_name,
      horizon == horizon_name,
      model == model_name,
      window == window_name
    ) %>%
    group_by(feature) %>%
    summarise(
      mean_importance = mean(importance_logloss, na.rm = TRUE),
      sd_importance = sd(importance_logloss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    slice_max(order_by = mean_importance, n = top_n, with_ties = FALSE) %>%
    mutate(feature = fct_reorder(feature, mean_importance))
  
  ggplot(plot_df, aes(x = feature, y = mean_importance)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean_importance - sd_importance,
                      ymax = mean_importance + sd_importance),
                  width = 0.2) +
    coord_flip() +
    labs(
      title = paste("Top log-loss feature importance:", coin_name, horizon_name),
      subtitle = paste(model_name, "|", window_name),
      x = NULL,
      y = "Mean Δ Log Loss"
    ) +
    theme_minimal()
}

# C) Faceted plot: top features by coin/horizon
plot_top_features_faceted <- function(imp_all, model_name, window_name, top_n = 8) {
  
  plot_df <- imp_all %>%
    filter(model == model_name, window == window_name) %>%
    group_by(coin, horizon, feature) %>%
    summarise(mean_importance = mean(importance_logloss, na.rm = TRUE), .groups = "drop") %>%
    group_by(coin, horizon) %>%
    slice_max(order_by = mean_importance, n = top_n, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(panel = paste(coin, horizon, sep = " | "),
           feature_panel = paste(feature, panel, sep = "___"))
  
  # reorder within panel
  plot_df <- plot_df %>%
    group_by(panel) %>%
    mutate(feature_panel = fct_reorder(feature_panel, mean_importance)) %>%
    ungroup()
  
  ggplot(plot_df, aes(x = feature_panel, y = mean_importance)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ panel, scales = "free_y") +
    scale_x_discrete(labels = function(x) str_replace(x, "___.*$", "")) +
    labs(
      title = "Top log-loss feature importance by coin and horizon",
      subtitle = paste(model_name, "|", window_name),
      x = NULL,
      y = "Mean Δ Log Loss"
    ) +
    theme_minimal()
}

##########################################
### 5)Run on RF ###
##########################################

# coin vectors
coins_w1 <- names(dfw1)
coins_w2 <- names(dfw2)

# horizons <- c("depeg_1d", "depeg_3d", "depeg_5d", "depeg_7d")

# -----------------------------
# Random Forest - Window 1
# -----------------------------
rf_logloss_imp_w1 <- run_logloss_importance_all(
  model_results = rfw1,
  dfw = dfw1,
  coin_list = coins_w1,
  horizons = horizons,
  n_repeats = 10,
  seed = 123,
  model_name = "RF",
  window_name = "Window 1"
)

# -----------------------------
# Random Forest - Window 2
# -----------------------------
rf_logloss_imp_w2 <- run_logloss_importance_all(
  model_results = rfw2,
  dfw = dfw2,
  coin_list = coins_w2,
  horizons = horizons,
  n_repeats = 10,
  seed = 123,
  model_name = "RF",
  window_name = "Window 2"
)

# combine RF
rf_logloss_imp_all <- bind_rows(rf_logloss_imp_w1, rf_logloss_imp_w2)

write.csv(rf_logloss_imp_all, "../../plots/rf_logloss_importance_all.csv", row.names = FALSE)

##########################################
### 6) run on GB / XGBoost #####
##########################################
gb_logloss_imp_w1 <- run_logloss_importance_all(
  model_results = gbw1,
  dfw = dfw1,
  coin_list = coins_w1,
  horizons = horizons,
  n_repeats = 10,
  seed = 123,
  model_name = "GB",
  window_name = "Window 1"
)

gb_logloss_imp_w2 <- run_logloss_importance_all(
  model_results = gbw2,
  dfw = dfw2,
  coin_list = coins_w2,
  horizons = horizons,
  n_repeats = 10,
  seed = 123,
  model_name = "GB",
  window_name = "Window 2"
)

gb_logloss_imp_all <- bind_rows(gb_logloss_imp_w1, gb_logloss_imp_w2)
write.csv(gb_logloss_imp_all, "../../plots/gb_logloss_importance_all.csv", row.names = FALSE)

#################################
### 7) plots #####
#################################

# ---- RF heatmap, window 1 ----
p_rf_heat_w1 <- plot_importance_heatmap(
  imp_all = rf_logloss_imp_w1,
  model_name = "RF",
  window_name = "Window 1",
  top_n_features = 15
)
p_rf_heat_w1
ggsave("../../plots/RF_logloss_importance_heatmap_w1.png", p_rf_heat_w1, width = 12, height = 8)

# ---- RF heatmap, window 2 ----
p_rf_heat_w2 <- plot_importance_heatmap(
  imp_all = rf_logloss_imp_all,
  model_name = "RF",
  window_name = "Window 2",
  top_n_features = 15
)
p_rf_heat_w2
ggsave("../../plots/RF_logloss_importance_heatmap_w2.png", p_rf_heat_w2, width = 12, height = 8)

# ---- Example: one coin/horizon bar plot ----
p_rf_dai_3d_w1 <- plot_top_features_one(
  imp_all = rf_logloss_imp_w1,
  coin_name = "DAI",
  horizon_name = "depeg_3d",
  model_name = "RF",
  window_name = "Window 1",
  top_n = 15
)
p_rf_dai_3d_w1
ggsave("../../plots/RF_DAI_depeg3d_logloss_importance_w1.png", p_rf_dai_3d_w1, width = 8, height = 6)

# ---- Faceted plot across all coin/horizon combos ----
p_rf_faceted_w1 <- plot_top_features_faceted(
  imp_all = rf_logloss_imp_all,
  model_name = "RF",
  window_name = "Window 1",
  top_n = 8
)
p_rf_faceted_w1
ggsave("../../plots/RF_logloss_importance_faceted_w1.png", p_rf_faceted_w1, width = 14, height = 10)

p_rf_faceted_w2 <- plot_top_features_faceted(
  imp_all = rf_logloss_imp_all,
  model_name = "RF",
  window_name = "Window 2",
  top_n = 8
)
p_rf_faceted_w2
ggsave("../../plots/RF_logloss_importance_faceted_w2.png", p_rf_faceted_w2, width = 14, height = 10)

# One plot per coin, 4 subplots for each horizon (1d/3d/5d/7d), top 10 features each
plot_coin_feature_importance <- function(imp_data, 
                                         coin_name,
                                         model_name = "RF", 
                                         window_name = "Window 1",
                                         top_n = 10) {
  
  plot_df <- imp_data %>%
    filter(model == model_name, window == window_name, coin == coin_name) %>%
    group_by(horizon, feature) %>%
    summarise(mean_importance = mean(importance_logloss, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(horizon) %>%
    slice_max(order_by = mean_importance, n = top_n, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      horizon_label = case_when(
        horizon == "depeg_1d" ~ "1 Day",
        horizon == "depeg_3d" ~ "3 Day",
        horizon == "depeg_5d" ~ "5 Day",
        horizon == "depeg_7d" ~ "7 Day"
      ),
      horizon_label = factor(horizon_label, 
                             levels = c("1 Day", "3 Day", "5 Day", "7 Day"))
    )
  
  ggplot(plot_df, aes(x = reorder_within(feature, mean_importance, horizon), 
                      y = mean_importance)) +
    geom_col(fill = "#2E86AB") +
    coord_flip() +
    facet_wrap(~ horizon_label, nrow = 2, scales = "free_y") +
    scale_x_reordered() +
    labs(
      title = paste(coin_name, "Top", top_n, "Feature Importance by Horizon"),
      subtitle = paste(model_name, "|", window_name),
      x = NULL,
      y = "Mean Log Loss Increase"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      strip.text = element_text(size = 11, face = "bold"),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(size = 14, face = "bold")
    )
}

# ============================================
# Generate plots for all coins
# ============================================

# RF Window 1
for (coin in c("DAI", "PAX", "USDC", "USDT", "UST")) {
  p <- plot_coin_feature_importance(rf_logloss_imp_all, coin, "RF", "Window 1", top_n = 10)
  print(p)
  ggsave(paste0("../../plots/feature_importance/RF_", coin, "_feature_importance_w1.png"), p, width = 10, height = 8)
}

# RF Window 2 (no UST)
for (coin in c("DAI", "PAX", "USDC", "USDT")) {
  p <- plot_coin_feature_importance(rf_logloss_imp_all, coin, "RF", "Window 2", top_n = 10)
  print(p)
  ggsave(paste0("../../plots/feature_importance/RF_", coin, "_feature_importance_w2.png"), p, width = 10, height = 8)
}


###########################################
### 8) Quick summary tables for report ####
###########################################

# Average importance across all coins/horizons
rf_global_feature_rank <- rf_logloss_imp_all %>%
  group_by(model, window, feature) %>%
  summarise(mean_importance = mean(importance_logloss, na.rm = TRUE), .groups = "drop") %>%
  arrange(model, window, desc(mean_importance))

print(head(rf_global_feature_rank, 20))

# Top 10 per coin/horizon
rf_top10_by_group <- rf_logloss_imp_all %>%
  group_by(model, window, coin, horizon) %>%
  summarise(mean_importance = mean(importance_logloss, na.rm = TRUE), .by = c(model, window, coin, horizon, feature)) %>%
  group_by(model, window, coin, horizon) %>%
  slice_max(order_by = mean_importance, n = 10, with_ties = FALSE) %>%
  ungroup()

write.csv(rf_top10_by_group, "../../plots/rf_top10_logloss_importance_by_group.csv", row.names = FALSE)

#########################
### Gradient Boosting ###
#########################
source("func-gb.R")

## WINDOW 1 ====================================================================
# run XGBoost on all coins for all horizons
gbw1 = rungb_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- UST ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## WINDOW 2 ====================================================================
# run XGBoost on all coins for all horizons
gbw2 = rungb_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


#plot_gb_list <- list(plot_gb_DAI, plot_gb_PAX, plot_gb_USDC, plot_gb_USDT, plot_gb_UST)
#gb_grid <- grid.arrange(grobs = plot_gb_list, nrow = 2, ncol = 3)
#ggsave("../../plots/GB_model_OOS.png", gb_grid, width = 12, height = 8)


######################################
### Principal Component Regression ###
######################################
source("func-pcr.R") 

# Note warnings: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
# occurs when some PCs predict the class exactly (overfit)
# happens due to small sample size and class imbalance (depeg rare)

## WINDOW 1 ====================================================================
# run PCR on all coins for all horizons
pcrw1 = runpcr_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- UST ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## WINDOW 2 ====================================================================
# run PCR on all coins for all horizons
pcrw2 = runpcr_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## Some additional functions: show PCR components, plot CV error curve
summary(pcrw1$DAI$depeg_1d$model)
cat("Optimal No. of components for DAI 1d:", pcrw1$DAI$depeg_1d$ncomp, "\n")
show_pcr_comps(pcrw1$DAI$depeg_1d, top_n = 5)
plot_cv_curve(pcrw1$DAI$depeg_1d$cv_errors, title = "DAI 1d: PCR Cross-Validation Error")

summary(pcrw1$UST$depeg_5d$model)
cat("Optimal No. of components for UST 5d:", pcrw1$UST$depeg_5d$ncomp, "\n")
show_pcr_comps(pcrw1$UST$depeg_5d, top_n = 5)
plot_cv_curve(pcrw1$UST$depeg_5d$cv_errors, title = "UST 5d: PCR Cross-Validation Error")

summary(pcrw1$UST$depeg_7d$model)
cat("Optimal No. of components for UST 7d:", pcrw1$UST$depeg_7d$ncomp, "\n")
show_pcr_comps(pcrw1$UST$depeg_7d, top_n = 5)
plot_cv_curve(pcrw1$UST$depeg_7d$cv_errors, title = "UST 7d: PCR Cross-Validation Error")


# plot_pcr_list <- list(plot_pcr_DAI, plot_pcr_PAX, plot_pcr_USDC, plot_pcr_USDT, plot_pcr_UST)
# pcr_grid <- grid.arrange(grobs = plot_pcr_list, nrow = 2, ncol = 3)
# ggsave("../../plots/PCR_model_OOS.png", pcr_grid, width = 12, height = 8)


#############################
### Partial Least Squares ###
#############################
source("func-pls.R") 

## WINDOW 1 ====================================================================
# run PLS on all coins for all horizons
plsw1 = runpls_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- UST ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## WINDOW 2 ====================================================================
# run PLS on all coins for all horizons
plsw2 = runpls_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## Some additional functions: show PLS components
summary(plsw1$DAI$depeg_1d$model)
cat("Optimal No. of components for DAI 1d:", plsw1$DAI$depeg_1d$ncomp, "\n")
show_pls_comps(plsw1$DAI$depeg_1d, top_n = 5)

summary(plsw1$UST$depeg_5d$model)
cat("Optimal No. of components for UST 5d:", plsw1$UST$depeg_5d$ncomp, "\n")
show_pls_comps(plsw1$UST$depeg_5d, top_n = 5)

summary(plsw1$UST$depeg_7d$model)
cat("Optimal No. of components for UST 7d:", plsw1$UST$depeg_7d$ncomp, "\n")
show_pls_comps(plsw1$UST$depeg_7d, top_n = 5)


###########################
### Logistic Regression ###
###########################
source("func-logit.R")

# Note warnings: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
# occurs when some PCs predict the class exactly (overfit)
# happens due to small sample size and class imbalance (depeg rare)

## WINDOW 1 ====================================================================
# run Logit on all coins for all horizons
logitw1 = runlogit_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
logit_DAI_1_metrics1 <- depeg_metrics(logitw1$DAI$depeg_1d$test$y, logitw1$DAI$depeg_1d$pred_class)
logit_DAI_3_metrics1 <- depeg_metrics(logitw1$DAI$depeg_3d$test$y, logitw1$DAI$depeg_3d$pred_class)
logit_DAI_5_metrics1 <- depeg_metrics(logitw1$DAI$depeg_5d$test$y, logitw1$DAI$depeg_5d$pred_class)
logit_DAI_7_metrics1 <- depeg_metrics(logitw1$DAI$depeg_7d$test$y, logitw1$DAI$depeg_7d$pred_class)

plot_logit_DAI_w1_data <- list(depeg_1d = list(test = dfw1$DAI$depeg_1d$test,
                                               pred = logitw1$DAI$depeg_1d$pred_class),
                               depeg_3d = list(test = dfw1$DAI$depeg_3d$test,
                                               pred = logitw1$DAI$depeg_3d$pred_class),
                               depeg_5d = list(test = dfw1$DAI$depeg_5d$test,
                                               pred = logitw1$DAI$depeg_5d$pred_class),
                               depeg_7d = list(test = dfw1$DAI$depeg_7d$test,
                                               pred = logitw1$DAI$depeg_7d$pred_class))

plot_logit_DAI_w1 <- plot_results(plot_logit_DAI_w1_data,org_data = data_DAI,
                                  title = "Window 1: DAI Depeg Predictions")
plot_logit_DAI_w1

#comparing performance across horizons 
plot_horizon_performance(logitw1$DAI$depeg_1d, logitw1$DAI$depeg_3d, logitw1$DAI$depeg_5d, logitw1$DAI$depeg_7d)

#looking at top 10 coeffs (not stable, with odds ratio close to 0/+inf)
show_logit_coeffs(logitw1$DAI$depeg_1d, 10)
show_logit_coeffs(logitw1$DAI$depeg_3d, 10)
show_logit_coeffs(logitw1$DAI$depeg_5d, 10)
show_logit_coeffs(logitw1$DAI$depeg_7d, 10)

# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- UST ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## WINDOW 2 ====================================================================
# run Logit on all coins for all horizons
logitw2 = runlogit_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
logit_DAI_1_metrics2 <- depeg_metrics(logitw2$DAI$depeg_1d$test$y, logitw2$DAI$depeg_1d$pred_class)
logit_DAI_3_metrics2 <- depeg_metrics(logitw2$DAI$depeg_3d$test$y, logitw2$DAI$depeg_3d$pred_class)
logit_DAI_5_metrics2 <- depeg_metrics(logitw2$DAI$depeg_5d$test$y, logitw2$DAI$depeg_5d$pred_class)
logit_DAI_7_metrics2 <- depeg_metrics(logitw2$DAI$depeg_7d$test$y, logitw2$DAI$depeg_7d$pred_class)

plot_logit_DAI_w2_data <- list(depeg_1d = list(test = dfw2$DAI$depeg_1d$test,
                                               pred = logitw2$DAI$depeg_1d$pred_class),
                               depeg_3d = list(test = dfw2$DAI$depeg_3d$test,
                                               pred = logitw2$DAI$depeg_3d$pred_class),
                               depeg_5d = list(test = dfw2$DAI$depeg_5d$test,
                                               pred = logitw2$DAI$depeg_5d$pred_class),
                               depeg_7d = list(test = dfw2$DAI$depeg_7d$test,
                                               pred = logitw2$DAI$depeg_7d$pred_class))

plot_logit_DAI_w2 <- plot_results(plot_logit_DAI_w2_data,org_data = data_DAI,
                                  title = "Window 2: DAI Depeg Predictions")
plot_logit_DAI_w2

#comparing performance across horizons 
plot_horizon_performance(logitw2$DAI$depeg_1d, logitw2$DAI$depeg_3d, logitw2$DAI$depeg_5d, logitw2$DAI$depeg_7d)

#looking at top 10 coeffs (not stable, with odds ratio close to 0/+inf)
show_logit_coeffs(logitw2$DAI$depeg_1d, 10)
show_logit_coeffs(logitw2$DAI$depeg_3d, 10)
show_logit_coeffs(logitw2$DAI$depeg_5d, 10)
show_logit_coeffs(logitw2$DAI$depeg_7d, 10)
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


#############
### LASSO ###
#############
source("func-lasso.R") 

# Note: glmnet can issue convergence warnings for some low-lambda fits.
# These warnings usually affect only the least-regularized part of the path, while
# the stronger-penalty solutions remain valid and are still returned by cv.glmnet().


## WINDOW 1 ====================================================================
# run LASSO on all coins for all horizons
lassow1 = run_lasso_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
lasso_DAI_1_metrics1 <- depeg_metrics(lassow1$DAI$depeg_1d$test$y, lassow1$DAI$depeg_1d$pred_class)
lasso_DAI_3_metrics1 <- depeg_metrics(lassow1$DAI$depeg_3d$test$y, lassow1$DAI$depeg_3d$pred_class)
lasso_DAI_5_metrics1 <- depeg_metrics(lassow1$DAI$depeg_5d$test$y, lassow1$DAI$depeg_5d$pred_class)
lasso_DAI_7_metrics1 <- depeg_metrics(lassow1$DAI$depeg_7d$test$y, lassow1$DAI$depeg_7d$pred_class)

plot_lasso_DAI_w1_data <- list(depeg_1d = list(test = dfw1$DAI$depeg_1d$test,
                                                     pred = lassow1$DAI$depeg_1d$pred_class),
                                     depeg_3d = list(test = dfw1$DAI$depeg_3d$test,
                                                     pred = lassow1$DAI$depeg_3d$pred_class),
                                     depeg_5d = list(test = dfw1$DAI$depeg_5d$test,
                                                     pred = lassow1$DAI$depeg_5d$pred_class),
                                     depeg_7d = list(test = dfw1$DAI$depeg_7d$test,
                                                     pred = lassow1$DAI$depeg_7d$pred_class))

plot_lasso_DAI_w1 <- plot_results(plot_lasso_DAI_w1_data,org_data = data_DAI,
                                        title = "Window 1: DAI Depeg Predictions")
plot_lasso_DAI_w1

#comparing performance across horizons 
plot_horizon_performance(lassow1$DAI$depeg_1d, lassow1$DAI$depeg_3d, lassow1$DAI$depeg_5d, lassow1$DAI$depeg_7d)

#looking at top 10 coeffs (not stable, with odds ratio close to 0/+inf)
show_lasso_coeffs(lassow1$DAI$depeg_1d, 10)
show_lasso_coeffs(lassow1$DAI$depeg_3d, 10)
show_lasso_coeffs(lassow1$DAI$depeg_5d, 10)
show_lasso_coeffs(lassow1$DAI$depeg_7d, 10)

# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- UST ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


## WINDOW 2 ====================================================================
# run LASSO on all coins for all horizons
lassow2 = run_lasso_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

# --------------------------------- DAI ----------------------------------------
#comparing performance across horizons 
lasso_DAI_1_metrics2 <- depeg_metrics(lassow2$DAI$depeg_1d$test$y, lassow2$DAI$depeg_1d$pred_class)
lasso_DAI_3_metrics2 <- depeg_metrics(lassow2$DAI$depeg_3d$test$y, lassow2$DAI$depeg_3d$pred_class)
lasso_DAI_5_metrics2 <- depeg_metrics(lassow2$DAI$depeg_5d$test$y, lassow2$DAI$depeg_5d$pred_class)
lasso_DAI_7_metrics2 <- depeg_metrics(lassow2$DAI$depeg_7d$test$y, lassow2$DAI$depeg_7d$pred_class)

plot_lasso_DAI_w2_data <- list(depeg_1d = list(test = dfw2$DAI$depeg_1d$test,
                                                     pred = lassow2$DAI$depeg_1d$pred_class),
                                     depeg_3d = list(test = dfw2$DAI$depeg_3d$test,
                                                     pred = lassow2$DAI$depeg_3d$pred_class),
                                     depeg_5d = list(test = dfw2$DAI$depeg_5d$test,
                                                     pred = lassow2$DAI$depeg_5d$pred_class),
                                     depeg_7d = list(test = dfw2$DAI$depeg_7d$test,
                                                     pred = lassow2$DAI$depeg_7d$pred_class))

plot_lasso_DAI_w2 <- plot_results(plot_lasso_DAI_w2_data,org_data = data_DAI,
                                        title = "Window 2: DAI Depeg Predictions")
plot_logit_lasso_DAI_w2

#comparing performance across horizons 
plot_horizon_performance(lassow2$DAI$depeg_1d, lassow2$DAI$depeg_3d, lassow2$DAI$depeg_5d, lassow2$DAI$depeg_7d)

#looking at top 10 coeffs (not stable, with odds ratio close to 0/+inf)
show_lasso_coeffs(lassow2$DAI$depeg_1d, 10)
show_lasso_coeffs(lassow2$DAI$depeg_3d, 10)
show_lasso_coeffs(lassow2$DAI$depeg_5d, 10)
show_lasso_coeffs(lassow2$DAI$depeg_7d, 10)
# ------------------------------------------------------------------------------

# --------------------------------- PAX ----------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDC ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# --------------------------------- USDT ---------------------------------------
# COPY OVER CODE
# ------------------------------------------------------------------------------

# ==============================================================================


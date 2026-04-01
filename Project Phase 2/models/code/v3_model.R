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
library(tibble)
library(scales)
library(grid)

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
rfw2 = runrf_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)



##########################################
### Gradient Boosting
##########################################

source("func-gb.R")

## WINDOW 1 ====================================================================
# run XGBoost on all coins for all horizons
gbw1 = rungb_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)
gbw2 = rungb_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)



#########################################
### Feature Importance
#########################################
source("func-feature_importance_logloss.R")

coins_w1 <- names(dfw1)
coins_w2 <- names(dfw2)

# -----------------------------
# RF importance
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

rf_logloss_imp_all <- bind_rows(rf_logloss_imp_w1, rf_logloss_imp_w2)

write.csv(
  rf_logloss_imp_all,
  "../../plots/rf_logloss_importance_all.csv",
  row.names = FALSE
)

# -----------------------------
# GB importance
# -----------------------------
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

write.csv(
  gb_logloss_imp_all,
  "../../plots/gb_logloss_importance_all.csv",
  row.names = FALSE
)

# ============================================
# Generate 4-horizon bar plots for each coin
# ============================================

# RF Window 1
for (coin in c("DAI", "PAX", "USDC", "USDT", "UST")) {
  p <- plot_coin_feature_importance(
    imp_data = rf_logloss_imp_all,
    coin_name = coin,
    model_name = "RF",
    window_name = "Window 1",
    top_n = 10
  )
  print(p)
  ggsave(
    paste0("../../plots/feature_importance/RF_", coin, "_feature_importance_w1.png"),
    p, width = 16, height = 4)
}

# RF Window 2
for (coin in c("DAI", "PAX", "USDC", "USDT")) {
  p <- plot_coin_feature_importance(
    imp_data = rf_logloss_imp_all,
    coin_name = coin,
    model_name = "RF",
    window_name = "Window 2",
    top_n = 10
  )
  print(p)
  ggsave(
    paste0("../../plots/feature_importance/RF_", coin, "_feature_importance_w2.png"),
    p, width = 16, height = 4
  )
}

# Code to show top 5 predictors by logloss importance
rf_logloss_imp_all %>% filter(window == "Window 1", 
                              coin == "UST", 
                              horizon == "depeg_7d") %>%
  mutate(importance_logloss = round(importance_logloss, 5)) %>%
  select(feature, importance_logloss) %>%
  head(5)

# GB Window 1
for (coin in c("DAI", "PAX", "USDC", "USDT", "UST")) {
  p <- plot_coin_feature_importance(
    imp_data = gb_logloss_imp_all,
    coin_name = coin,
    model_name = "GB",
    window_name = "Window 1",
    top_n = 10
  )
  print(p)
  ggsave(
    paste0("../../plots/feature_importance/GB_", coin, "_feature_importance_w1.png"),
    p, width = 16, height = 4)
}

# GB Window 2
for (coin in c("DAI", "PAX", "USDC", "USDT")) {
  p <- plot_coin_feature_importance(
    imp_data = gb_logloss_imp_all,
    coin_name = coin,
    model_name = "GB",
    window_name = "Window 2",
    top_n = 10
  )
  print(p)
  ggsave(
    paste0("../../plots/feature_importance/GB_", coin, "_feature_importance_w2.png"),
    p, width = 16, height = 4)
}

# Code to show top 5 predictors by logloss importance
gb_logloss_imp_all %>% filter(window == "Window 1", 
                              coin == "UST", 
                              horizon == "depeg_7d") %>%
  mutate(importance_logloss = round(importance_logloss, 5)) %>%
  select(feature, importance_logloss) %>%
  head(5)

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
pcrw2 = runpcr_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)


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
plsw2 = runpls_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

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
logitw2 = runlogit_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)

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
lassow2 = run_lasso_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)


lasso_DAI_1_metrics2 <- depeg_metrics(lassow2$DAI$depeg_1d$test$y, lassow2$DAI$depeg_1d$pred_class)
lasso_DAI_3_metrics2 <- depeg_metrics(lassow2$DAI$depeg_3d$test$y, lassow2$DAI$depeg_3d$pred_class)
lasso_DAI_5_metrics2 <- depeg_metrics(lassow2$DAI$depeg_5d$test$y, lassow2$DAI$depeg_5d$pred_class)
lasso_DAI_7_metrics2 <- depeg_metrics(lassow2$DAI$depeg_7d$test$y, lassow2$DAI$depeg_7d$pred_class)

# ---Summary ---
#model performance
#-- window 1
#DAI
rfw1_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, rfw1$DAI$depeg_1d$pred_class)
rfw1_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, rfw1$DAI$depeg_3d$pred_class)
rfw1_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, rfw1$DAI$depeg_5d$pred_class)
rfw1_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, rfw1$DAI$depeg_7d$pred_class)


# PAX
rfw1_PAX_1_metrics <- depeg_metrics(dfw1$PAX$depeg_1d$test$y, rfw1$PAX$depeg_1d$pred_class)
rfw1_PAX_3_metrics <- depeg_metrics(dfw1$PAX$depeg_3d$test$y, rfw1$PAX$depeg_3d$pred_class)
rfw1_PAX_5_metrics <- depeg_metrics(dfw1$PAX$depeg_5d$test$y, rfw1$PAX$depeg_5d$pred_class)
rfw1_PAX_7_metrics <- depeg_metrics(dfw1$PAX$depeg_7d$test$y, rfw1$PAX$depeg_7d$pred_class)

# USDT
rfw1_USDT_1_metrics <- depeg_metrics(dfw1$USDT$depeg_1d$test$y, rfw1$USDT$depeg_1d$pred_class)
rfw1_USDT_3_metrics <- depeg_metrics(dfw1$USDT$depeg_3d$test$y, rfw1$USDT$depeg_3d$pred_class)
rfw1_USDT_5_metrics <- depeg_metrics(dfw1$USDT$depeg_5d$test$y, rfw1$USDT$depeg_5d$pred_class)
rfw1_USDT_7_metrics <- depeg_metrics(dfw1$USDT$depeg_7d$test$y, rfw1$USDT$depeg_7d$pred_class)

# USDC
rfw1_USDC_1_metrics <- depeg_metrics(dfw1$USDC$depeg_1d$test$y, rfw1$USDC$depeg_1d$pred_class)
rfw1_USDC_3_metrics <- depeg_metrics(dfw1$USDC$depeg_3d$test$y, rfw1$USDC$depeg_3d$pred_class)
rfw1_USDC_5_metrics <- depeg_metrics(dfw1$USDC$depeg_5d$test$y, rfw1$USDC$depeg_5d$pred_class)
rfw1_USDC_7_metrics <- depeg_metrics(dfw1$USDC$depeg_7d$test$y, rfw1$USDC$depeg_7d$pred_class)

# UST
rfw1_UST_1_metrics <- depeg_metrics(dfw1$UST$depeg_1d$test$y, rfw1$UST$depeg_1d$pred_class)
rfw1_UST_3_metrics <- depeg_metrics(dfw1$UST$depeg_3d$test$y, rfw1$UST$depeg_3d$pred_class)
rfw1_UST_5_metrics <- depeg_metrics(dfw1$UST$depeg_5d$test$y, rfw1$UST$depeg_5d$pred_class)
rfw1_UST_7_metrics <- depeg_metrics(dfw1$UST$depeg_7d$test$y, rfw1$UST$depeg_7d$pred_class)



#--- Window 2
# PAX
rfw2_PAX_1_metrics <- depeg_metrics(dfw2$PAX$depeg_1d$test$y, rfw2$PAX$depeg_1d$pred_class)
rfw2_PAX_3_metrics <- depeg_metrics(dfw2$PAX$depeg_3d$test$y, rfw2$PAX$depeg_3d$pred_class)
rfw2_PAX_5_metrics <- depeg_metrics(dfw2$PAX$depeg_5d$test$y, rfw2$PAX$depeg_5d$pred_class)
rfw2_PAX_7_metrics <- depeg_metrics(dfw2$PAX$depeg_7d$test$y, rfw2$PAX$depeg_7d$pred_class)

# USDT
rfw2_USDT_1_metrics <- depeg_metrics(dfw2$USDT$depeg_1d$test$y, rfw2$USDT$depeg_1d$pred_class)
rfw2_USDT_3_metrics <- depeg_metrics(dfw2$USDT$depeg_3d$test$y, rfw2$USDT$depeg_3d$pred_class)
rfw2_USDT_5_metrics <- depeg_metrics(dfw2$USDT$depeg_5d$test$y, rfw2$USDT$depeg_5d$pred_class)
rfw2_USDT_7_metrics <- depeg_metrics(dfw2$USDT$depeg_7d$test$y, rfw2$USDT$depeg_7d$pred_class)

# USDC
rfw2_USDC_1_metrics <- depeg_metrics(dfw2$USDC$depeg_1d$test$y, rfw2$USDC$depeg_1d$pred_class)
rfw2_USDC_3_metrics <- depeg_metrics(dfw2$USDC$depeg_3d$test$y, rfw2$USDC$depeg_3d$pred_class)
rfw2_USDC_5_metrics <- depeg_metrics(dfw2$USDC$depeg_5d$test$y, rfw2$USDC$depeg_5d$pred_class)
rfw2_USDC_7_metrics <- depeg_metrics(dfw2$USDC$depeg_7d$test$y, rfw2$USDC$depeg_7d$pred_class)

# DAI
rfw2_DAI_1_metrics <- depeg_metrics(dfw2$DAI$depeg_1d$test$y, rfw2$DAI$depeg_1d$pred_class)
rfw2_DAI_3_metrics <- depeg_metrics(dfw2$DAI$depeg_3d$test$y, rfw2$DAI$depeg_3d$pred_class)
rfw2_DAI_5_metrics <- depeg_metrics(dfw2$DAI$depeg_5d$test$y, rfw2$DAI$depeg_5d$pred_class)
rfw2_DAI_7_metrics <- depeg_metrics(dfw2$DAI$depeg_7d$test$y, rfw2$DAI$depeg_7d$pred_class)

#Gradient Boosting
# DAI
gbw1_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, gbw1$DAI$depeg_1d$pred_class)
gbw1_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, gbw1$DAI$depeg_3d$pred_class)
gbw1_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, gbw1$DAI$depeg_5d$pred_class)
gbw1_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, gbw1$DAI$depeg_7d$pred_class)

# PAX
gbw1_PAX_1_metrics <- depeg_metrics(dfw1$PAX$depeg_1d$test$y, gbw1$PAX$depeg_1d$pred_class)
gbw1_PAX_3_metrics <- depeg_metrics(dfw1$PAX$depeg_3d$test$y, gbw1$PAX$depeg_3d$pred_class)
gbw1_PAX_5_metrics <- depeg_metrics(dfw1$PAX$depeg_5d$test$y, gbw1$PAX$depeg_5d$pred_class)
gbw1_PAX_7_metrics <- depeg_metrics(dfw1$PAX$depeg_7d$test$y, gbw1$PAX$depeg_7d$pred_class)

# USDT
gbw1_USDT_1_metrics <- depeg_metrics(dfw1$USDT$depeg_1d$test$y, gbw1$USDT$depeg_1d$pred_class)
gbw1_USDT_3_metrics <- depeg_metrics(dfw1$USDT$depeg_3d$test$y, gbw1$USDT$depeg_3d$pred_class)
gbw1_USDT_5_metrics <- depeg_metrics(dfw1$USDT$depeg_5d$test$y, gbw1$USDT$depeg_5d$pred_class)
gbw1_USDT_7_metrics <- depeg_metrics(dfw1$USDT$depeg_7d$test$y, gbw1$USDT$depeg_7d$pred_class)

# USDC
gbw1_USDC_1_metrics <- depeg_metrics(dfw1$USDC$depeg_1d$test$y, gbw1$USDC$depeg_1d$pred_class)
gbw1_USDC_3_metrics <- depeg_metrics(dfw1$USDC$depeg_3d$test$y, gbw1$USDC$depeg_3d$pred_class)
gbw1_USDC_5_metrics <- depeg_metrics(dfw1$USDC$depeg_5d$test$y, gbw1$USDC$depeg_5d$pred_class)
gbw1_USDC_7_metrics <- depeg_metrics(dfw1$USDC$depeg_7d$test$y, gbw1$USDC$depeg_7d$pred_class)

# UST
gbw1_UST_1_metrics <- depeg_metrics(dfw1$UST$depeg_1d$test$y, gbw1$UST$depeg_1d$pred_class)
gbw1_UST_3_metrics <- depeg_metrics(dfw1$UST$depeg_3d$test$y, gbw1$UST$depeg_3d$pred_class)
gbw1_UST_5_metrics <- depeg_metrics(dfw1$UST$depeg_5d$test$y, gbw1$UST$depeg_5d$pred_class)
gbw1_UST_7_metrics <- depeg_metrics(dfw1$UST$depeg_7d$test$y, gbw1$UST$depeg_7d$pred_class)

#--- Window 2
# PAX
gbw2_PAX_1_metrics <- depeg_metrics(dfw2$PAX$depeg_1d$test$y, gbw2$PAX$depeg_1d$pred_class)
gbw2_PAX_3_metrics <- depeg_metrics(dfw2$PAX$depeg_3d$test$y, gbw2$PAX$depeg_3d$pred_class)
gbw2_PAX_5_metrics <- depeg_metrics(dfw2$PAX$depeg_5d$test$y, gbw2$PAX$depeg_5d$pred_class)
gbw2_PAX_7_metrics <- depeg_metrics(dfw2$PAX$depeg_7d$test$y, gbw2$PAX$depeg_7d$pred_class)

# USDT
gbw2_USDT_1_metrics <- depeg_metrics(dfw2$USDT$depeg_1d$test$y, gbw2$USDT$depeg_1d$pred_class)
gbw2_USDT_3_metrics <- depeg_metrics(dfw2$USDT$depeg_3d$test$y, gbw2$USDT$depeg_3d$pred_class)
gbw2_USDT_5_metrics <- depeg_metrics(dfw2$USDT$depeg_5d$test$y, gbw2$USDT$depeg_5d$pred_class)
gbw2_USDT_7_metrics <- depeg_metrics(dfw2$USDT$depeg_7d$test$y, gbw2$USDT$depeg_7d$pred_class)

# USDC
gbw2_USDC_1_metrics <- depeg_metrics(dfw2$USDC$depeg_1d$test$y, gbw2$USDC$depeg_1d$pred_class)
gbw2_USDC_3_metrics <- depeg_metrics(dfw2$USDC$depeg_3d$test$y, gbw2$USDC$depeg_3d$pred_class)
gbw2_USDC_5_metrics <- depeg_metrics(dfw2$USDC$depeg_5d$test$y, gbw2$USDC$depeg_5d$pred_class)
gbw2_USDC_7_metrics <- depeg_metrics(dfw2$USDC$depeg_7d$test$y, gbw2$USDC$depeg_7d$pred_class)

# DAI
gbw2_DAI_1_metrics <- depeg_metrics(dfw2$DAI$depeg_1d$test$y, gbw2$DAI$depeg_1d$pred_class)
gbw2_DAI_3_metrics <- depeg_metrics(dfw2$DAI$depeg_3d$test$y, gbw2$DAI$depeg_3d$pred_class)
gbw2_DAI_5_metrics <- depeg_metrics(dfw2$DAI$depeg_5d$test$y, gbw2$DAI$depeg_5d$pred_class)
gbw2_DAI_7_metrics <- depeg_metrics(dfw2$DAI$depeg_7d$test$y, gbw2$DAI$depeg_7d$pred_class)

# =========================
# PCR
# =========================

# -- Window 1
# DAI
pcrw1_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, pcrw1$DAI$depeg_1d$pred_class)
pcrw1_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, pcrw1$DAI$depeg_3d$pred_class)
pcrw1_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, pcrw1$DAI$depeg_5d$pred_class)
pcrw1_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, pcrw1$DAI$depeg_7d$pred_class)

# PAX
pcrw1_PAX_1_metrics <- depeg_metrics(dfw1$PAX$depeg_1d$test$y, pcrw1$PAX$depeg_1d$pred_class)
pcrw1_PAX_3_metrics <- depeg_metrics(dfw1$PAX$depeg_3d$test$y, pcrw1$PAX$depeg_3d$pred_class)
pcrw1_PAX_5_metrics <- depeg_metrics(dfw1$PAX$depeg_5d$test$y, pcrw1$PAX$depeg_5d$pred_class)
pcrw1_PAX_7_metrics <- depeg_metrics(dfw1$PAX$depeg_7d$test$y, pcrw1$PAX$depeg_7d$pred_class)

# USDT
pcrw1_USDT_1_metrics <- depeg_metrics(dfw1$USDT$depeg_1d$test$y, pcrw1$USDT$depeg_1d$pred_class)
pcrw1_USDT_3_metrics <- depeg_metrics(dfw1$USDT$depeg_3d$test$y, pcrw1$USDT$depeg_3d$pred_class)
pcrw1_USDT_5_metrics <- depeg_metrics(dfw1$USDT$depeg_5d$test$y, pcrw1$USDT$depeg_5d$pred_class)
pcrw1_USDT_7_metrics <- depeg_metrics(dfw1$USDT$depeg_7d$test$y, pcrw1$USDT$depeg_7d$pred_class)

# USDC
pcrw1_USDC_1_metrics <- depeg_metrics(dfw1$USDC$depeg_1d$test$y, pcrw1$USDC$depeg_1d$pred_class)
pcrw1_USDC_3_metrics <- depeg_metrics(dfw1$USDC$depeg_3d$test$y, pcrw1$USDC$depeg_3d$pred_class)
pcrw1_USDC_5_metrics <- depeg_metrics(dfw1$USDC$depeg_5d$test$y, pcrw1$USDC$depeg_5d$pred_class)
pcrw1_USDC_7_metrics <- depeg_metrics(dfw1$USDC$depeg_7d$test$y, pcrw1$USDC$depeg_7d$pred_class)

# UST
pcrw1_UST_1_metrics <- depeg_metrics(dfw1$UST$depeg_1d$test$y, pcrw1$UST$depeg_1d$pred_class)
pcrw1_UST_3_metrics <- depeg_metrics(dfw1$UST$depeg_3d$test$y, pcrw1$UST$depeg_3d$pred_class)
pcrw1_UST_5_metrics <- depeg_metrics(dfw1$UST$depeg_5d$test$y, pcrw1$UST$depeg_5d$pred_class)
pcrw1_UST_7_metrics <- depeg_metrics(dfw1$UST$depeg_7d$test$y, pcrw1$UST$depeg_7d$pred_class)

# -- Window 2
# PAX
pcrw2_PAX_1_metrics <- depeg_metrics(dfw2$PAX$depeg_1d$test$y, pcrw2$PAX$depeg_1d$pred_class)
pcrw2_PAX_3_metrics <- depeg_metrics(dfw2$PAX$depeg_3d$test$y, pcrw2$PAX$depeg_3d$pred_class)
pcrw2_PAX_5_metrics <- depeg_metrics(dfw2$PAX$depeg_5d$test$y, pcrw2$PAX$depeg_5d$pred_class)
pcrw2_PAX_7_metrics <- depeg_metrics(dfw2$PAX$depeg_7d$test$y, pcrw2$PAX$depeg_7d$pred_class)

# USDT
pcrw2_USDT_1_metrics <- depeg_metrics(dfw2$USDT$depeg_1d$test$y, pcrw2$USDT$depeg_1d$pred_class)
pcrw2_USDT_3_metrics <- depeg_metrics(dfw2$USDT$depeg_3d$test$y, pcrw2$USDT$depeg_3d$pred_class)
pcrw2_USDT_5_metrics <- depeg_metrics(dfw2$USDT$depeg_5d$test$y, pcrw2$USDT$depeg_5d$pred_class)
pcrw2_USDT_7_metrics <- depeg_metrics(dfw2$USDT$depeg_7d$test$y, pcrw2$USDT$depeg_7d$pred_class)

# USDC
pcrw2_USDC_1_metrics <- depeg_metrics(dfw2$USDC$depeg_1d$test$y, pcrw2$USDC$depeg_1d$pred_class)
pcrw2_USDC_3_metrics <- depeg_metrics(dfw2$USDC$depeg_3d$test$y, pcrw2$USDC$depeg_3d$pred_class)
pcrw2_USDC_5_metrics <- depeg_metrics(dfw2$USDC$depeg_5d$test$y, pcrw2$USDC$depeg_5d$pred_class)
pcrw2_USDC_7_metrics <- depeg_metrics(dfw2$USDC$depeg_7d$test$y, pcrw2$USDC$depeg_7d$pred_class)

# DAI
pcrw2_DAI_1_metrics <- depeg_metrics(dfw2$DAI$depeg_1d$test$y, pcrw2$DAI$depeg_1d$pred_class)
pcrw2_DAI_3_metrics <- depeg_metrics(dfw2$DAI$depeg_3d$test$y, pcrw2$DAI$depeg_3d$pred_class)
pcrw2_DAI_5_metrics <- depeg_metrics(dfw2$DAI$depeg_5d$test$y, pcrw2$DAI$depeg_5d$pred_class)
pcrw2_DAI_7_metrics <- depeg_metrics(dfw2$DAI$depeg_7d$test$y, pcrw2$DAI$depeg_7d$pred_class)


# =========================
# PLS
# =========================

# -- Window 1
# DAI
plsw1_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, plsw1$DAI$depeg_1d$pred_class)
plsw1_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, plsw1$DAI$depeg_3d$pred_class)
plsw1_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, plsw1$DAI$depeg_5d$pred_class)
plsw1_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, plsw1$DAI$depeg_7d$pred_class)

# PAX
plsw1_PAX_1_metrics <- depeg_metrics(dfw1$PAX$depeg_1d$test$y, plsw1$PAX$depeg_1d$pred_class)
plsw1_PAX_3_metrics <- depeg_metrics(dfw1$PAX$depeg_3d$test$y, plsw1$PAX$depeg_3d$pred_class)
plsw1_PAX_5_metrics <- depeg_metrics(dfw1$PAX$depeg_5d$test$y, plsw1$PAX$depeg_5d$pred_class)
plsw1_PAX_7_metrics <- depeg_metrics(dfw1$PAX$depeg_7d$test$y, plsw1$PAX$depeg_7d$pred_class)

# USDT
plsw1_USDT_1_metrics <- depeg_metrics(dfw1$USDT$depeg_1d$test$y, plsw1$USDT$depeg_1d$pred_class)
plsw1_USDT_3_metrics <- depeg_metrics(dfw1$USDT$depeg_3d$test$y, plsw1$USDT$depeg_3d$pred_class)
plsw1_USDT_5_metrics <- depeg_metrics(dfw1$USDT$depeg_5d$test$y, plsw1$USDT$depeg_5d$pred_class)
plsw1_USDT_7_metrics <- depeg_metrics(dfw1$USDT$depeg_7d$test$y, plsw1$USDT$depeg_7d$pred_class)

# USDC
plsw1_USDC_1_metrics <- depeg_metrics(dfw1$USDC$depeg_1d$test$y, plsw1$USDC$depeg_1d$pred_class)
plsw1_USDC_3_metrics <- depeg_metrics(dfw1$USDC$depeg_3d$test$y, plsw1$USDC$depeg_3d$pred_class)
plsw1_USDC_5_metrics <- depeg_metrics(dfw1$USDC$depeg_5d$test$y, plsw1$USDC$depeg_5d$pred_class)
plsw1_USDC_7_metrics <- depeg_metrics(dfw1$USDC$depeg_7d$test$y, plsw1$USDC$depeg_7d$pred_class)

# UST
plsw1_UST_1_metrics <- depeg_metrics(dfw1$UST$depeg_1d$test$y, plsw1$UST$depeg_1d$pred_class)
plsw1_UST_3_metrics <- depeg_metrics(dfw1$UST$depeg_3d$test$y, plsw1$UST$depeg_3d$pred_class)
plsw1_UST_5_metrics <- depeg_metrics(dfw1$UST$depeg_5d$test$y, plsw1$UST$depeg_5d$pred_class)
plsw1_UST_7_metrics <- depeg_metrics(dfw1$UST$depeg_7d$test$y, plsw1$UST$depeg_7d$pred_class)

# -- Window 2
# PAX
plsw2_PAX_1_metrics <- depeg_metrics(dfw2$PAX$depeg_1d$test$y, plsw2$PAX$depeg_1d$pred_class)
plsw2_PAX_3_metrics <- depeg_metrics(dfw2$PAX$depeg_3d$test$y, plsw2$PAX$depeg_3d$pred_class)
plsw2_PAX_5_metrics <- depeg_metrics(dfw2$PAX$depeg_5d$test$y, plsw2$PAX$depeg_5d$pred_class)
plsw2_PAX_7_metrics <- depeg_metrics(dfw2$PAX$depeg_7d$test$y, plsw2$PAX$depeg_7d$pred_class)

# USDT
plsw2_USDT_1_metrics <- depeg_metrics(dfw2$USDT$depeg_1d$test$y, plsw2$USDT$depeg_1d$pred_class)
plsw2_USDT_3_metrics <- depeg_metrics(dfw2$USDT$depeg_3d$test$y, plsw2$USDT$depeg_3d$pred_class)
plsw2_USDT_5_metrics <- depeg_metrics(dfw2$USDT$depeg_5d$test$y, plsw2$USDT$depeg_5d$pred_class)
plsw2_USDT_7_metrics <- depeg_metrics(dfw2$USDT$depeg_7d$test$y, plsw2$USDT$depeg_7d$pred_class)

# USDC
plsw2_USDC_1_metrics <- depeg_metrics(dfw2$USDC$depeg_1d$test$y, plsw2$USDC$depeg_1d$pred_class)
plsw2_USDC_3_metrics <- depeg_metrics(dfw2$USDC$depeg_3d$test$y, plsw2$USDC$depeg_3d$pred_class)
plsw2_USDC_5_metrics <- depeg_metrics(dfw2$USDC$depeg_5d$test$y, plsw2$USDC$depeg_5d$pred_class)
plsw2_USDC_7_metrics <- depeg_metrics(dfw2$USDC$depeg_7d$test$y, plsw2$USDC$depeg_7d$pred_class)

# DAI
plsw2_DAI_1_metrics <- depeg_metrics(dfw2$DAI$depeg_1d$test$y, plsw2$DAI$depeg_1d$pred_class)
plsw2_DAI_3_metrics <- depeg_metrics(dfw2$DAI$depeg_3d$test$y, plsw2$DAI$depeg_3d$pred_class)
plsw2_DAI_5_metrics <- depeg_metrics(dfw2$DAI$depeg_5d$test$y, plsw2$DAI$depeg_5d$pred_class)
plsw2_DAI_7_metrics <- depeg_metrics(dfw2$DAI$depeg_7d$test$y, plsw2$DAI$depeg_7d$pred_class)


# constructing summary
extract_metrics <- function(metric_obj, model, window, coin, horizon) {
  tibble(
    model = model,
    window = window,
    coin = coin,
    horizon = horizon,
    accuracy = metric_obj$accuracy,
    misclassifications = metric_obj$misclassifications,
    sensitivity = metric_obj$sensitivity,
    specificity = metric_obj$specificity,
    precision = metric_obj$precision,
    recall = metric_obj$recall,
    f1_score = metric_obj$f1_score,
    TP = unname(metric_obj$confusion_matrix["TP"]),
    TN = unname(metric_obj$confusion_matrix["TN"]),
    FP = unname(metric_obj$confusion_matrix["FP"]),
    FN = unname(metric_obj$confusion_matrix["FN"]),
    FP_cost_ratio = metric_obj$FP_cost_ratio,
    FN_cost_ratio = metric_obj$FN_cost_ratio,
    total_observations = metric_obj$total_observations
  )
}
summary_metrics <- bind_rows(
  # ---------------- RF ----------------
  extract_metrics(rfw1_DAI_1_metrics,  "RF", "Window 1", "DAI",  "1d"),
  extract_metrics(rfw1_DAI_3_metrics,  "RF", "Window 1", "DAI",  "3d"),
  extract_metrics(rfw1_DAI_5_metrics,  "RF", "Window 1", "DAI",  "5d"),
  extract_metrics(rfw1_DAI_7_metrics,  "RF", "Window 1", "DAI",  "7d"),
  
  extract_metrics(rfw1_PAX_1_metrics,  "RF", "Window 1", "PAX",  "1d"),
  extract_metrics(rfw1_PAX_3_metrics,  "RF", "Window 1", "PAX",  "3d"),
  extract_metrics(rfw1_PAX_5_metrics,  "RF", "Window 1", "PAX",  "5d"),
  extract_metrics(rfw1_PAX_7_metrics,  "RF", "Window 1", "PAX",  "7d"),
  
  extract_metrics(rfw1_USDT_1_metrics, "RF", "Window 1", "USDT", "1d"),
  extract_metrics(rfw1_USDT_3_metrics, "RF", "Window 1", "USDT", "3d"),
  extract_metrics(rfw1_USDT_5_metrics, "RF", "Window 1", "USDT", "5d"),
  extract_metrics(rfw1_USDT_7_metrics, "RF", "Window 1", "USDT", "7d"),
  
  extract_metrics(rfw1_USDC_1_metrics, "RF", "Window 1", "USDC", "1d"),
  extract_metrics(rfw1_USDC_3_metrics, "RF", "Window 1", "USDC", "3d"),
  extract_metrics(rfw1_USDC_5_metrics, "RF", "Window 1", "USDC", "5d"),
  extract_metrics(rfw1_USDC_7_metrics, "RF", "Window 1", "USDC", "7d"),
  
  extract_metrics(rfw1_UST_1_metrics,  "RF", "Window 1", "UST",  "1d"),
  extract_metrics(rfw1_UST_3_metrics,  "RF", "Window 1", "UST",  "3d"),
  extract_metrics(rfw1_UST_5_metrics,  "RF", "Window 1", "UST",  "5d"),
  extract_metrics(rfw1_UST_7_metrics,  "RF", "Window 1", "UST",  "7d"),
  
  extract_metrics(rfw2_DAI_1_metrics,  "RF", "Window 2", "DAI",  "1d"),
  extract_metrics(rfw2_DAI_3_metrics,  "RF", "Window 2", "DAI",  "3d"),
  extract_metrics(rfw2_DAI_5_metrics,  "RF", "Window 2", "DAI",  "5d"),
  extract_metrics(rfw2_DAI_7_metrics,  "RF", "Window 2", "DAI",  "7d"),
  
  extract_metrics(rfw2_PAX_1_metrics,  "RF", "Window 2", "PAX",  "1d"),
  extract_metrics(rfw2_PAX_3_metrics,  "RF", "Window 2", "PAX",  "3d"),
  extract_metrics(rfw2_PAX_5_metrics,  "RF", "Window 2", "PAX",  "5d"),
  extract_metrics(rfw2_PAX_7_metrics,  "RF", "Window 2", "PAX",  "7d"),
  
  extract_metrics(rfw2_USDT_1_metrics, "RF", "Window 2", "USDT", "1d"),
  extract_metrics(rfw2_USDT_3_metrics, "RF", "Window 2", "USDT", "3d"),
  extract_metrics(rfw2_USDT_5_metrics, "RF", "Window 2", "USDT", "5d"),
  extract_metrics(rfw2_USDT_7_metrics, "RF", "Window 2", "USDT", "7d"),
  
  extract_metrics(rfw2_USDC_1_metrics, "RF", "Window 2", "USDC", "1d"),
  extract_metrics(rfw2_USDC_3_metrics, "RF", "Window 2", "USDC", "3d"),
  extract_metrics(rfw2_USDC_5_metrics, "RF", "Window 2", "USDC", "5d"),
  extract_metrics(rfw2_USDC_7_metrics, "RF", "Window 2", "USDC", "7d"),
  
  # ---------------- GB ----------------
  extract_metrics(gbw1_DAI_1_metrics,  "GB", "Window 1", "DAI",  "1d"),
  extract_metrics(gbw1_DAI_3_metrics,  "GB", "Window 1", "DAI",  "3d"),
  extract_metrics(gbw1_DAI_5_metrics,  "GB", "Window 1", "DAI",  "5d"),
  extract_metrics(gbw1_DAI_7_metrics,  "GB", "Window 1", "DAI",  "7d"),
  
  extract_metrics(gbw1_PAX_1_metrics,  "GB", "Window 1", "PAX",  "1d"),
  extract_metrics(gbw1_PAX_3_metrics,  "GB", "Window 1", "PAX",  "3d"),
  extract_metrics(gbw1_PAX_5_metrics,  "GB", "Window 1", "PAX",  "5d"),
  extract_metrics(gbw1_PAX_7_metrics,  "GB", "Window 1", "PAX",  "7d"),
  
  extract_metrics(gbw1_USDT_1_metrics, "GB", "Window 1", "USDT", "1d"),
  extract_metrics(gbw1_USDT_3_metrics, "GB", "Window 1", "USDT", "3d"),
  extract_metrics(gbw1_USDT_5_metrics, "GB", "Window 1", "USDT", "5d"),
  extract_metrics(gbw1_USDT_7_metrics, "GB", "Window 1", "USDT", "7d"),
  
  extract_metrics(gbw1_USDC_1_metrics, "GB", "Window 1", "USDC", "1d"),
  extract_metrics(gbw1_USDC_3_metrics, "GB", "Window 1", "USDC", "3d"),
  extract_metrics(gbw1_USDC_5_metrics, "GB", "Window 1", "USDC", "5d"),
  extract_metrics(gbw1_USDC_7_metrics, "GB", "Window 1", "USDC", "7d"),
  
  extract_metrics(gbw1_UST_1_metrics,  "GB", "Window 1", "UST",  "1d"),
  extract_metrics(gbw1_UST_3_metrics,  "GB", "Window 1", "UST",  "3d"),
  extract_metrics(gbw1_UST_5_metrics,  "GB", "Window 1", "UST",  "5d"),
  extract_metrics(gbw1_UST_7_metrics,  "GB", "Window 1", "UST",  "7d"),
  
  extract_metrics(gbw2_DAI_1_metrics,  "GB", "Window 2", "DAI",  "1d"),
  extract_metrics(gbw2_DAI_3_metrics,  "GB", "Window 2", "DAI",  "3d"),
  extract_metrics(gbw2_DAI_5_metrics,  "GB", "Window 2", "DAI",  "5d"),
  extract_metrics(gbw2_DAI_7_metrics,  "GB", "Window 2", "DAI",  "7d"),
  
  extract_metrics(gbw2_PAX_1_metrics,  "GB", "Window 2", "PAX",  "1d"),
  extract_metrics(gbw2_PAX_3_metrics,  "GB", "Window 2", "PAX",  "3d"),
  extract_metrics(gbw2_PAX_5_metrics,  "GB", "Window 2", "PAX",  "5d"),
  extract_metrics(gbw2_PAX_7_metrics,  "GB", "Window 2", "PAX",  "7d"),
  
  extract_metrics(gbw2_USDT_1_metrics, "GB", "Window 2", "USDT", "1d"),
  extract_metrics(gbw2_USDT_3_metrics, "GB", "Window 2", "USDT", "3d"),
  extract_metrics(gbw2_USDT_5_metrics, "GB", "Window 2", "USDT", "5d"),
  extract_metrics(gbw2_USDT_7_metrics, "GB", "Window 2", "USDT", "7d"),
  
  extract_metrics(gbw2_USDC_1_metrics, "GB", "Window 2", "USDC", "1d"),
  extract_metrics(gbw2_USDC_3_metrics, "GB", "Window 2", "USDC", "3d"),
  extract_metrics(gbw2_USDC_5_metrics, "GB", "Window 2", "USDC", "5d"),
  extract_metrics(gbw2_USDC_7_metrics, "GB", "Window 2", "USDC", "7d"),
  
  # ---------------- PCR ----------------
  extract_metrics(pcrw1_DAI_1_metrics,  "PCR", "Window 1", "DAI",  "1d"),
  extract_metrics(pcrw1_DAI_3_metrics,  "PCR", "Window 1", "DAI",  "3d"),
  extract_metrics(pcrw1_DAI_5_metrics,  "PCR", "Window 1", "DAI",  "5d"),
  extract_metrics(pcrw1_DAI_7_metrics,  "PCR", "Window 1", "DAI",  "7d"),
  
  extract_metrics(pcrw1_PAX_1_metrics,  "PCR", "Window 1", "PAX",  "1d"),
  extract_metrics(pcrw1_PAX_3_metrics,  "PCR", "Window 1", "PAX",  "3d"),
  extract_metrics(pcrw1_PAX_5_metrics,  "PCR", "Window 1", "PAX",  "5d"),
  extract_metrics(pcrw1_PAX_7_metrics,  "PCR", "Window 1", "PAX",  "7d"),
  
  extract_metrics(pcrw1_USDT_1_metrics, "PCR", "Window 1", "USDT", "1d"),
  extract_metrics(pcrw1_USDT_3_metrics, "PCR", "Window 1", "USDT", "3d"),
  extract_metrics(pcrw1_USDT_5_metrics, "PCR", "Window 1", "USDT", "5d"),
  extract_metrics(pcrw1_USDT_7_metrics, "PCR", "Window 1", "USDT", "7d"),
  
  extract_metrics(pcrw1_USDC_1_metrics, "PCR", "Window 1", "USDC", "1d"),
  extract_metrics(pcrw1_USDC_3_metrics, "PCR", "Window 1", "USDC", "3d"),
  extract_metrics(pcrw1_USDC_5_metrics, "PCR", "Window 1", "USDC", "5d"),
  extract_metrics(pcrw1_USDC_7_metrics, "PCR", "Window 1", "USDC", "7d"),
  
  extract_metrics(pcrw1_UST_1_metrics,  "PCR", "Window 1", "UST",  "1d"),
  extract_metrics(pcrw1_UST_3_metrics,  "PCR", "Window 1", "UST",  "3d"),
  extract_metrics(pcrw1_UST_5_metrics,  "PCR", "Window 1", "UST",  "5d"),
  extract_metrics(pcrw1_UST_7_metrics,  "PCR", "Window 1", "UST",  "7d"),
  
  extract_metrics(pcrw2_DAI_1_metrics,  "PCR", "Window 2", "DAI",  "1d"),
  extract_metrics(pcrw2_DAI_3_metrics,  "PCR", "Window 2", "DAI",  "3d"),
  extract_metrics(pcrw2_DAI_5_metrics,  "PCR", "Window 2", "DAI",  "5d"),
  extract_metrics(pcrw2_DAI_7_metrics,  "PCR", "Window 2", "DAI",  "7d"),
  
  extract_metrics(pcrw2_PAX_1_metrics,  "PCR", "Window 2", "PAX",  "1d"),
  extract_metrics(pcrw2_PAX_3_metrics,  "PCR", "Window 2", "PAX",  "3d"),
  extract_metrics(pcrw2_PAX_5_metrics,  "PCR", "Window 2", "PAX",  "5d"),
  extract_metrics(pcrw2_PAX_7_metrics,  "PCR", "Window 2", "PAX",  "7d"),
  
  extract_metrics(pcrw2_USDT_1_metrics, "PCR", "Window 2", "USDT", "1d"),
  extract_metrics(pcrw2_USDT_3_metrics, "PCR", "Window 2", "USDT", "3d"),
  extract_metrics(pcrw2_USDT_5_metrics, "PCR", "Window 2", "USDT", "5d"),
  extract_metrics(pcrw2_USDT_7_metrics, "PCR", "Window 2", "USDT", "7d"),
  
  extract_metrics(pcrw2_USDC_1_metrics, "PCR", "Window 2", "USDC", "1d"),
  extract_metrics(pcrw2_USDC_3_metrics, "PCR", "Window 2", "USDC", "3d"),
  extract_metrics(pcrw2_USDC_5_metrics, "PCR", "Window 2", "USDC", "5d"),
  extract_metrics(pcrw2_USDC_7_metrics, "PCR", "Window 2", "USDC", "7d"),
  
  # ---------------- PLS ----------------
  extract_metrics(plsw1_DAI_1_metrics,  "PLS", "Window 1", "DAI",  "1d"),
  extract_metrics(plsw1_DAI_3_metrics,  "PLS", "Window 1", "DAI",  "3d"),
  extract_metrics(plsw1_DAI_5_metrics,  "PLS", "Window 1", "DAI",  "5d"),
  extract_metrics(plsw1_DAI_7_metrics,  "PLS", "Window 1", "DAI",  "7d"),
  
  extract_metrics(plsw1_PAX_1_metrics,  "PLS", "Window 1", "PAX",  "1d"),
  extract_metrics(plsw1_PAX_3_metrics,  "PLS", "Window 1", "PAX",  "3d"),
  extract_metrics(plsw1_PAX_5_metrics,  "PLS", "Window 1", "PAX",  "5d"),
  extract_metrics(plsw1_PAX_7_metrics,  "PLS", "Window 1", "PAX",  "7d"),
  
  extract_metrics(plsw1_USDT_1_metrics, "PLS", "Window 1", "USDT", "1d"),
  extract_metrics(plsw1_USDT_3_metrics, "PLS", "Window 1", "USDT", "3d"),
  extract_metrics(plsw1_USDT_5_metrics, "PLS", "Window 1", "USDT", "5d"),
  extract_metrics(plsw1_USDT_7_metrics, "PLS", "Window 1", "USDT", "7d"),
  
  extract_metrics(plsw1_USDC_1_metrics, "PLS", "Window 1", "USDC", "1d"),
  extract_metrics(plsw1_USDC_3_metrics, "PLS", "Window 1", "USDC", "3d"),
  extract_metrics(plsw1_USDC_5_metrics, "PLS", "Window 1", "USDC", "5d"),
  extract_metrics(plsw1_USDC_7_metrics, "PLS", "Window 1", "USDC", "7d"),
  
  extract_metrics(plsw1_UST_1_metrics,  "PLS", "Window 1", "UST",  "1d"),
  extract_metrics(plsw1_UST_3_metrics,  "PLS", "Window 1", "UST",  "3d"),
  extract_metrics(plsw1_UST_5_metrics,  "PLS", "Window 1", "UST",  "5d"),
  extract_metrics(plsw1_UST_7_metrics,  "PLS", "Window 1", "UST",  "7d"),
  
  extract_metrics(plsw2_DAI_1_metrics,  "PLS", "Window 2", "DAI",  "1d"),
  extract_metrics(plsw2_DAI_3_metrics,  "PLS", "Window 2", "DAI",  "3d"),
  extract_metrics(plsw2_DAI_5_metrics,  "PLS", "Window 2", "DAI",  "5d"),
  extract_metrics(plsw2_DAI_7_metrics,  "PLS", "Window 2", "DAI",  "7d"),
  
  extract_metrics(plsw2_PAX_1_metrics,  "PLS", "Window 2", "PAX",  "1d"),
  extract_metrics(plsw2_PAX_3_metrics,  "PLS", "Window 2", "PAX",  "3d"),
  extract_metrics(plsw2_PAX_5_metrics,  "PLS", "Window 2", "PAX",  "5d"),
  extract_metrics(plsw2_PAX_7_metrics,  "PLS", "Window 2", "PAX",  "7d"),
  
  extract_metrics(plsw2_USDT_1_metrics, "PLS", "Window 2", "USDT", "1d"),
  extract_metrics(plsw2_USDT_3_metrics, "PLS", "Window 2", "USDT", "3d"),
  extract_metrics(plsw2_USDT_5_metrics, "PLS", "Window 2", "USDT", "5d"),
  extract_metrics(plsw2_USDT_7_metrics, "PLS", "Window 2", "USDT", "7d"),
  
  extract_metrics(plsw2_USDC_1_metrics, "PLS", "Window 2", "USDC", "1d"),
  extract_metrics(plsw2_USDC_3_metrics, "PLS", "Window 2", "USDC", "3d"),
  extract_metrics(plsw2_USDC_5_metrics, "PLS", "Window 2", "USDC", "5d"),
  extract_metrics(plsw2_USDC_7_metrics, "PLS", "Window 2", "USDC", "7d")
)


summary_main <- summary_metrics %>%
  select(model, window, coin, horizon, accuracy, sensitivity, specificity,
         precision, recall, f1_score, TP, TN, FP, FN, total_observations) %>%
  mutate(across(c(accuracy, sensitivity, specificity, precision, recall, f1_score),
                ~ round(.x, 3))) %>%
  arrange(model, window, coin, horizon)

summary_main

summary_compare <- summary_main %>%
  select(model, window, coin, horizon, accuracy, f1_score, precision, recall) %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(accuracy, f1_score, precision, recall)
  )

summary_compare

write.csv(
  summary_main,
  "../../model performance output/summary_main.csv",
  row.names = FALSE
)

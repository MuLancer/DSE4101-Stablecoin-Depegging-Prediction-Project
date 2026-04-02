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

## RUN MODEL ====================================================================
# run random forest on all coins for all horizons
rfw1 = runrf_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)
rfw2 = runrf_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)



##########################################
### Gradient Boosting
##########################################

source("func-gb.R")

## RUN MODEL ====================================================================
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

## RUN MODEL ====================================================================
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

## RUN MODEL ====================================================================
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

## RUN MODEL ====================================================================
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


## RUN MODEL ====================================================================
# run LASSO on all coins for all horizons
lassow1 = run_lasso_all(dfw = dfw1, coin_list = coin_dfw1, horizons = horizons)
lassow2 = run_lasso_all(dfw = dfw2, coin_list = coin_dfw2, horizons = horizons)



###############################
### Load relevant functions ###
###############################

# load depeg metrics function
source("func-depeg_metrics.R")

# load in plotting functions
source("func-plots.R")

# -------------------------
# Get Performance Metrics 
# -------------------------
results_rfw1 <- get_all_metrics(rfw1, dfw1, coins_w1, horizons)
results_rfw2 <- get_all_metrics(rfw2, dfw2, coins_w2, horizons)

results_gbw1 <- get_all_metrics(gbw1, dfw1, coins_w1, horizons)
results_gbw2 <- get_all_metrics(gbw2, dfw2, coins_w2, horizons)

results_pcrw1 <- get_all_metrics(pcrw1, dfw1, coins_w1, horizons)
results_pcrw2 <- get_all_metrics(pcrw2, dfw2, coins_w2, horizons)

results_plsw1 <- get_all_metrics(plsw1, dfw1, coins_w1, horizons)
results_plsw2 <- get_all_metrics(plsw2, dfw2, coins_w2, horizons)

results_lassow1 <- get_all_metrics(lassow1, dfw1, coins_w1, horizons)
results_lassow2 <- get_all_metrics(lassow2, dfw2, coins_w2, horizons)

# constructing summary
extract_metrics <- function(model, window, results){
  results <- results %>%
    mutate(model = model, window = window, results) %>%
    select(model, window, everything())
}

summary_metrics <- bind_rows(
  extract_metrics("RF", "Window 1", results_rfw1),
  extract_metrics("RF", "Window 2", results_rfw2),
  extract_metrics("GB", "Window 1", results_gbw1),
  extract_metrics("GB", "Window 2", results_gbw2),
  extract_metrics("PCR", "Window 1", results_pcrw1),
  extract_metrics("PCR", "Window 2", results_pcrw2),
  extract_metrics("PLS", "Window 1", results_plsw1),
  extract_metrics("PLS", "Window 2", results_plsw2),
  extract_metrics("LASSO", "Window 1", results_lassow1),
  extract_metrics("LASSO", "Window 2", results_lassow2)
  )


summary_main <- summary_metrics %>%
  select(model, window, coin, horizon, accuracy, sensitivity, specificity,
         precision, recall, f1_score, auc, TP, TN, FP, FN, total_observations) %>%
  mutate(across(c(accuracy, sensitivity, specificity, precision, recall, f1_score, auc),
                ~ round(.x, 3))) %>%
  arrange(model, window, coin, horizon)

summary_compare <- summary_main %>%
  select(model, window, coin, horizon, accuracy, f1_score, auc, precision, recall) %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(accuracy, f1_score, auc, precision, recall)
  )


write.csv(
  summary_main,
  "../../model performance output/summary_main.csv",
  row.names = FALSE
)



# Plot Results ===========================================

#### Window 1
# DAI
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


# Plot AUC ===============================================

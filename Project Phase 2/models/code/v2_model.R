
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

prep_features <- function(data, target_col, remove_col, smote = FALSE, verbose = TRUE){
  
  feature_cols <- names(data)[!names(data) %in% remove_col]
  
  cleaned_data <- data %>% 
    filter(!is.na(!!sym(target_col))) %>%
    drop_na(all_of(feature_cols))
  
  X <- cleaned_data %>% select(all_of(feature_cols))
  y <- cleaned_data %>% pull(!!sym(target_col)) %>% as.factor()
  y <- factor(as.character(y), levels = c("0", "1"))
  
  if(smote){
    if (verbose) {
      cat("TRAIN before SMOTE:\n")
      print(table(y))
      cat("Proportion of depegs:",
          round(mean(as.character(y) == "1") * 100, 2), "%\n")
    }
    
    smote_out <- SMOTE(X, as.numeric(y) - 1, dup_size = 2, K = 3)
    smote_data <- smote_out$data
    
    X <- smote_data[, 1:ncol(X), drop = FALSE]
    y <- as.factor(smote_data[, ncol(smote_data)])
    y <- factor(as.character(y), levels = c("0", "1"))
    
    if (verbose) {
      cat("TRAIN after SMOTE:\n")
      print(table(y))
      cat("Proportion of depegs:",
          round(mean(as.character(y) == "1") * 100, 2), "%\n")
    }
    
  } else {
    if (verbose) {
      cat("\nTEST class distribution", target_col, ":\n")
      print(table(y))
      cat("Proportion of depegs:",
          round(mean(as.character(y) == "1") * 100, 2), "%\n\n")
    }
  }
  
  return(list(
    X = X,
    y = y,
    dates = cleaned_data$date,
    feature_names = feature_cols
  ))
}

remove_col <- c(
  "date", "open", "high", "low", "close",
  "ThreshD", "ThreshU", "value_classification",
  "depeg_1d", "depeg_3d", "depeg_5d", "depeg_7d",
  "token_name", "close_ust"
)

horizons <- c("depeg_1d", "depeg_3d", "depeg_5d", "depeg_7d")

#####################
### Random Forest ###
#####################
source("func-rf_roll.R")

# -----------------------------------
# Coins used in each evaluation window
# -----------------------------------
coins_window1 <- list(
  DAI  = data_DAI,
  PAX  = data_PAX,
  USDC = data_USDC,
  USDT = data_USDT,
  UST  = data_UST
)

coins_window2 <- list(
  DAI  = data_DAI,
  PAX  = data_PAX,
  USDC = data_USDC,
  USDT = data_USDT
)

# -----------------------------------
# Helper: run rolling RF over a date window
# -----------------------------------
run_rf_window <- function(data, target_col, remove_col,
                          train_start, train_end, test_start, test_end,
                          smote_train = TRUE,
                          expanding = FALSE,
                          step = 1,
                          verbose = FALSE,
                          prep_verbose = FALSE) {
  
  train_start <- as.Date(train_start)
  train_end   <- as.Date(train_end)
  test_start  <- as.Date(test_start)
  test_end    <- as.Date(test_end)
  
  data_sub <- data %>%
    arrange(date) %>%
    filter(date >= train_start & date <= test_end)
  
  train_window <- sum(data_sub$date >= train_start & data_sub$date <= train_end)
  
  if (train_window < 2) stop("Training window is too small.")
  if (sum(data_sub$date >= test_start & data_sub$date <= test_end) < 1) {
    stop("No test rows found in the requested date range.")
  }
  
  rf_obj <- rf_rolling_window(
    data = data_sub,
    target_col = target_col,
    remove_col = remove_col,
    train_window = train_window,
    test_window = 1,
    step = step,
    smote_train = smote_train,
    expanding = expanding,
    verbose = verbose,
    prep_verbose = prep_verbose
  )
  
  rf_obj$results <- rf_obj$results %>%
    filter(date >= test_start & date <= test_end)
  
  if (nrow(rf_obj$results) == 0) {
    stop("Rolling RF produced no rows in the requested test period.")
  }
  
  rf_obj$confusion_matrix <- table(
    Predicted = factor(rf_obj$results$pred_class, levels = c("0", "1")),
    Actual    = factor(rf_obj$results$actual, levels = c("0", "1"))
  )
  
  rf_obj$accuracy <- mean(
    factor(rf_obj$results$pred_class, levels = c("0", "1")) ==
      factor(rf_obj$results$actual, levels = c("0", "1")),
    na.rm = TRUE
  )
  
  rf_obj$window_info <- list(
    train_start = train_start,
    train_end   = train_end,
    test_start  = test_start,
    test_end    = test_end,
    train_window = train_window,
    expanding = expanding
  )
  
  rf_obj
}

# -----------------------------------
# Helper: run rolling RF for all coins and horizons
# -----------------------------------
run_rf_for_coins <- function(coin_list, horizons, remove_col,
                             train_start, train_end, test_start, test_end,
                             smote_train = TRUE,
                             expanding = FALSE,
                             step = 1,
                             verbose = FALSE,
                             prep_verbose = FALSE) {
  
  out <- list()
  
  for (coin in names(coin_list)) {
    cat("==========\n")
    cat("Rolling RF:", coin, "\n")
    cat("==========\n")
    
    out[[coin]] <- list()
    
    for (h in horizons) {
      cat("  Horizon:", h, "\n")
      out[[coin]][[h]] <- run_rf_window(
        data = coin_list[[coin]],
        target_col = h,
        remove_col = remove_col,
        train_start = train_start,
        train_end = train_end,
        test_start = test_start,
        test_end = test_end,
        smote_train = smote_train,
        expanding = expanding,
        step = step,
        verbose = verbose,
        prep_verbose = prep_verbose
      )
    }
  }
  
  out
}

# -----------------------------------
# Helper: metrics
# -----------------------------------
rf_metrics_nested <- function(rf_nested) {
  out <- list()
  
  for (coin in names(rf_nested)) {
    out[[coin]] <- list()
    for (h in names(rf_nested[[coin]])) {
      obj <- rf_nested[[coin]][[h]]
      out[[coin]][[h]] <- depeg_metrics(obj$results$actual, obj$results$pred_class)
    }
  }
  
  out
}

rf_summary_table <- function(rf_nested, window_label) {
  rows <- list()
  k <- 1
  
  for (coin in names(rf_nested)) {
    for (h in names(rf_nested[[coin]])) {
      obj <- rf_nested[[coin]][[h]]
      rows[[k]] <- data.frame(
        window = window_label,
        coin = coin,
        horizon = h,
        n_test = nrow(obj$results),
        actual_positives = sum(as.character(obj$results$actual) == "1", na.rm = TRUE),
        predicted_positives = sum(as.character(obj$results$pred_class) == "1", na.rm = TRUE),
        accuracy = obj$accuracy,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
  
  bind_rows(rows)
}

# -----------------------------------
# Helper: plot one coin across horizons
# -----------------------------------
plot_rf_coin_window <- function(rf_coin_results, coin_name, window_label) {
  plot_df <- bind_rows(lapply(names(rf_coin_results), function(h) {
    x <- rf_coin_results[[h]]$results
    if (nrow(x) == 0) return(NULL)
    
    data.frame(
      date = as.Date(x$date),
      horizon = h,
      actual = as.numeric(as.character(x$actual)),
      pred_class = as.numeric(as.character(x$pred_class)),
      pred_prob = as.numeric(x$pred_prob),
      stringsAsFactors = FALSE
    )
  }))
  
  ggplot(plot_df, aes(x = date)) +
    geom_line(aes(y = pred_prob, colour = "Predicted probability"), linewidth = 0.5) +
    geom_point(aes(y = actual, colour = "Actual class"), size = 1.3, alpha = 0.8) +
    geom_point(aes(y = pred_class, colour = "Predicted class"), shape = 1, size = 1.3, alpha = 0.8) +
    facet_wrap(~ horizon, ncol = 1, scales = "free_x") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(
      title = paste(window_label, "-", coin_name, "depeg predictions"),
      x = NULL,
      y = NULL,
      colour = NULL
    ) +
    theme_minimal()
}

# -----------------------------------
# Helper: safe AUC
# -----------------------------------
plot_auc_safe <- function(actual, score, title, add_ci = TRUE, add_optimal = TRUE) {
  actual_num <- as.numeric(as.character(actual))
  score_num  <- as.numeric(score)
  
  keep <- !is.na(actual_num) & !is.na(score_num)
  actual_num <- actual_num[keep]
  score_num  <- score_num[keep]
  
  if (length(unique(actual_num)) < 2) {
    message("Skipping AUC for ", title, ": actual has only one class.")
    return(NULL)
  }
  
  if (length(unique(score_num)) < 2) {
    message("Skipping AUC for ", title, ": score has only one unique value.")
    return(NULL)
  }
  
  plot_auc(
    actual_num,
    score_num,
    title = title,
    add_ci = add_ci,
    add_optimal = add_optimal
  )
}

# -----------------------------------
# Rolling RF: Window 1
# train 2020-11-25 to 2021-12-31
# test  2022-01-01 to 2022-05-08
# -----------------------------------
rf_w1 <- run_rf_for_coins(
  coin_list = coins_window1,
  horizons = horizons,
  remove_col = remove_col,
  train_start = "2020-11-25",
  train_end   = "2021-12-31",
  test_start  = "2022-01-01",
  test_end    = "2022-05-08",
  smote_train = TRUE,
  expanding = FALSE,
  step = 1,
  verbose = FALSE,
  prep_verbose = FALSE
)

# -----------------------------------
# Rolling RF: Window 2
# train 2019-11-22 to 2023-12-31
# test  2024-01-01 to 2025-12-31
# -----------------------------------
rf_w2 <- run_rf_for_coins(
  coin_list = coins_window2,
  horizons = horizons,
  remove_col = remove_col,
  train_start = "2019-11-22",
  train_end   = "2023-12-31",
  test_start  = "2024-01-01",
  test_end    = "2025-12-31",
  smote_train = TRUE,
  expanding = FALSE,
  step = 1,
  verbose = FALSE,
  prep_verbose = FALSE
)

# -----------------------------------
# Metrics / summary
# -----------------------------------
rf_metrics_w1 <- rf_metrics_nested(rf_w1)
rf_metrics_w2 <- rf_metrics_nested(rf_w2)

rf_summary_w1 <- rf_summary_table(rf_w1, "W1")
rf_summary_w2 <- rf_summary_table(rf_w2, "W2")
rf_summary_all <- bind_rows(rf_summary_w1, rf_summary_w2)

print(rf_summary_w1)
print(rf_summary_w2)

# -----------------------------------
# Plots
# -----------------------------------
plot_rf_w1 <- lapply(names(rf_w1), function(coin) {
  plot_rf_coin_window(rf_w1[[coin]], coin, "Rolling Window 1")
})
names(plot_rf_w1) <- names(rf_w1)

plot_rf_w2 <- lapply(names(rf_w2), function(coin) {
  plot_rf_coin_window(rf_w2[[coin]], coin, "Rolling Window 2")
})
names(plot_rf_w2) <- names(rf_w2)

rf_grid_w1 <- grid.arrange(grobs = plot_rf_w1, nrow = 2, ncol = 3)
ggsave("../../plots/RF_model_OOS_w1.png", rf_grid_w1, width = 12, height = 8)

rf_grid_w2 <- grid.arrange(grobs = plot_rf_w2, nrow = 2, ncol = 2)
ggsave("../../plots/RF_model_OOS_w2.png", rf_grid_w2, width = 12, height = 8)

# -----------------------------------
# Example AUC plots
# -----------------------------------
auc_rf_w1_DAI_7 <- plot_auc_safe(
  rf_w1$DAI$depeg_7d$results$actual,
  rf_w1$DAI$depeg_7d$results$pred_prob,
  title = "ROC: DAI depeg_7d (Rolling W1)",
  add_ci = TRUE,
  add_optimal = TRUE
)

auc_rf_w2_USDC_7 <- plot_auc_safe(
  rf_w2$USDC$depeg_7d$results$actual,
  rf_w2$USDC$depeg_7d$results$pred_prob,
  title = "ROC: USDC depeg_7d (Rolling W2)",
  add_ci = TRUE,
  add_optimal = TRUE
)



#########################
### Gradient Boosting ###
#########################
source("func-gradboost.R")

# Predicting the 6th column: close price
# Note: using the data_(token)_num dataset for modelling to avoid errors
gb_DAI = gb.rolling.window(data_DAI_num,nprev,6,1)
gb_PAX = gb.rolling.window(data_PAX_num,nprev,6,1)
gb_USDC = gb.rolling.window(data_USDC_num,nprev,6,1)
gb_USDT = gb.rolling.window(data_USDT_num,nprev,6,1)
gb_UST = gb.rolling.window(data_UST_num,nprev,6,1)

# Gradient Boosting RMSE's, use errors[2] for MAE
gb_rmse_DAI=gb_DAI$errors[1]
gb_rmse_PAX=gb_PAX$errors[1]
gb_rmse_USDC=gb_USDC$errors[1]
gb_rmse_USDT=gb_USDT$errors[1]
gb_rmse_UST=gb_UST$errors[1]

# Directional Predictability
# Note: Use the regular dataset for date column, so use 8th column to get close price
#gb_depeg_DAI <- depeg_metrics(oos_DAI[[5]], gb_DAI$pred, threshold)

# Plot OOS results, oos_DAI[[8]] is close price column
plot_gb_DAI <- plot_results(data_DAI, oos_DAI[[8]], nprev, gb_DAI, gb_rmse_DAI, 
                            "Gradient Boosting (DAI Close)")
plot_gb_PAX <- plot_results(data_PAX, oos_PAX[[8]], nprev, gb_PAX, gb_rmse_PAX, 
                            "Gradient Boosting (PAX Close)")
plot_gb_USDC <- plot_results(data_USDC, oos_USDC[[8]], nprev, gb_USDC, gb_rmse_USDC, 
                             "Gradient Boosting (USDC Close)")
plot_gb_USDT <- plot_results(data_USDT, oos_USDT[[8]], nprev, gb_USDT, gb_rmse_USDT, 
                             "Gradient Boosting (USDT Close)")
plot_gb_UST <- plot_results(data_UST, oos_UST[[5]], nprev, gb_UST, gb_rmse_UST, 
                            "Gradient Boosting (UST Close)")

plot_gb_list <- list(plot_gb_DAI, plot_gb_PAX, plot_gb_USDC, plot_gb_USDT, plot_gb_UST)
gb_grid <- grid.arrange(grobs = plot_gb_list, nrow = 2, ncol = 3)
ggsave("../../plots/GB_model_OOS.png", gb_grid, width = 12, height = 8)


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

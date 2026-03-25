
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


################################
### Depeg Prediction Metrics ###
################################

depeg_metrics <- function(actual, predicted) {
  
  # confusion matrix 
  TP <- sum(actual == 1 & predicted == 1, na.rm = TRUE)
  FP <- sum(actual == 0 & predicted == 1, na.rm = TRUE)
  TN <- sum(actual == 0 & predicted == 0, na.rm = TRUE)
  FN <- sum(actual == 1 & predicted == 0, na.rm = TRUE)
  
  # performance metrics
  accuracy <- (TP + TN) / (TP + FP + TN + FN) 
  misclass_rate <- 1 - accuracy
  sensitivity <- TP / (TP + FN) # want to maximise 
  specificity <- TN / (TN + FP)
  precision <- TP / (TP + FP)
  recall <- sensitivity
  f1 <- 2 / (1/precision + 1/recall)
  
  # cost ratios (FP vs FN): In the case of our project, FN is more detrimental
  FP_cost_ratio <- FP / (FP + FN)
  FN_cost_ratio <- FN / (FP + FN)
  
  return(list(
    accuracy = accuracy,
    misclassifications = misclass_rate,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = recall,
    f1_score = f1,
    confusion_matrix = c(TP = TP, TN = TN, FP = FP, FN = FN),
    FP_cost_ratio = FP_cost_ratio,
    FN_cost_ratio = FN_cost_ratio,
    total_observations = TP + TN + FP + FN
  ))
}

########################
### Plot OOS Results ###
########################

# actual_y = data$test$y or data$train$y
# need to include original stablecoin data which includes all variables (including OHLC, thresholds)
plot_results <- function(data, actual_y, pred_y, title, f1,
                         org_data = NULL, show_thresh = TRUE, test = TRUE) {
  if(test){
    dates <- data$test$dates
  } else {
    dates <- data$train$dates
  }
  
  # create base plot data with actual and predicted
  plot_data <- data.frame(
    date = dates, 
    actual = as.vector(actual_y), 
    predicted = as.vector(pred_y))
  
  if(!is.null(org_data) && show_thresh) {
    thresh_data <- org_data %>%
      filter(date %in% dates) %>%
      select(date, close, ThreshU, ThreshD)
    
    plot_data <- plot_data %>%
      left_join(thresh_data, by = "date")
  }
  
  plot <- ggplot(plot_data, aes(x = date)) +
    # add threshold bands first (so they appear in background)
    geom_ribbon(aes(ymin = ThreshD, ymax = ThreshU, fill = "Threshold Band"), 
                alpha = 0.2) +
    # add close price line
    geom_line(aes(y = close, color = "Close Price"), linewidth = 0.8) +
    # add actual depeg as points
    geom_point(aes(y = actual_y * max(close, na.rm = TRUE) * 0.95, 
                   color = "Actual Depeg"), 
               size = 2, shape = 18, na.rm = TRUE) +
    # add predicted depeg as points
    geom_point(aes(y = pred_y * max(close, na.rm = TRUE) * 0.95, 
                   color = "Predicted Depeg"), 
               size = 1.5, shape = 4, na.rm = TRUE) +
    # custom colors
    scale_color_manual(name = "Series",
                       values = c("Price" = "black", 
                                  "Actual Depeg" = "red", 
                                  "Predicted Depeg" = "blue")) +
    scale_fill_manual(name = "",
                      values = c("Threshold Band" = "grey70")) +
    labs(title = title,
         subtitle = paste("F1-Score:", round(f1, 4)),
         x = "Date", 
         y = "Price",
         caption = "Note: Depeg indicators shown at 95% of max price for visibility") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          plot.caption = element_text(size = 8, face = "italic"))
  
  return(plot)
}



plot_auc <- function(actual_y, pred_y, title, 
                     add_ci = FALSE, add_optimal = FALSE) {
  
  # convert actual to numeric if factor
  if(is.factor(actual_y)) {
    actual_y <- as.numeric(actual_y) - 1
  }
  
  # calculate ROC
  roc_obj <- roc(actual_y, pred_y)
  
  # calculate AUC
  auc_value <- auc(roc_obj)
  auc_text <- paste("AUC =", round(auc_value, 4))
  
  # add confidence interval if add_ci = TRUE
  if(add_ci) {
    ci_obj <- ci.auc(roc_obj)
    auc_text <- paste(auc_text, "\n95% CI: [", 
                      round(ci_obj[1], 4), ", ", 
                      round(ci_obj[3], 4), "]")
  }
  
  # find optimal threshold if add_optimal = TRUE
  if(add_optimal) {
    optimal <- coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"))
    optimal_thresh <- round(optimal$threshold, 4)
    optimal_sens <- round(optimal$sensitivity, 4)
    optimal_spec <- round(optimal$specificity, 4)
  }
  
  # plot ROC
  plot <- ggroc(roc_obj, color = "blue", size = 1) +
    geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray50") +
    coord_equal() +
    labs(title = title,
         subtitle = paste("AUC =", round(auc_value, 4)),
         x = "1 - Specificity (False Positive Rate)",
         y = "Sensitivity (True Positive Rate)") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # add optimal threshold annotation
  if(add_optimal) {
    plot <- plot +
      annotate("point", 
               x = 1 - optimal_spec, 
               y = optimal_sens, 
               color = "red", size = 3) +
      annotate("text", 
               x = 1 - optimal_spec + 0.05, 
               y = optimal_sens, 
               label = paste("Optimal threshold:", optimal_thresh),
               hjust = 0, size = 3)
  }
  
  # print additional metrics
  cat("\n--- ROC Analysis ---\n")
  cat("AUC:", round(auc_value, 4), "\n")
  if(add_ci) {
    cat("95% CI:", round(ci_obj[1], 4), "-", round(ci_obj[3], 4), "\n")
  }
  if(add_optimal) {
    cat("Optimal threshold:", optimal_thresh, "\n")
    cat("Sensitivity at optimal threshold:", optimal_sens, "\n")
    cat("Specificity at optimal threshold:", optimal_spec, "\n")
  }
  
  return(list(plot = plot, roc_obj = roc_obj, auc = auc_value))
}



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

DAI_1 <- dfw1$DAI$depeg_1d

#####################
### Random Forest ###
#####################
source("func-rf.R")


## Window 1: DAI
# ===============
rf_DAI_1 <- runrf(dfw1$DAI$depeg_1d$train, dfw1$DAI$depeg_1d$test, "DAI depeg_1d")
rf_DAI_3 <- runrf(dfw1$DAI$depeg_3d$train, dfw1$DAI$depeg_3d$test, "DAI depeg_3d")
rf_DAI_5 <- runrf(dfw1$DAI$depeg_5d$train, dfw1$DAI$depeg_5d$test, "DAI depeg_5d")
rf_DAI_7 <- runrf(dfw1$DAI$depeg_7d$train, dfw1$DAI$depeg_7d$test, "DAI depeg_7d")

rf_DAI_1_metrics <- depeg_metrics(dfw1$DAI$depeg_1d$test$y, rf_DAI_1$pred_class)
rf_DAI_3_metrics <- depeg_metrics(dfw1$DAI$depeg_3d$test$y, rf_DAI_3$pred_class)
rf_DAI_5_metrics <- depeg_metrics(dfw1$DAI$depeg_5d$test$y, rf_DAI_5$pred_class)
rf_DAI_7_metrics <- depeg_metrics(dfw1$DAI$depeg_7d$test$y, rf_DAI_7$pred_class)

plot_rf_DAI_1 <- plot_results(dfw1$DAI$depeg_1d, dfw1$DAI$depeg_1d$test$y, rf_DAI_1$pred_class, 
                              "Window 1: DAI depeg_1d", rf_DAI_1_metrics$f1,
                              org_data = data_DAI, show_thresh = TRUE, test = TRUE)
plot_rf_DAI_1

plot_rf_DAI_3 <- plot_results(dfw1$DAI$depeg_3d, dfw1$DAI$depeg_3d$test$y, rf_DAI_3$pred_class, 
                              "Window 1: DAI depeg_3d", rf_DAI_3_metrics$f1,
                              org_data = data_DAI, show_thresh = TRUE, test = TRUE)
plot_rf_DAI_3

plot_rf_DAI_5 <- plot_results(dfw1$DAI$depeg_5d, dfw1$DAI$depeg_5d$test$y, rf_DAI_5$pred_class, 
                              "Window 1: DAI depeg_5d", rf_DAI_5_metrics$f1,
                              org_data = data_DAI, show_thresh = TRUE, test = TRUE)
plot_rf_DAI_5

plot_rf_DAI_7 <- plot_results(dfw1$DAI$depeg_7d, dfw1$DAI$depeg_7d$test$y, rf_DAI_7$pred_class, 
                              "Window 1: DAI depeg_7d", rf_DAI_7_metrics$f1,
                              org_data = data_DAI, show_thresh = TRUE, test = TRUE)
plot_rf_DAI_7

# AUC has error here since all of it is just class 0
# auc_rf_DAI_1 <- plot_auc(dfw1$DAI$depeg_1d$test$y, rf_DAI_1$pred_class, 
#                         add_ci = TRUE, add_optimal = TRUE)
#auc_rf_DAI_1

# =======================
#  OLD CODE: DO NOT RUN 
# =======================
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

# =======================
#    END OF OLD CODE 
# =======================


#########################
### Gradient Boosting ###
#########################
#source("func-gradboost.R") # uncomment for old splitting logic
source("func-gradboost_edited.R")

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

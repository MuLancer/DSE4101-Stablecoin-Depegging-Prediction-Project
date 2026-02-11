library(gridExtra)
library(randomForest)
library(xgboost)
library(pls)
library(tidyverse)

rm(list=ls())

#################
### Load Data ###
#################

data <- read.csv("df_all.csv") 

################################
### Depeg Prediction Metrics ###
################################

# check whether depeg is identified based on depeg threshold

## TBU: update code for "threshold" ##

depeg_metrics <- function(actual, predicted, threshold) {
  # Compare actual and predicted prices to depeg threshold
  actual_depeg <- actual >= threshold
  predicted_depeg <- predicted >= threshold
  
  # confusion matrix 
  TP <- sum(actual_depeg == TRUE & predicted_depeg == TRUE, na.rm = TRUE)
  FP <- sum(actual_depeg == FALSE & predicted_depeg == TRUE, na.rm = TRUE)
  TN <- sum(actual_depeg == FALSE & predicted_depeg == FALSE, na.rm = TRUE)
  FN <- sum(actual_depeg == TRUE & predicted_depeg == FALSE, na.rm = TRUE)
  
  # performance metrics
  accuracy <- (TP + TN) / (TP + FP + TN + FN) 
  misclass_rate <- 1 - accuracy
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  precision <- TP / (TP + FP)
  recall <- sensitivity
  f1 <- 2 / (1/precision + 1/recall)
  
  # cost ratios (FP vs FN): In the case of our project, FP is more detrimental
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

plot_results <- function(y_data, oosy, nprev, model, rmse, title){
  oos_dates <- tail(y_data$date, nprev)
  
  plot_data <- data.frame(date = oos_dates, actual = as.vector(oosy), pred = as.vector(model$pred))
  
  plot <- ggplot(plot_data, aes(x = date)) +
    geom_line(aes(y = actual, color = "Actual")) +
    geom_line(aes(y = pred, color = "Predicted")) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    labs(title = title,
         subtitle = paste("RMSE:", round(rmse, 4)),
         x = "Index", 
         y = "Price",
         color = "Series") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plot)
}

###################################
### Set training and test split ###
###################################

set.seed(99)

# Convert "date" column into date type (previously character)
data$date <- as.Date(data$date)

# Split coins into individual datasets (for simplicity)
for (i in unique(data$token_name)){
    data_i <- data %>% filter(token_name == i) %>% arrange(date)
    var_name <- paste0("data_", i)
    assign(var_name, data_i)
}

# remove overlap OHLC price columns and bb (currently NA columns), remove NA rows
data_DAI <- data_DAI %>% 
  select(-DAI_open, -DAI_high, -DAI_low, -DAI_close, -bb) %>% 
  na.omit()
data_PAX <- data_PAX %>% 
  select(-PAX_open, -PAX_high, -PAX_low, -PAX_close, -bb) %>% 
  na.omit()
data_USDC <- data_USDC %>% 
  select(-USDC_open, -USDC_high, -USDC_low, -USDC_close, -bb) %>% 
  na.omit()
data_USDT <- data_USDT %>% 
  select(-USDT_open, -USDT_high, -USDT_low, -USDT_close, -bb) %>%
  na.omit()
data_UST <- data_UST %>% 
  select(-UST_open, -UST_high, -UST_low, -UST_close, -bb) %>%
  na.omit()
data_WLUNA <- data_WLUNA %>% 
  select(-WLUNA_open, -WLUNA_high, -WLUNA_low, -WLUNA_close, -bb) %>%
  na.omit()

# Numerical dataset FOR MODELS, and remove NA obs
data_DAI_num <- data_DAI %>% select(where(is.numeric))
data_PAX_num <- data_PAX %>% select(where(is.numeric))
data_USDC_num <- data_USDC %>% select(where(is.numeric))
data_USDT_num <- data_USDT %>% select(where(is.numeric))
data_UST_num <- data_UST %>% select(where(is.numeric))
data_WLUNA_num <- data_WLUNA %>% select(where(is.numeric))

# check number of observations, 70/30 train-test split
n = nrow(data_DAI_num)
nprev <- floor(0.3*n)

oos_DAI <- data_DAI %>% tail(nprev)
oos_PAX <- data_PAX %>% tail(nprev)
oos_USDC <- data_USDC %>% tail(nprev)
oos_USDT <- data_USDT %>% tail(nprev)
oos_UST <- data_UST %>% tail(nprev)
oos_WLUNA <- data_WLUNA %>% tail(nprev)


#####################
### Random Forest ###
#####################
source("func-rf.R")

# Predicting the 6th column: close price
# Note: using the data_(token)_num dataset for modelling to avoid errors
rf_DAI = rf.rolling.window(data_DAI_num,nprev,6,1)
rf_PAX = rf.rolling.window(data_PAX_num,nprev,6,1)
rf_USDC = rf.rolling.window(data_USDC_num,nprev,6,1)
rf_USDT = rf.rolling.window(data_USDT_num,nprev,6,1)
rf_UST = rf.rolling.window(data_UST_num,nprev,6,1)

# Random Forest RMSE's, use errors[2] for MAE
rf_rmse_DAI = rf_DAI$errors[1]
rf_rmse_PAX = rf_PAX$errors[1]
rf_rmse_USDC = rf_USDC$errors[1]
rf_rmse_USDT = rf_USDT$errors[1]
rf_rmse_UST = rf_UST$errors[1]

# Depeg Prediction
# Note: Use the regular dataset for date column, so use 8th column to get close price
#rf_depeg_DAI <- depeg_metrics(oos_DAI[[8]], rf_DAI$pred, threshold)

# Plot OOS results, oos_DAI[[8]] is close price column
plot_rf_DAI <- plot_results(data_DAI, oos_DAI[[8]], nprev, rf_DAI, rf_rmse_DAI, 
                            "Random Forest (DAI Close)")
plot_rf_PAX <- plot_results(data_PAX, oos_PAX[[8]], nprev, rf_PAX, rf_rmse_PAX, 
                            "Random Forest (PAX Close)")
plot_rf_USDC <- plot_results(data_USDC, oos_USDC[[8]], nprev, rf_USDC, rf_rmse_USDC, 
                             "Random Forest (USDC Close)")
plot_rf_USDT <- plot_results(data_USDT, oos_USDT[[8]], nprev, rf_USDT, rf_rmse_USDT, 
                             "Random Forest (USDT Close)")
plot_rf_UST <- plot_results(data_UST, oos_UST[[8]], nprev, rf_UST, rf_rmse_UST, 
                            "Random Forest (UST Close)")

plot_rf_list <- list(plot_rf_DAI, plot_rf_PAX, plot_rf_USDC, plot_rf_USDT, plot_rf_UST)
grid.arrange(grobs = plot_rf_list, nrow = 2, ncol = 3)


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
grid.arrange(grobs = plot_gb_list, nrow = 2, ncol = 3)


######################################
### Principal Component Regression ###
######################################
source("func-pcr.R")

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

# Note some additional functions: 
pcr_result_DAI <- runpcr(data_DAI_num, indice=4, lag=1)
summary(pcr_result_DAI$model)
#show_pcr_comps(pcr_result_DAI, top_n = 5)

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
grid.arrange(grobs = plot_pcr_list, nrow = 2, ncol = 3)




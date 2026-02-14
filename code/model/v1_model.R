
library(randomForest)
library(xgboost)
library(pls)
library(tidyverse)
library(gridExtra)

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
  actual_depeg <- actual <= threshold
  predicted_depeg <- predicted <= threshold
  
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

# remove overlap OHLC price columns, remove NA rows
data_DAI <- data_DAI %>% 
  select(-DAI_open, -DAI_high, -DAI_low, -DAI_close) %>% 
  na.omit()
data_PAX <- data_PAX %>% 
  select(-PAX_open, -PAX_high, -PAX_low, -PAX_close) %>% 
  na.omit()
data_USDC <- data_USDC %>% 
  select(-USDC_open, -USDC_high, -USDC_low, -USDC_close) %>% 
  na.omit()
data_USDT <- data_USDT %>% 
  select(-USDT_open, -USDT_high, -USDT_low, -USDT_close) %>%
  na.omit()
data_UST <- data_UST %>% 
  select(-UST_open, -UST_high, -UST_low, -UST_close) %>%
  na.omit()
data_WLUNA <- data_WLUNA %>% 
  select(-WLUNA_open, -WLUNA_high, -WLUNA_low, -WLUNA_close) %>%
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
#source("func-rf.R") # uncomment for old splitting logic
source("func-rf_edited.R")

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
grid.arrange(grobs = plot_gb_list, nrow = 2, ncol = 3)


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
grid.arrange(grobs = plot_pcr_list, nrow = 2, ncol = 3)


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




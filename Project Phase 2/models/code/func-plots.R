# Function file containing plot_results and plot_auc
# plot_results plots the depeg actual and predictions against close price during the test period
# plot auc plots the ROC curve 


# might need to jitter points
plot_results <- function(coin_data, org_data, title) {
  
  # coin_data should be a list containing results for each horizon
  # E.g. list(
  #   depeg_1d = list(test = list(dates = ..., y = ...), pred = ...),
  #   depeg_3d = list(test = list(dates = ..., y = ...), pred = ...),
  #   ...
  # )
  
  combined <- data.frame()
  
  #horizon_colours <- c("depeg_1d" = "blue",
  #                     "depeg_3d" = "orange",
  #                     "depeg_5d" = "forestgreen",
  #                     "depeg_7d" = "red3")
  
  price_colours <- c("close" = "blue",
                     "high" = "forestgreen",
                     "low" = "orange")
  
  for(horizon in names(coin_data)){
    horizon_data <- coin_data[[horizon]]
    
    dates <- horizon_data$test$dates
    actual_y <- as.vector(horizon_data$test$y)
    pred_y <- as.vector(horizon_data$pred)
    
    temp <- data.frame(date = dates, actual = actual_y,
                       pred = pred_y, horizon = horizon)
    
    combined <- rbind(combined, temp)
  }
  
  all_dates <- unique(combined$date)
  thresh_data <- org_data %>% 
    filter(date %in% all_dates) %>%
    select(date, close, high, low, ThreshU, ThreshD) %>%
    arrange(date)
  max_price <- max(thresh_data$close, na.rm = TRUE)
  
  plot <- ggplot() +
    facet_wrap(~ horizon, ncol = 2) +
    
    # threshold band
    geom_ribbon(data = thresh_data,
                aes(x = date, ymin = ThreshD, ymax = ThreshU, 
                    fill = "Threshold Band"), alpha = 0.4) +
    
    geom_line(data = thresh_data,aes(x = date, y = ThreshD), 
              alpha = 0.4, linetype = "dotted") + 
    geom_line(data = thresh_data,aes(x = date, y = ThreshU), 
              alpha = 0.4, linetype = "dotted") + 
    
    # depeg lines
    geom_vline(data = combined %>% filter(actual == 1),
               aes(xintercept = date),
               linetype = "solid", alpha = 0.4, color = "forestgreen") +
    geom_vline(data = combined %>% filter(pred == 1),
               aes(xintercept = date),
               linetype = "dashed", alpha = 0.4, color = "red") +
    
    # price lines
    geom_line(data = thresh_data, 
              aes(x = date, y = close, colour = "Close"),
              linewidth = 0.8) +
    geom_line(data = thresh_data, 
              aes(x = date, y = high, colour = "High"),
              linewidth = 0.8) +
    geom_line(data = thresh_data, 
              aes(x = date, y = low, colour = "Low"),
              linewidth = 0.8) +
    
    #scale_color_manual(name = "Horizon",
    #                   values = horizon_colours) +
    scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
    scale_fill_manual(name = '',
                      values = c("Threshold Band" = "grey80")) +
    scale_color_manual(name = "Price", 
                       values = c("Close" = "grey20",
                                  "High" = "blue",
                                  "Low" = "orange")) +
    scale_x_date(date_labels = "%Y-%m-%d") + 
    labs(title = title,
         caption = "Depeg occurs when low price < ThreshD or high price > ThreshU
         \nActual depegs (green solid lines) | Predicted depegs (red dashed lines)",
         x = "Date",
         y = "Price") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "vertical")
  
  return(plot)
}


plot_auc <- function(actual_y, pred_y, title, 
                     add_ci = FALSE, add_optimal = FALSE) {
  
  # convert actual to numeric if factor
  if(is.factor(actual_y)) {
    actual_y <- as.numeric(actual_y) - 1
  }
  
  if(is.factor(pred_y)) {
    pred_y <- as.numeric(pred_y) - 1
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
# Function file containing depeg metrics calculation
# Output is a list containing the metrics calculated
# Input: actual - vector of actual y values, predicted - vector of predicted y values

depeg_metrics <- function(actual, predicted, pred_prob) {
  
  # confusion matrix 
  TP <- sum(actual == 1 & predicted == 1, na.rm = TRUE)
  FP <- sum(actual == 0 & predicted == 1, na.rm = TRUE)
  TN <- sum(actual == 0 & predicted == 0, na.rm = TRUE)
  FN <- sum(actual == 1 & predicted == 0, na.rm = TRUE)
  
  # performance metrics
  accuracy <- (TP + TN) / (TP + FP + TN + FN)
  misclass_rate <- 1 - accuracy
  sensitivity <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  specificity <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1  <- ifelse((precision + recall) == 0, 0, 
                2 * precision * recall / (precision + recall))
  
  # cost ratios
  FP_cost_ratio <- FP / (FP + FN)
  FN_cost_ratio <- FN / (FP + FN)
  
  # AUC calculation
  actual_num <- if (is.factor(actual)) {
    as.numeric(as.character(actual))
    } else {
      as.numeric(actual)
      }
  
  # AUC only defined if both classes present
  if (length(unique(actual_num)) == 2) {
    roc_obj <- pROC::roc(actual_num, pred_prob, quiet = TRUE)
    auc <- as.numeric(pROC::auc(roc_obj))
    } else {
      warning("Only one class present in actuals. AUC undefined -> set to 0.")
      auc <- 0
      }
  
  return(list(
    accuracy = accuracy,
    misclassifications = misclass_rate,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = recall,
    f1_score = f1,
    auc = auc,
    confusion_matrix = c(TP = TP, TN = TN, FP = FP, FN = FN),
    FP_cost_ratio = FP_cost_ratio,
    FN_cost_ratio = FN_cost_ratio,
    total_observations = TP + TN + FP + FN))
}


get_all_metrics <- function(model_results, dfw, coins, horizons) {
  
  all_results <- list()
  k <- 1
  
  for (c in coins) {
    for (h in horizons) {
      
      # extract actual + predictions
      actual <- dfw[[c]][[h]]$test$y
      predicted <- model_results[[c]][[h]]$pred_class
      
      # optional probabilities (for AUC)
      pred_prob <- NULL
      if (!is.null(model_results[[c]][[h]]$pred_prob)) {
        pred_prob <- model_results[[c]][[h]]$pred_prob
      }
      
      # compute metrics
      metrics <- depeg_metrics(
        actual = actual,
        predicted = predicted,
        pred_prob = pred_prob)
      
      # store as row
      all_results[[k]] <- data.frame(
        coin = c,
        horizon = h,
        accuracy = metrics$accuracy,
        f1_score = metrics$f1_score,
        precision = metrics$precision,
        recall = metrics$recall,
        sensitivity = metrics$sensitivity,
        specificity = metrics$specificity,
        auc = metrics$auc,
        TP = metrics$confusion_matrix["TP"],
        TN = metrics$confusion_matrix["TN"],
        FP = metrics$confusion_matrix["FP"],
        FN = metrics$confusion_matrix["FN"],
        total_observations = metrics$total_observations)
      
      k <- k + 1
    }
  }
  
  dplyr::bind_rows(all_results)
}
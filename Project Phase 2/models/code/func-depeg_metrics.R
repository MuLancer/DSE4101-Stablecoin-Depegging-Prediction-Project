# Function file containing depeg metrics calculation
# Output is a list containing the metrics calculated
# Input: actual - vector of actual y values, predicted - vector of predicted y values

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
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
  sensitivity <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  specificity <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1  <- ifelse((precision + recall) == 0, 0, 
                      2 * precision * recall / (precision + recall))
  
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
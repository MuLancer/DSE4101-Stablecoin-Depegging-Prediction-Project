
rungb <- function(train_data, test_data, title) {
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(as.character(y_train)))
  dtest <- xgb.DMatrix(data = as.matrix(X_test))
  
  # Fixed parameters
  gb_params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.1,             # no change, tried lower learning rate (0.01-0.05), but 0.1 still better
    max_depth = 6,         # no change, 6 splits to balance complexity 
    subsample = 0.8,       # no change, using 80% randomly selected obs for training
    colsample_bytree = 0.8 # no change, using 80% randomly selected features for constructing each tree
  )
  
  # Train model
  gb_model <- xgb.train(
    params = gb_params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Prediction probabilities for class 1 (depeg)
  pred_prob <- predict(gb_model, dtest)
  
  pred_class <- ifelse(pred_prob >= 0.5, "1", "0")
  pred_class <- factor(pred_class, levels = levels(y_test))
  
  # confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm)) / sum(cm)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  print(cm)
  
  importance_matrix <- xgb.importance(model = gb_model)
  
  return(list(model = gb_model, 
              pred_prob = pred_prob,
              pred_class = pred_class,
              importance = importance_matrix,
              confusion_matrix = cm))
}


run_gb_all <- function(dfw, coin_list, horizons) {
  all_results <- list()
  
  for(coin in names(coin_list)) {
    cat("RUNNING GRADIENT BOOSTING FOR:", coin)
    
    coin_results <- list()
    
    for(h in horizons) {
      cat("\n---", h, "---\n")
      train_data <- dfw[[coin]][[h]]$train
      test_data <- dfw[[coin]][[h]]$test
      
      cat("Training class distribution:")
      print(table(train_data$y))
      cat("\nTest class distribution:")
      print(table(test_data$y))
      
      title <- paste(coin, ":", h)
      gb_results <- rungb(train_data = train_data,
                          test_data = test_data,
                          title = title)
      
      coin_results[[h]] <- gb_results
    }
    
    all_results[[coin]] <- coin_results
  }
  
  return(all_results)
}



gb.rolling.window <- function(Y, nprev, indice = 1, lag = 1) {
  
  save.pred <- matrix(NA, nprev, 1)
  
  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), ] # create expanding window 
    gb_model <- rungb(Y.window, indice, lag) # run GB model
    save.pred[(1 + nprev - i), ] <- gb_model$pred
    cat("iteration", (1 + nprev - i), "\n")
  }
  
  # Calculate errors
  real <- Y[, indice]
  oos_real <- tail(real, nprev)
  
  rmse <- sqrt(mean((oos_real - save.pred)^2))
  mae <- mean(abs(oos_real - save.pred))
  errors <- c("rmse" = rmse, "mae" = mae)
  
  # Plot results
  plot(real, type = "l", main = "Gradient Boosting Forecast")
  lines(c(rep(NA, length(real) - nprev), save.pred), col = "red")
  legend("topleft", legend = c("Actual", "Predicted"), 
         col = c("black", "red"), lty = 1)
  
  return(list("pred" = save.pred, "errors" = errors))
}

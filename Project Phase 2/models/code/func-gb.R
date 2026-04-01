
rungb <- function(train_data, test_data, title) {
  
  library(xgboost)
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  X_train_scaled <- scale(X_train) # Need to scale to avoid issues (Input `inf` error)
  train_center <- attr(X_train_scaled, "scaled:center")
  train_scale  <- attr(X_train_scaled, "scaled:scale")
  X_test_scaled <- scale(X_test, center = train_center, scale = train_scale)
  
  dtrain <- xgb.DMatrix(data = data.matrix(X_train_scaled),
                        label = as.numeric(as.character(y_train)))
  dtest <- xgb.DMatrix(data = data.matrix(X_test_scaled))

  # Parameter grid ==============================
  param_grid <- expand.grid(
    max_depth = c(2, 5, 6),         # shallower trees to avoid overfit, 6 is default
    eta = c(0.01, 0.1),             # slower and faster learning rate
    subsample = c(0.5, 0.7, 0.8),   # using half, 70% and 80% (default) of randomly selected obs for training
    colsample_bytree = c(0.5, 0.7, 0.8) # using half, 70% and 80% (default) of randomly selected features for training
  )
  
  nrounds_cap <- 1000
  early_stop <- 75
  nfold <- 5
  
  best_logloss <- Inf
  best_params <- NULL
  best_nrounds <- NULL
  
  # Grid search + CV ==============================
  for(i in 1:nrow(param_grid)) {
    
    params <- list(objective = "binary:logistic",
                   eval_metric = "logloss",
                   max_depth = param_grid$max_depth[i],
                   eta = param_grid$eta[i],
                   subsample = param_grid$subsample[i],
                   colsample_bytree = param_grid$colsample_bytree[i])
    
    cv <- xgb.cv(params = params,
                 data = dtrain,
                 nrounds = nrounds_cap,
                 nfold = nfold,
                 early_stopping_rounds = early_stop,
                 verbose = 0)
    
    mean_logloss <- min(cv$evaluation_log$test_logloss_mean)
    best_iter <- cv$best_iteration
    
    if(mean_logloss < best_logloss) {
      best_logloss <- mean_logloss
      best_params <- params
      best_nrounds <- best_iter
    }
  }
  
  # Backup if tuning failed =====================
  # e.g. if nrounds == 0 because of class imbalance
  if (is.null(best_nrounds)) {
    cat("WARNING: nrounds == 0. Using default nrounds == 50.\n")
    best_nrounds <- 50
  }
  
  if (is.null(best_params)) {
    cat("WARNING: No valid CV result. Using default parameters.\n")
    best_params <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 3,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
  }
  
  # Train final model on best params =================
  gb_model <- xgb.train(params = best_params,
                        data = dtrain,
                        nrounds = best_nrounds,
                        verbose = 0)
  
  # Compute predictions ==============================
  pred_prob <- predict(gb_model, dtest)
  pred_class <- ifelse(pred_prob >= 0.5, "1", "0")
  pred_class <- factor(pred_class, levels = levels(y_test))
  
  # confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm)) / sum(cm)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Best Params:\n")
  print(best_params)
  cat("Best nrounds:", best_nrounds, "\n")
  
  print(cm)
  
  importance_matrix <- xgb.importance(model = gb_model)
  
  return(list(
    model = gb_model, 
    pred_prob = pred_prob,
    pred_class = pred_class,
    importance = importance_matrix,
    confusion_matrix = cm,
    X_test_scaled = X_test_scaled,
    tuning = list(
      best_params = best_params,
      best_nrounds = best_nrounds,
      best_logloss = best_logloss
    )
  ))
}


rungb_all <- function(dfw, coin_list, horizons) {
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





############
# Old Code #
############

rungb_default <- function(train_data, test_data, title) {
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  X_train_scaled <- scale(X_train) # Need to scale to avoid issues (Input `inf` error)
  train_center <- attr(X_train_scaled, "scaled:center")
  train_scale  <- attr(X_train_scaled, "scaled:scale")
  X_test_scaled <- scale(X_test, center = train_center, scale  = train_scale)
  
  #cat("Train range:", range(X_train_scaled), "\n")
  #cat("Test range:", range(X_test_scaled), "\n")
  
  dtrain <- xgb.DMatrix(data = data.matrix(X_train_scaled), label = as.numeric(as.character(y_train)))
  dtest <- xgb.DMatrix(data = data.matrix(X_test_scaled))
  
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


# indice: predict the i column of Y

rungb <- function(Y, indice, lag) {
  
  Y2 <- as.matrix(Y)
  n  <- nrow(Y2)
  
  # y_{t+lag}
  y_all <- Y2[(1 + lag):n, indice]
  
  # X_t (info available at time t)
  X_all <- Y2[1:(n - lag), , drop = FALSE]
  
  # remove the target column from predictors
  X_all <- X_all[, -indice, drop = FALSE]
  
  # train on all but last, predict last
  X_train <- X_all[-nrow(X_all), , drop = FALSE]
  y_train <- y_all[-length(y_all)]
  X.out   <- X_all[nrow(X_all), , drop = FALSE]
  
  # Convert to matrix format for xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest <- xgb.DMatrix(data = matrix(X.out, nrow = 1))
  
  # Fixed parameters
  gb_params <- list(
    objective = "reg:squarederror",
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
  
  # Generate prediction
  pred <- predict(gb_model, dtest)
  
  return(list("model" = gb_model, "pred" = pred))
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

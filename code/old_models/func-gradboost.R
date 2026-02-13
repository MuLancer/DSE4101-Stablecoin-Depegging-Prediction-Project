
# indice: predict the i column of Y

rungb <- function(Y, indice, lag) {
  
  Y2 <- as.matrix(Y)
  aux <- embed(Y2, 1 + lag) # create 1 lags + forecast horizon shift (=lag option)
  
  y <- aux[, indice] # target variable in the first column
  X <- aux[, -indice, drop = FALSE] # predictors
  
  X.out <- as.numeric(tail(Y2, 1))
  
  # Convert to matrix format for xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)
  dtest <- xgb.DMatrix(data = matrix(X.out, nrow = 1))
  
  # Fixed parameters
  gb_params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
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
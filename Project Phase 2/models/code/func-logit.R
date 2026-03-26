runlogit <- function(train, test, target_name = NULL, cv_folds = 5) {
  
  X_train <- train$X
  X_test  <- test$X
  
  y_train <- train$y
  y_test  <- test$y
  
  if (is.factor(y_train)) {
    y_train <- as.numeric(as.character(y_train))
  }
  if (is.factor(y_test)) {
    y_test <- as.numeric(as.character(y_test))
  }
  
  if (!all(y_train %in% c(0, 1))) {
    stop("train$y must be binary 0/1.")
  }
  if (!all(y_test %in% c(0, 1))) {
    stop("test$y must be binary 0/1.")
  }
  
  common_cols <- intersect(colnames(X_train), colnames(X_test))
  X_train <- X_train[, common_cols, drop = FALSE]
  X_test  <- X_test[, common_cols, drop = FALSE]
  X_test  <- X_test[, colnames(X_train), drop = FALSE]
  
  train_data <- data.frame(y = y_train, X_train, check.names = FALSE)
  test_data  <- data.frame(X_test, check.names = FALSE)
  
  n_obs <- nrow(train_data)
  fold_size <- floor(n_obs / cv_folds)
  cv_errors <- rep(NA, cv_folds)
  
  for (fold in 1:cv_folds) {
    test_indices <- ((fold - 1) * fold_size + 1):min(fold * fold_size, n_obs)
    train_indices <- setdiff(seq_len(n_obs), test_indices)
    
    if (length(test_indices) == 0 || length(train_indices) == 0) next
    
    train_fold <- train_data[train_indices, , drop = FALSE]
    valid_fold <- train_data[test_indices, , drop = FALSE]
    
    if (length(unique(train_fold$y)) < 2) next
    
    fit <- glm(y ~ ., data = train_fold, family = binomial())
    p <- predict(fit, newdata = valid_fold, type = "response")
    p <- pmin(pmax(p, 1e-8), 1 - 1e-8)
    
    cv_errors[fold] <- -mean(valid_fold$y * log(p) + (1 - valid_fold$y) * log(1 - p))
  }
  
  mean_cv_error <- mean(cv_errors, na.rm = TRUE)
  
  model <- glm(y ~ ., data = train_data, family = binomial())
  coefficients <- coef(model)
  
  pred_prob <- as.numeric(predict(model, newdata = test_data, type = "response"))
  pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
  
  p <- pmin(pmax(pred_prob, 1e-8), 1 - 1e-8)
  errors <- c(
    accuracy = mean(y_test == pred_class, na.rm = TRUE),
    mae = mean(abs(y_test - pred_prob), na.rm = TRUE),
    logloss = -mean(y_test * log(p) + (1 - y_test) * log(1 - p), na.rm = TRUE)
  )
  
  cat(sprintf("Test Accuracy: %.4f\n", errors["accuracy"]))
  
  return(list(
    model = model,
    coefficients = coefficients,
    cv_errors = cv_errors,
    mean_cv_error = mean_cv_error,
    pred_prob = pred_prob,
    pred_class = pred_class,
    errors = errors
  ))
}




##rolling window, not used yet 
logit.rolling.window <- function(data, target_name, window_size, 
                                 date_col = NULL, cv_folds = 5) {
  
  data <- as.data.frame(data)
  
  if (!(target_name %in% colnames(data))) {
    stop("target_name not found in data.")
  }
  
  n <- nrow(data)
  if (n <= window_size) {
    stop("window_size must be smaller than number of rows in data.")
  }
  
  predictor_cols <- setdiff(colnames(data), c(target_name, date_col))
  
  save.pred_prob <- rep(NA, n - window_size)
  save.pred_class <- rep(NA, n - window_size)
  save.cv_errors <- vector("list", n - window_size)
  save.coefficients <- vector("list", n - window_size)
  save.actual <- rep(NA, n - window_size)
  save.date <- vector("list", n - window_size)
  
  for (i in (window_size + 1):n) {
    
    # Rolling train/test split
    train_data <- data[(i - window_size):(i - 1), , drop = FALSE]
    test_data  <- data[i, , drop = FALSE]
    
    # Build nested objects expected by runlogit()
    train <- list(
      X = train_data[, predictor_cols, drop = FALSE],
      y = train_data[[target_name]]
    )
    
    test <- list(
      X = test_data[, predictor_cols, drop = FALSE],
      y = test_data[[target_name]]
    )
    
    # Skip if train window has only one class
    y_train_check <- train$y
    if (is.factor(y_train_check)) {
      y_train_check <- as.numeric(as.character(y_train_check))
    }
    
    if (length(unique(y_train_check)) < 2) {
      next
    }
    
    logit_model <- runlogit(train = train, test = test, cv_folds = cv_folds)
    
    idx <- i - window_size
    save.pred_prob[idx] <- logit_model$pred_prob[1]
    save.pred_class[idx] <- logit_model$pred_class[1]
    save.cv_errors[[idx]] <- logit_model$cv_errors
    save.coefficients[[idx]] <- logit_model$coefficients
    
    y_test_val <- test$y
    if (is.factor(y_test_val)) {
      y_test_val <- as.numeric(as.character(y_test_val))
    }
    save.actual[idx] <- y_test_val[1]
    
    if (!is.null(date_col) && date_col %in% colnames(test_data)) {
      save.date[[idx]] <- test_data[[date_col]][1]
    } else {
      save.date[[idx]] <- i
    }
    
    cat("iteration", idx, "\n")
  }
  
  p <- pmin(pmax(save.pred_prob, 1e-8), 1 - 1e-8)
  
  errors <- c(
    accuracy = mean(save.actual == save.pred_class, na.rm = TRUE),
    mae = mean(abs(save.actual - save.pred_prob), na.rm = TRUE),
    logloss = -mean(save.actual * log(p) + (1 - save.actual) * log(1 - p), na.rm = TRUE)
  )
  
  results <- data.frame(
    date = unlist(save.date),
    actual = save.actual,
    pred_prob = save.pred_prob,
    pred_class = save.pred_class
  )
  
  return(list(
    results = results,
    pred_prob = save.pred_prob,
    pred_class = save.pred_class,
    actual = save.actual,
    cv_errors = save.cv_errors,
    coefficients = save.coefficients,
    errors = errors
  ))
}


## run logit for all coins, across all horizons
runlogit_all <- function(dfw, coin_list, horizons) {
  all_results <- list()
  
  for (coin in names(coin_list)) {
    cat("RUNNING LOGISTIC REGRESSION FOR:", coin, "\n")
    coin_results <- list()
    
    for (h in horizons) {
      cat("\n---", h, "---\n")
      
      train_data <- dfw[[coin]][[h]]$train
      test_data  <- dfw[[coin]][[h]]$test
      
      cat("Training class distribution:\n")
      print(table(train_data$y))
      cat("\nTest class distribution:\n")
      print(table(test_data$y))
      
      logit_results <- runlogit(
        train = train_data,
        test = test_data
      )
      
      coin_results[[h]] <- logit_results
    }
    
    all_results[[coin]] <- coin_results
  }
  
  return(all_results)
}


#plot horizon performance (also used for lasso)

plot_horizon_performance <- function(logit_1, logit_3, logit_5, logit_7) {
  df <- data.frame(
    horizon = c("1d", "3d", "5d", "7d"),
    accuracy = c(logit_1$errors["accuracy"], logit_3$errors["accuracy"],
                 logit_5$errors["accuracy"], logit_7$errors["accuracy"]),
    logloss = c(logit_1$errors["logloss"], logit_3$errors["logloss"],
                logit_5$errors["logloss"], logit_7$errors["logloss"])
  )
  
  df_long <- reshape(
    df,
    varying = c("accuracy", "logloss"),
    v.names = "value",
    timevar = "metric",
    times = c("accuracy", "logloss"),
    direction = "long"
  )
  
  ggplot(df_long, aes(x = horizon, y = value, fill = metric)) +
    geom_col(position = "dodge") +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = "Logistic Regression Performance by Horizon",
      x = "Forecast Horizon",
      y = "Value"
    ) +
    theme_minimal()
}


#show logit coeffs
show_logit_coeffs <- function(logit_result, top_n = 10) {
  model_summary <- summary(logit_result$model)
  coef_table <- model_summary$coefficients
  
  # remove intercept
  coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
  
  if (nrow(coef_table) == 0) {
    cat("No predictor coefficients found.\n")
    return(invisible(NULL))
  }
  
  # build results table
  results <- data.frame(
    predictor = rownames(coef_table),
    coefficient = coef_table[, "Estimate"],
    odds_ratio = exp(coef_table[, "Estimate"]),
    p_value = coef_table[, "Pr(>|z|)"],
    abs_coef = abs(coef_table[, "Estimate"]),
    row.names = NULL
  )
  
  # rank by absolute coefficient size
  results <- results[order(results$abs_coef, decreasing = TRUE), ]
  results <- results[1:min(top_n, nrow(results)), ]
  
  cat("=== Logistic Regression - Top Predictors ===\n\n")
  
  for (i in 1:nrow(results)) {
    cat(sprintf(
      "%2d. %-30s coef = %8.4f | odds ratio = %8.4f | p-value = %.4g\n",
      i,
      results$predictor[i],
      results$coefficient[i],
      results$odds_ratio[i],
      results$p_value[i]
    ))
  }
  
  invisible(results)
}
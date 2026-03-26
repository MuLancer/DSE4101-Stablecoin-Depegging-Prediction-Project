
run_lasso <- function(train, test, cv_folds = 5) {
  
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required. Install it with install.packages('glmnet').")
  }
  
  X_train <- as.data.frame(train$X)
  X_test  <- as.data.frame(test$X)
  
  y_train <- train$y
  y_test  <- test$y
  
  if (is.factor(y_train)) y_train <- as.character(y_train)
  if (is.factor(y_test)) y_test <- as.character(y_test)
  
  y_train <- as.numeric(y_train)
  y_test  <- as.numeric(y_test)
  
  if (any(is.na(y_train))) stop("train$y contains NA or non-numeric class labels.")
  if (any(is.na(y_test))) stop("test$y contains NA or non-numeric class labels.")
  
  if (!all(y_train %in% c(0, 1))) stop("train$y must be binary 0/1.")
  if (!all(y_test %in% c(0, 1))) stop("test$y must be binary 0/1.")
  
  common_cols <- intersect(colnames(X_train), colnames(X_test))
  X_train <- X_train[, common_cols, drop = FALSE]
  X_test  <- X_test[, common_cols, drop = FALSE]
  X_test  <- X_test[, colnames(X_train), drop = FALSE]
  
  x_train <- as.matrix(X_train)
  x_test  <- as.matrix(X_test)
  
  storage.mode(x_train) <- "double"
  storage.mode(x_test) <- "double"
  
  cvfit <- glmnet::cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = 1,
    nfolds = cv_folds,
    type.measure = "deviance"
  )
  
  coef_sparse <- coef(cvfit, s = "lambda.min")
  coefficients <- as.matrix(coef_sparse)
  
  pred_prob <- as.numeric(
    predict(cvfit, newx = x_test, s = "lambda.min", type = "response")
  )
  pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
  
  p <- pmin(pmax(pred_prob, 1e-8), 1 - 1e-8)
  errors <- c(
    accuracy = mean(y_test == pred_class, na.rm = TRUE),
    mae = mean(abs(y_test - pred_prob), na.rm = TRUE),
    logloss = -mean(y_test * log(p) + (1 - y_test) * log(1 - p), na.rm = TRUE)
  )
  
  cat(sprintf("Test Accuracy: %.4f\n", errors["accuracy"]))
  
  list(
    model = cvfit,
    coefficients = coefficients,
    lambda_min = cvfit$lambda.min,
    lambda_1se = cvfit$lambda.1se,
    cv_errors = cvfit$cvm,
    mean_cv_error = min(cvfit$cvm, na.rm = TRUE),
    pred_prob = pred_prob,
    pred_class = pred_class,
    errors = errors
  )
}


## run lasso for all coins,  across all horizons
run_lasso_all <- function(dfw, coin_list, horizons) {
  all_results <- list()
  
  for (coin in names(coin_list)) {
    cat("RUNNING LASSO LOGISTIC REGRESSION FOR:", coin, "\n")
    coin_results <- list()
    
    for (h in horizons) {
      cat("\n---", h, "---\n")
      
      train_data <- dfw[[coin]][[h]]$train
      test_data  <- dfw[[coin]][[h]]$test
      
      cat("Training class distribution:\n")
      print(table(train_data$y))
      cat("\nTest class distribution:\n")
      print(table(test_data$y))
      
      logit_results <- run_lasso(
        train = train_data,
        test = test_data
      )
      
      coin_results[[h]] <- logit_results
    }
    
    all_results[[coin]] <- coin_results
  }
  
  return(all_results)
}

#show lasso coeffs
show_lasso_coeffs <- function(lasso_result, top_n = 10) {
  coef_mat <- as.matrix(lasso_result$coefficients)
  
  results <- data.frame(
    predictor = rownames(coef_mat),
    coefficient = coef_mat[, 1],
    odds_ratio = exp(coef_mat[, 1]),
    abs_coef = abs(coef_mat[, 1]),
    row.names = NULL
  )
  
  results <- results[results$predictor != "(Intercept)", , drop = FALSE]
  
  total_predictors <- nrow(results)
  selected_results <- results[results$coefficient != 0, , drop = FALSE]
  selected_count <- nrow(selected_results)
  removed_count <- total_predictors - selected_count
  
  cat("=== Lasso Logistic Regression - Top Predictors ===\n\n")
  cat(sprintf("lambda.min used: %.6f\n", lasso_result$lambda_min))
  cat(sprintf("lambda.1se      : %.6f\n", lasso_result$lambda_1se))
  cat(sprintf("Selected predictors: %d / %d\n", selected_count, total_predictors))
  cat(sprintf("Shrunk to zero     : %d / %d (%.1f%%)\n\n",
              removed_count, total_predictors,
              100 * removed_count / total_predictors))
  
  if (selected_count == 0) {
    cat("No predictors selected by lasso.\n")
    return(invisible(NULL))
  }
  
  selected_results <- selected_results[order(selected_results$abs_coef, decreasing = TRUE), ]
  selected_results <- selected_results[1:min(top_n, nrow(selected_results)), ]
  
  for (i in 1:nrow(selected_results)) {
    cat(sprintf(
      "%2d. %-30s coef = %8.4f | odds ratio = %8.4f\n",
      i,
      selected_results$predictor[i],
      selected_results$coefficient[i],
      selected_results$odds_ratio[i]
    ))
  }
  
  invisible(selected_results)
}

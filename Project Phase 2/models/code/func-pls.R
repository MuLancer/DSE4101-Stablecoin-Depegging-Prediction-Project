# Alternative to PCR. Trying again with PLS (doesn't work on rolling windows)
# because PCR has a "glm.fit: fitted probabilities numerically 0 or 1 occurred" warning
# i.e. perfect separation warning
# PLS doesn't cause this warning (probably due to the correlation with y)

runpls <- function(train_data, test_data, title, max_comp = 10, cv_folds = 5) {
  
  library(pls)
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  # Convert y to numeric (0/1)
  y_train_n <- as.numeric(as.character(y_train))
  y_test_n  <- as.numeric(as.character(y_test))
  
  # Scale data (same as before)
  X_train_scaled <- scale(X_train)
  train_center <- attr(X_train_scaled, "scaled:center")
  train_scale  <- attr(X_train_scaled, "scaled:scale")
  X_test_scaled <- scale(X_test, center = train_center, scale = train_scale)
  
  # Convert to data.frame for pls
  train_df <- as.data.frame(X_train_scaled)
  train_df$y <- y_train_n
  
  test_df <- as.data.frame(X_test_scaled)
  
  # -------------------------
  # Cross-validation to choose ncomp
  # -------------------------
  
  pls_model <- plsr(y ~ ., data = train_df, 
                    ncomp = max_comp, 
                    validation = "CV", 
                    segments = cv_folds)
  
  # Extract CV RMSEP
  rmsep <- RMSEP(pls_model)
  cv_errors <- rmsep$val[1, , -1]  # remove intercept
  
  optimal_ncomp <- which.min(cv_errors)
  
  cat("Optimal number of components:", optimal_ncomp, "\n")
  cat("CV RMSEP:", round(cv_errors[optimal_ncomp], 4), "\n")
  
  # -------------------------
  # Final model
  # -------------------------
  
  pls_final <- plsr(y ~ ., data = train_df, ncomp = optimal_ncomp)
  
  # Predict probabilities
  pred_prob <- predict(pls_final, newdata = test_df, ncomp = optimal_ncomp)
  pred_prob <- as.vector(pred_prob)
  
  # Clip probabilities
  eps <- 1e-16
  pred_prob <- pmax(pmin(pred_prob, 1 - eps), eps)
  
  # Classification
  pred_class <- ifelse(pred_prob >= 0.5, "1", "0")
  pred_class <- factor(pred_class, levels = levels(y_test))
  
  # Confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm)) / sum(cm)
  
  # Log loss
  logloss <- -mean(y_test_n * log(pred_prob) + 
                     (1 - y_test_n) * log(1 - pred_prob))
  
  # -------------------------
  # Coefficients
  # -------------------------
  
  coeff_pls <- coef(pls_final, ncomp = optimal_ncomp)
  coeff_pls <- as.vector(coeff_pls)
  names(coeff_pls) <- colnames(X_train)
  
  # Explained variance (approx)
  expl_var <- explvar(pls_final)
  cum_var <- cumsum(expl_var)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Log Loss:", round(logloss, 4), "\n")
  print(cm)
  
  return(list(model = pls_final,
              pred_prob = pred_prob,
              pred_class = pred_class,
              ncomp = optimal_ncomp,
              cv_errors = cv_errors,
              coefficients = coeff_pls,
              explained_variance = expl_var,
              cumulative_variance = cum_var,
              confusion_matrix = cm,
              logloss = logloss))
}


runpls_all <- function(dfw, coin_list, horizons) {
  all_results <- list()
  
  for(coin in names(coin_list)) {
    cat("RUNNING PLS FOR:", coin)
    
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
      pls_results <- runpls(train_data = train_data,
                            test_data = test_data,
                            title = title)
      
      coin_results[[h]] <- pls_results
    }
    
    all_results[[coin]] <- coin_results
  }
  
  return(all_results)
}
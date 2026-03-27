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
  
  n_obs <- nrow(X_train_scaled)
  fold_size <- floor(n_obs / cv_folds)
  max_comp <- min(max_comp, ncol(X_train_scaled), n_obs - 1)
  cv_errors <- matrix(NA, nrow = cv_folds, ncol = min(max_comp, ncol(X_train_scaled), n_obs-1))
  
  for (fold in 1:cv_folds) {
    
    test_indices <- ((fold - 1) * fold_size + 1):min(fold * fold_size, n_obs)
    train_indices <- setdiff(1:n_obs, test_indices)
    
    train_fold <- train_df[train_indices, ]
    test_fold  <- train_df[test_indices, ]
    y_test_fold <- test_fold$y
    
    for (ncomp in 1:min(max_comp, ncol(train_fold))) {
      
      # Fit PLS
      pls_model <- plsr(y ~ ., data = train_fold, ncomp = ncomp)
      
      # Predict
      pred <- predict(pls_model, newdata = test_fold, ncomp = ncomp)
      pred <- as.vector(pred)
      
      # Clip probabilities
      eps <- 1e-16
      pred <- pmax(pmin(pred, 1 - eps), eps)
      # Log-loss
      cv_errors[fold, ncomp] <- -mean(y_test_fold * log(pred) + (1 - y_test_fold) * log(1 - pred))
    }
  }
  
  # Average CV errors across folds
  mean_cv_errors <- colMeans(cv_errors, na.rm = TRUE)
  optimal_ncomp <- which.min(mean_cv_errors)
  
  cat("Optimal number of components:", optimal_ncomp, "\n")
  cat("CV log loss:", round(mean_cv_errors[optimal_ncomp], 4), "\n")
  
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
  # Log loss
  logloss <- -mean(y_test_n * log(pred_prob) + 
                     (1 - y_test_n) * log(1 - pred_prob))
  
  # Classification
  pred_class <- ifelse(pred_prob >= 0.5, "1", "0")
  pred_class <- factor(pred_class, levels = levels(y_test))
  
  # Confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm)) / sum(cm)
  
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
              pls_loadings = pls_final$loading.weights,
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


# top_n refers to the no. of variables in each component 
show_pls_comps <- function(pls_result, top_n) {
  # Get PLS loadings
  loadings <- pls_result$pls_loadings
  
  cat("=== PLS Principal Components - Important Predictors ===\n\n")
  
  # Show top predictors for each component
  for(comp in 1:ncol(loadings)) {
    cat(sprintf("PRINCIPAL COMPONENT %d:\n", comp))
    
    # Get loadings for this component
    comp_loadings <- loadings[, comp]
    
    # Sort by absolute value (most important first)
    top_indices <- order(abs(comp_loadings), decreasing = TRUE)[1:top_n]
    top_loadings <- comp_loadings[top_indices]
    
    # Print results
    for(i in 1:length(top_loadings)) {
      cat(sprintf("  %2d. %-30s: %7.4f\n",
                  i,
                  names(top_loadings)[i],
                  top_loadings[i]))}
    cat("\n")
  }
}


# Plot CV error curve -- same code as in func-pcr.R
plot_cv_curve <- function(cv_errors, title) {
  ncomp_range <- 1:length(cv_errors)
  cv_df <- data.frame(ncomp = ncomp_range, CV_Error = cv_errors)
  
  optimal_ncomp <- which.min(cv_errors)
  
  ggplot(cv_df, aes(x = ncomp, y = CV_Error)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    geom_vline(xintercept = optimal_ncomp, linetype = "dashed", color = "red", size = 1) +
    geom_point(aes(x = optimal_ncomp, y = cv_errors[optimal_ncomp]), 
               color = "red", size = 3) +
    annotate("text", x = optimal_ncomp, y = max(cv_errors),
             label = paste("Optimal:", optimal_ncomp, "PCs"),
             hjust = -0.1, color = "red", fontface = "bold") +
    labs(title = title,
         x = "Number of Principal Components",
         y = "Cross-Validation Log-loss") +
    theme_minimal()
}
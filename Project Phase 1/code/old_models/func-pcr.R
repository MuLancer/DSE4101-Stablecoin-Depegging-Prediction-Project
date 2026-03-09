
runpcr <- function(Y, indice, lag, max_comp = 10, cv_folds = 5) {
  
  Y2 <- as.matrix(Y)
  aux <- embed(Y2, 1 + lag)  # create lags + forecast horizon
  
  y <- aux[, indice]
  X <- aux[, -c(1:(ncol(Y2) * lag)), drop = FALSE]
  
  if (lag == 1) {
    X.out <- tail(aux, 1)[1:ncol(X)] # retrieve the last observations if one-step forecast 
  } else {
    X.out <- aux[, -c(1:(ncol(Y2) * (lag - 1)))] # delete first (h-1) columns of aux,
    X.out <- tail(X.out, 1)[1:ncol(X)] # last observations: y_T,y_t-1...y_t-h
  }
  
  # Center data (so PCA works)
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  y_centered <- y - mean(y)
  
  # run PCA
  pca <- prcomp(X_centered, center = FALSE, scale. = FALSE)
  
  # CV to choose optimal number of components
  n_obs <- nrow(X_centered)
  fold_size <- floor(n_obs / cv_folds)
  cv_errors <- matrix(NA, nrow = cv_folds, ncol = min(max_comp, ncol(X_centered)))
  
  for (fold in 1:cv_folds) {
    
    test_indices <- ((fold - 1) * fold_size + 1):min(fold * fold_size, n_obs)
    train_indices <- setdiff(1:n_obs, test_indices)
    
    X_train <- X_centered[train_indices, , drop = FALSE]
    y_train <- y_centered[train_indices]
    X_test <- X_centered[test_indices, , drop = FALSE]
    y_test <- y_centered[test_indices]
    
    # run PCA on training data
    pca_train <- prcomp(X_train, center = FALSE, scale. = FALSE)
    
    # Test different numbers of components
    for (ncomp in 1:min(max_comp, ncol(X_train))) {
      scores_train <- pca_train$x[, 1:ncomp, drop = FALSE]
      scores_test <- X_test %*% pca_train$rotation[, 1:ncomp, drop = FALSE]
      
      # Fit PCR model
      pcr_model <- lm(y_train ~ scores_train - 1)
      
      # Predict on test set
      y_pred <- scores_test %*% coef(pcr_model)
      
      # Calculate MSE for this fold
      cv_errors[fold, ncomp] <- mean((y_test - y_pred)^2)
    }
  }
  
  # Average CV errors across folds
  mean_cv_errors <- colMeans(cv_errors, na.rm = TRUE)
  
  # Choose optimal number of components (minimum CV error)
  optimal_ncomp <- which.min(mean_cv_errors)
  
  # Train final model with optimal ncomp on all data
  scores_all <- pca$x[, 1:optimal_ncomp, drop = FALSE]
  pcr_model_final <- lm(y_centered ~ scores_all - 1) # consider changing to PCR
  
  #pcr_model <- pcr(y ~ X, data = train_data, ncomp = ncomp, validation = "none")
  #pred <- predict(pcr_model, newdata = new_data, ncomp = ncomp)
  
  # Extract coefficients
  beta_pcr <- pca$rotation[, 1:optimal_ncomp, drop = FALSE] %*% coef(pcr_model_final)
  
  # Prediction for new data
  X.out_centered <- X.out - colMeans(X)
  pred <- mean(y) + sum(X.out_centered * beta_pcr)
  
  # Explained variance by each PC
  explained_variance <- pca$sdev^2 / sum(pca$sdev^2)
  cumulative_variance <- cumsum(explained_variance)
  
  return(list("model" = list(beta = beta_pcr,
                             ncomp = optimal_ncomp,
                             pca = pca,
                             cv_errors = mean_cv_errors,
                             explained_variance = explained_variance,
                             cumulative_variance = cumulative_variance),
              "pred" = pred))
}


pcr.rolling.window <- function(Y, nprev, indice, lag = 1, max_comp = 10, cv_folds = 5) {
  
  save.pred <- matrix(NA, nprev, 1)
  save.ncomp <- matrix(NA, nprev, 1)
  save.cv_errors <- list()
  save.explained_var <- list()
  
  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    pcr_model <- runpcr(Y.window, indice, lag, max_comp, cv_folds)
    
    save.pred[(1 + nprev - i), ] <- pcr_model$pred
    save.ncomp[(1 + nprev - i), ] <- pcr_model$model$ncomp
    save.cv_errors[[i]] <- pcr_model$model$cv_errors
    save.explained_var[[i]] <- pcr_model$model$explained_variance
    
    cat("iteration", (1 + nprev - i), "\n")
  }
  
  # Calculate errors
  real <- Y[, indice]
  oos_real <- tail(real, nprev)
  
  rmse <- sqrt(mean((oos_real - save.pred)^2, na.rm = TRUE))
  mae <- mean(abs(oos_real - save.pred), na.rm = TRUE)
  errors <- c("rmse" = rmse, "mae" = mae)
  
  return(list("pred" = save.pred,"ncomp" = save.ncomp, "cv_errors" = save.cv_errors,
    "explained_var" = save.explained_var,"errors" = errors))
}


show_pcr_comps <- function(pcr_result, top_n = 10) {
  # Get PCA loadings
  pca_loadings <- pcr_result$pca_loadings
  
  cat("=== PCR Principal Components - Important Predictors ===\n\n")
  
  # Show top predictors for each component
  for(comp in 1:ncol(pca_loadings)) {
    cat(sprintf("PRINCIPAL COMPONENT %d:\n", comp))
    
    # Get loadings for this component
    loadings <- pca_loadings[, comp]
    
    # Sort by absolute value (most important first)
    top_indices <- order(abs(loadings), decreasing = TRUE)[1:top_n]
    top_loadings <- loadings[top_indices]
    
    # Print results
    for(i in 1:length(top_loadings)) {
      cat(sprintf("  %2d. %-30s: %7.4f\n", 
                  i, 
                  names(top_loadings)[i], 
                  top_loadings[i]))}
    cat("\n")
  }
}



runpcr <- function(Y, indice, lag, max_comp = 10, cv_folds = 5) {

  Y2 <- as.matrix(Y)
  n <- nrow(Y2)
  
  # --- Updated time series splitting logic ---
  # y_{t+lag}
  y_all <- Y2[(1 + lag):n, indice]
  
  # X_t (info available at time t)
  X_all <- Y2[1:(n - lag), , drop = FALSE]
  
  # remove the target column from predictors
  X_all <- X_all[, -indice, drop = FALSE]
  
  # train on all but last, predict last (for expanding window)
  X_train <- X_all[-nrow(X_all), , drop = FALSE]
  y_train <- y_all[-length(y_all)]
  X.out   <- X_all[nrow(X_all), , drop = FALSE]
  # --------------------------------------------

  # Center data (so PCA works)
  X_centered <- scale(X_train, center = TRUE, scale = FALSE)
  y_centered <- y_train - mean(y_train)

  # run PCA
  pca <- prcomp(X_centered, center = FALSE, scale. = FALSE)

  # CV to choose optimal number of components
  n_obs <- nrow(X_centered)
  fold_size <- floor(n_obs / cv_folds)
  cv_errors <- matrix(NA, nrow = cv_folds, ncol = min(max_comp, ncol(X_centered), n_obs-1))

  for (fold in 1:cv_folds) {

    test_indices <- ((fold - 1) * fold_size + 1):min(fold * fold_size, n_obs)
    train_indices <- setdiff(1:n_obs, test_indices)

    X_train_fold <- X_centered[train_indices, , drop = FALSE]
    y_train_fold <- y_centered[train_indices]
    X_test_fold <- X_centered[test_indices, , drop = FALSE]
    y_test_fold <- y_centered[test_indices]

    # run PCA on training data
    pca_train <- prcomp(X_train_fold, center = FALSE, scale. = FALSE)

    # Test different numbers of components
    for (ncomp in 1:min(max_comp, ncol(X_train_fold))) {
      scores_train <- pca_train$x[, 1:ncomp, drop = FALSE]
      scores_test <- X_test_fold %*% pca_train$rotation[, 1:ncomp, drop = FALSE]

      # Fit PCR model
      pcr_model <- lm(y_train_fold ~ scores_train - 1)

      # Predict on test set
      y_pred <- scores_test %*% coef(pcr_model)

      # Calculate MSE for this fold
      cv_errors[fold, ncomp] <- mean((y_test_fold - y_pred)^2)
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
  coeff_pcr <- pca$rotation[, 1:optimal_ncomp, drop = FALSE] %*% coef(pcr_model_final)

  # Prediction for new data
  X.out_centered <- matrix(X.out - colMeans(X_train), nrow = 1)
  pred <- mean(y_train) + as.numeric(X.out_centered %*% coeff_pcr)

  # Explained variance by each PC
  explained_variance <- pca$sdev^2 / sum(pca$sdev^2)
  cumulative_variance <- cumsum(explained_variance)
  
  # Store loadings with variable names
  pca_loadings <- pca$rotation[, 1:optimal_ncomp, drop = FALSE]
  rownames(pca_loadings) <- colnames(X_train)

  return(list("model" = pcr_model_final,
              "coefficients" = coeff_pcr,
              "ncomp" = optimal_ncomp,
              "pca" = pca,
              "cv_errors" = mean_cv_errors,
              "explained_variance" = explained_variance,
              "cumulative_variance" = cumulative_variance,
              "pca_loadings" = pca_loadings,
              "pred" = pred))
}


pcr.rolling.window <- function(Y, nprev, indice, lag = 1, max_comp = 10, cv_folds = 5) {

  save.pred <- matrix(NA, nprev, 1)
  save.ncomp <- matrix(NA, nprev, 1)
  save.cv_errors <- list()
  save.explained_var <- list()
  save.loadings <- list()

  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    pcr_model <- runpcr(Y.window, indice, lag, max_comp, cv_folds)

    save.pred[(1 + nprev - i), ] <- pcr_model$pred
    save.ncomp[(1 + nprev - i), ] <- pcr_model$ncomp
    save.cv_errors[[i]] <- pcr_model$cv_errors
    save.explained_var[[i]] <- pcr_model$explained_variance
    save.loadings[[i]] <- pcr_model$pca_loadings

    cat("iteration", (1 + nprev - i), "\n")
  }

  # Calculate errors
  real <- Y[, indice]
  oos_real <- tail(real, nprev)

  rmse <- sqrt(mean((oos_real - save.pred)^2, na.rm = TRUE))
  mae <- mean(abs(oos_real - save.pred), na.rm = TRUE)
  errors <- c("rmse" = rmse, "mae" = mae)

  return(list("pred" = save.pred,
              "ncomp" = save.ncomp, 
              "cv_errors" = save.cv_errors,
              "explained_var" = save.explained_var,
              "pca_loadings" = save.loadings,
              "errors" = errors))
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


# Plot CV error curve
plot_cv_curve <- function(cv_errors, title = "PCR Cross-Validation Error") {
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
         y = "Cross-Validation MSE") +
    theme_minimal()
}

  




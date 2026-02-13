runpcr <- function(Y, indice, lag, ncomp = 5) {
  
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
  
  # scale and center training data
  X_train_scaled <- scale(X_train, center = TRUE, scale = TRUE)
  y_train_centered <- y_train - mean(y_train)
  
  # store scaling and center attributes (to be used for X.out in prediction)
  X_center_attr <- attr(X_train_scaled, "scaled:center")
  X_scale_attr <- attr(X_train_scaled, "scaled:scale")
  
  max_possible_comp <- min(ncol(X_train_scaled), nrow(X_train_scaled) - 1)
  ncomp <- min(ncomp, max_possible_comp)
  if(ncomp < 1) ncomp <- 1
  
  # create training data for pcr function
  train_data <- data.frame(y = y_train_centered, X = I(X_train_scaled))
  
  # Use pcr() instead of manual PCA + regression
  pcr_model <- pcr(y ~ X, data = train_data, ncomp = ncomp, validation = "none")
  
  # scale and center prediction data using training params
  X.out_scaled <- matrix((X.out - X_center_attr) / X_scale_attr, nrow = 1)
  colnames(X.out_scaled) <- colnames(X_train)
  
  new_data <- data.frame(X = I(X.out_scaled))
  
  pred_scaled <- predict(pcr_model_final, newdata = new_data, ncomp = ncomp)
  pred <- mean(y_train) + as.numeric(pred_scaled) # re-center prediction
  
  # loadings and scores
  pca_loadings <- pcr_model$loadings[, 1:ncomp, drop = FALSE]
  scores <- pcr_model$scores[, 1:ncomp, drop = FALSE]
  
  # variance explained by each component
  X_total_var <- sum(apply(X_train_scaled, 2, var))
  pc_vars <- apply(scores, 2, var)
  explained_variance <- pc_vars / X_total_var
  cumulative_variance <- cumsum(explained_variance)
  
  # y variance explained
  y_pred_train <- predict(pcr_model_final, ncomp = ncomp)
  total_y_var <- sum(y_train_centered^2)
  y_explained <- 1 - sum((y_train_centered - y_pred_train)^2) / total_y_var
  
  # run PCA
  #pca <- prcomp(X_train_scaled, center = FALSE, scale. = FALSE)
  
  return(list("model" = pcr_model_final, 
              "ncomp" = ncomp,
              "pred" = pred, 
              "pca_loadings" = pca_loadings,
              "explained_variance" = explained_variance,
              "cumulative_variance" = cumulative_variance,
              "y_explained" = y_explained))
}


pcr.rolling.window <- function(Y, nprev, indice, lag = 1, ncomp = 5) {
  
  save.pred <- matrix(NA, nprev, 1)
  save.ncomp <- matrix(NA, nprev, 1)
  save.explained_var <- list()
  save.loadings <-list()
  
  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    pcr_model <- runpcr(Y.window, indice, lag, ncomp)
    
    save.pred[(1 + nprev - i), ] <- pcr_model$pred
    save.ncomp[(1 + nprev - i), ] <- pcr_model$ncomp
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
              "explained_var" = save.explained_var,
              "pca_loadings" = save.loadings,
              "errors" = errors))
}


show_pcr_comps <- function(pcr_result, top_n = 10) {
  # Get PCA loadings
  pca_loadings <- pcr_result$pca_loadings
  
  cat("=== PCR Principal Components - Important Predictors ===\n\n")
  
  # Show top predictors for each component
  for(comp in 1:ncol(loadings)) {
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
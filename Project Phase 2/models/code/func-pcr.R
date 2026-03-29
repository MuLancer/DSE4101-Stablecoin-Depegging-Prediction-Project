
runpcr <- function(train_data, test_data, title, max_comp = 10, cv_folds = 5) {
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  # need to convert y to numeric for regression (0/1)
  y_train_n <- as.numeric(as.character(y_train))
  y_test_n <- as.numeric(as.character(y_test))
  
  # need to scale and center for PCR
  X_train_scaled <- scale(X_train)
  train_center <- attr(X_train_scaled, "scaled:center")
  train_scale  <- attr(X_train_scaled, "scaled:scale")
  X_test_scaled <- scale(X_test, center = train_center, scale  = train_scale)
  
  # no need to center and scale again
  pca <- prcomp(X_train_scaled, center = FALSE, scale. = FALSE)
  
  # CV to choose optimal number of components
  n_obs <- nrow(X_train_scaled)
  fold_size <- floor(n_obs / cv_folds)
  cv_errors <- matrix(NA, nrow = cv_folds, ncol = min(max_comp, ncol(X_train_scaled), n_obs-1))
  
  for (fold in 1:cv_folds) {
    
    test_indices <- ((fold - 1) * fold_size + 1):min(fold * fold_size, n_obs)
    train_indices <- setdiff(1:n_obs, test_indices)
    
    X_train_fold <- X_train_scaled[train_indices, , drop = FALSE]
    y_train_fold <- y_train_n[train_indices]
    X_test_fold <- X_train_scaled[test_indices, , drop = FALSE]
    y_test_fold <- y_train_n[test_indices]
    
    # run PCA on training data
    pca_train <- prcomp(X_train_fold, center = FALSE, scale. = FALSE)
    
    # Test different numbers of components
    for (ncomp in 1:min(max_comp, ncol(X_train_fold))) {
      scores_train <- pca_train$x[, 1:ncomp, drop = FALSE]
      scores_test <- X_test_fold %*% pca_train$rotation[, 1:ncomp, drop = FALSE]
      
      scores_train_df <- as.data.frame(scores_train)
      scores_test_df  <- as.data.frame(scores_test)
      
      # Fit PCR model
      pcr_model <- glm(y_train_fold ~ ., data = scores_train_df, family = binomial)
      
      # Predict on test set
      y_pred_prob <- predict(pcr_model, newdata = scores_test_df, type = "response")
      
      # Calculate logloss for CV, use small error to avoid log(0)
      eps <- 1e-16
      y_pp <- pmax(pmin(y_pred_prob, 1 - eps), eps)
      cv_errors[fold, ncomp] <- -mean(y_test_fold * log(y_pp) + (1 - y_test_fold) * log(1 - y_pp))
    }
  }
  
  # Average CV errors across folds
  mean_cv_errors <- colMeans(cv_errors, na.rm = TRUE)
  
  # Choose optimal number of components (minimum CV error)
  optimal_ncomp <- which.min(mean_cv_errors)
  
  cat("Optimal number of components:", optimal_ncomp, "\n")
  cat("CV log loss:", round(mean_cv_errors[optimal_ncomp], 4), "\n")
  
  # Train final model with optimal ncomp on all data
  scores_all <- pca$x[, 1:optimal_ncomp, drop = FALSE]
  scores_test <- X_test_scaled %*% pca$rotation[, 1:optimal_ncomp, drop = FALSE]
  
  colnames(scores_all) <- paste0("PC", seq_len(ncol(scores_all)))
  colnames(scores_test) <- paste0("PC", seq_len(ncol(scores_test)))
  
  scores_all_df <- as.data.frame(scores_all)
  scores_test_df <- as.data.frame(scores_test)
  
  pcr_model_final <- glm(y_train_n ~ ., data = scores_all_df, family = binomial)
  pred_prob <- predict(pcr_model_final, newdata = scores_test_df, type = "response")
  
  pred_class <- ifelse(pred_prob >= 0.5, "1", "0")
  pred_class <- factor(pred_class, levels = levels(y_test))
  
  #pcr_model <- pcr(y ~ X, data = train_data, ncomp = ncomp, validation = "none")
  #pred <- predict(pcr_model, newdata = new_data, ncomp = ncomp)
  
  # confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm))/sum(cm)
  
  # calculate log loss on test set
  eps <- 1e-16
  pp <- pmax(pmin(pred_prob, 1 - eps), eps)
  logloss <- -mean(y_test_n * log(pp) + (1 - y_test_n) * log(1 - pp))
  
  # calculate coeff for original features
  # need to remove intercept [-1] to avoid error "non-conformable arguments"
  # note "rotation" are the PCA loadings 
  coeff_pcr <- pca$rotation[, 1:optimal_ncomp, drop = FALSE] %*% coef(pcr_model_final)[-1]
  coeff_org <- coeff_pcr / train_scale # convert back to original scale
  
  # Explained variance by each PC
  explained_variance <- pca$sdev^2 / sum(pca$sdev^2)
  cumulative_variance <- cumsum(explained_variance)
  
  # Store loadings with variable names
  pca_loadings <- pca$rotation[, 1:optimal_ncomp, drop = FALSE]
  rownames(pca_loadings) <- colnames(X_train)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Log Loss:", round(logloss, 4), "\n")
  print(cm)
  
  return(list(model = pcr_model_final,
              pred_prob = pred_prob,
              pred_class = pred_class,
              ncomp = optimal_ncomp,
              pca = pca,
              cv_errors = mean_cv_errors,
              explained_variance = explained_variance,
              cumulative_variance = cumulative_variance,
              pca_loadings = pca_loadings,
              train_pc_scores = scores_all_df,
              test_pc_scores = scores_test_df,
              coefficients = coeff_org,
              confusion_matrix = cm,
              logloss = logloss))
}


runpcr_all <- function(dfw, coin_list, horizons) {
  all_results <- list()
  
  for(coin in names(coin_list)) {
    cat("RUNNING PCR FOR:", coin)
    
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
      pcr_results <- runpcr(train_data = train_data,
                          test_data = test_data,
                          title = title)
      
      coin_results[[h]] <- pcr_results
    }
    
    all_results[[coin]] <- coin_results
  }
  
  return(all_results)
}


# top_n refers to the no. of variables in each component 
show_pcr_comps <- function(pcr_result, top_n) {
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


# Plot CV error curve -- same code as in func-pls.R
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


#Plot PCs against test set date
plot_test_pcs <- function(result_data, test_data, coin_df, coin, horizon, title = NULL,
                          show_labels = TRUE) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  pcr_result <- result_data[[coin]][[horizon]]
  scores <- as.data.frame(pcr_result$test_pc_scores)
  colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
  
  test_dates <- as.Date(test_data[[coin]][[horizon]]$test$dates)
  
  if (length(test_dates) != nrow(scores)) {
    stop("Length of test dates must match number of rows in test_pc_scores")
  }
  
  scores$time <- test_dates
  
  plot_df <- pivot_longer(
    scores,
    cols = -time,
    names_to = "PC",
    values_to = "score"
  )
  
  test_start <- min(test_dates)
  test_end <- max(test_dates)
  
  depeg_df <- coin_df %>%
    mutate(
      date = as.Date(date),
      depeg_flag = as.integer(as.character(.data[[horizon]]))
    ) %>%
    filter(date >= test_start, date <= test_end) %>%
    arrange(date) %>%
    mutate(
      prev_flag = dplyr::lag(depeg_flag, default = 0),
      event_start = depeg_flag == 1 & prev_flag == 0
    ) %>%
    filter(event_start) %>%
    select(date)
  
  p <- ggplot() +
    geom_line(
      data = plot_df,
      aes(x = time, y = score),
      color = "steelblue",
      linewidth = 0.9
    ) +
    facet_wrap(~ PC, scales = "free_y") +
    geom_vline(
      data = depeg_df,
      aes(xintercept = date),
      color = "red",
      linetype = "solid",
      linewidth = 1,
      alpha = 0.9
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal() +
    labs(
      title = if (is.null(title)) paste(coin, "-", horizon, ": test PC scores") else title,
      x = "Time",
      y = "PC score"
    ) +
    theme(
      plot.margin = margin(20, 30, 10, 10)
    )
  
  if (show_labels && nrow(depeg_df) > 0) {
    p <- p +
      geom_text(
        data = depeg_df,
        aes(x = date, y = Inf, label = format(date, "%Y-%m-%d")),
        color = "red",
        angle = 90,
        vjust = -0.8,
        hjust = 1,
        size = 3.2,
        fontface = "bold"
      ) +
      coord_cartesian(clip = "off")
  }
  
  print(p)
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


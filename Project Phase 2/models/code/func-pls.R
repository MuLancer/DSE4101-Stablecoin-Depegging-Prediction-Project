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
  
  # Save train/test PLS component scores
  train_pls_scores <- pls_final$scores[, 1:optimal_ncomp, drop = FALSE]
  test_pls_scores <- X_test_scaled %*% pls_final$projection[, 1:optimal_ncomp, drop = FALSE]
  
  colnames(train_pls_scores) <- paste0("Comp", seq_len(ncol(train_pls_scores)))
  colnames(test_pls_scores) <- paste0("Comp", seq_len(ncol(test_pls_scores)))
  
  train_pls_scores <- as.data.frame(train_pls_scores)
  test_pls_scores <- as.data.frame(test_pls_scores)
  
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
              cv_errors_plot = mean_cv_errors,
              pls_loadings = pls_final$loading.weights,
              train_pls_scores = train_pls_scores,
              test_pls_scores = test_pls_scores,
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


#Plot Comps against test set dates
plot_test_comps <- function(result_data, test_data, coin_df, coin, horizon, title = NULL,
                            show_labels = TRUE, depeg_col = "depeg_1d") {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  pls_result <- result_data[[coin]][[horizon]]
  scores <- as.data.frame(pls_result$test_pls_scores)
  colnames(scores) <- paste0("Comp", seq_len(ncol(scores)))
  
  test_dates <- as.Date(test_data[[coin]][[horizon]]$test$dates)
  
  if (length(test_dates) != nrow(scores)) {
    stop("Length of test dates must match number of rows in test_pls_scores")
  }
  
  scores$time <- test_dates
  
  plot_df <- pivot_longer(
    scores,
    cols = -time,
    names_to = "Component",
    values_to = "score"
  )
  
  test_start <- min(test_dates)
  test_end <- max(test_dates)
  
  depeg_regions <- coin_df %>%
    mutate(
      date = as.Date(date),
      depeg_flag = as.integer(as.character(.data[[depeg_col]]))
    ) %>%
    filter(date >= test_start, date <= test_end) %>%
    arrange(date) %>%
    mutate(
      break_group = cumsum(depeg_flag != lag(depeg_flag, default = first(depeg_flag)))
    ) %>%
    filter(depeg_flag == 1) %>%
    group_by(break_group) %>%
    summarise(
      start_date = min(date),
      end_date = max(date),
      .groups = "drop"
    ) %>%
    mutate(
      label_date = start_date,
      xmin = start_date,
      xmax = end_date + 1
    )
  
  p <- ggplot() +
    geom_rect(
      data = depeg_regions,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = "red",
      alpha = 0.12
    ) +
    geom_vline(
      data = depeg_regions,
      aes(xintercept = start_date),
      color = "red4",
      linewidth = 1.2,
      alpha = 0.95
    ) +
    geom_line(
      data = plot_df,
      aes(x = time, y = score),
      color = "steelblue",
      linewidth = 0.9
    ) +
    facet_wrap(~ Component, scales = "free_y") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal() +
    labs(
      title = if (is.null(title)) paste(coin, "-", horizon, ": test PLS scores") else title,
      x = "Time",
      y = "PLS score"
    ) +
    theme(
      plot.margin = margin(20, 30, 10, 10)
    )
  
  if (show_labels && nrow(depeg_regions) > 0) {
    p <- p +
      geom_text(
        data = depeg_regions,
        aes(x = label_date, y = Inf, label = format(start_date, "%Y-%m-%d")),
        color = "red4",
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

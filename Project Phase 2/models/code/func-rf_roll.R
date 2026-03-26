library(randomForest)
library(dplyr)

runrf <- function(train_data, test_data, title = "RF model") {
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  y_train <- factor(as.character(y_train), levels = c("0", "1"))
  y_test  <- factor(as.character(y_test),  levels = c("0", "1"))
  
  keep_train <- !is.na(y_train)
  X_train <- X_train[keep_train, , drop = FALSE]
  y_train <- y_train[keep_train]
  
  keep_test <- !is.na(y_test)
  X_test <- X_test[keep_test, , drop = FALSE]
  y_test <- y_test[keep_test]
  
  if (nrow(X_test) == 0) {
    return(list(
      model = NULL,
      pred_prob = numeric(0),
      pred_class = factor(character(0), levels = c("0", "1")),
      importance = NULL,
      confusion_matrix = matrix(0, nrow = 2, ncol = 2,
                                dimnames = list(Predicted = c("0", "1"),
                                                Actual = c("0", "1"))),
      accuracy = NA_real_
    ))
  }
  
  if (length(unique(y_train)) < 2) {
    only_class <- as.character(unique(y_train))[1]
    
    pred_class <- factor(rep(only_class, nrow(X_test)), levels = c("0", "1"))
    pred_prob  <- if (only_class == "1") rep(1, nrow(X_test)) else rep(0, nrow(X_test))
    
    cm <- table(
      Predicted = factor(pred_class, levels = c("0", "1")),
      Actual    = factor(y_test, levels = c("0", "1"))
    )
    accuracy <- sum(diag(cm)) / sum(cm)
    
    cat("\n", title, "\n")
    cat("Only one class in training data. Using constant prediction:", only_class, "\n")
    cat("Accuracy:", round(accuracy, 4), "\n")
    print(cm)
    
    return(list(
      model = NULL,
      pred_prob = pred_prob,
      pred_class = pred_class,
      importance = NULL,
      confusion_matrix = cm,
      accuracy = accuracy
    ))
  }
  
  model <- randomForest(
    x = X_train,
    y = y_train,
    importance = TRUE,
    ntree = 500,
    mtry = max(1, floor(sqrt(ncol(X_train)))),
    nodesize = 5,
    keep.forest = TRUE
  )
  
  pred_class <- predict(model, newdata = X_test)
  pred_class <- factor(as.character(pred_class), levels = c("0", "1"))
  
  pred_prob_mat <- predict(model, newdata = X_test, type = "prob")
  if ("1" %in% colnames(pred_prob_mat)) {
    pred_prob <- as.numeric(pred_prob_mat[, "1"])
  } else {
    pred_prob <- rep(0, nrow(X_test))
  }
  
  cm <- table(
    Predicted = factor(pred_class, levels = c("0", "1")),
    Actual    = factor(y_test, levels = c("0", "1"))
  )
  accuracy <- sum(diag(cm)) / sum(cm)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  print(cm)
  
  return(list(
    model = model,
    pred_prob = pred_prob,
    pred_class = pred_class,
    importance = importance(model),
    confusion_matrix = cm,
    accuracy = accuracy
  ))
}

rf_rolling_window <- function(data,
                              target_col,
                              remove_col,
                              train_window,
                              test_window = 1,
                              step = 1,
                              smote_train = TRUE,
                              expanding = FALSE,
                              verbose = TRUE,
                              prep_verbose = FALSE) {
  
  data <- data %>%
    arrange(date) %>%
    filter(!is.na(.data[[target_col]]))
  
  n <- nrow(data)
  
  if (train_window < 1) stop("train_window must be >= 1")
  if (test_window < 1) stop("test_window must be >= 1")
  if (step < 1) stop("step must be >= 1")
  if (n <= train_window) stop("Not enough rows: n must be > train_window")
  
  all_results <- list()
  save_importance <- list()
  iter <- 1
  
  test_starts <- seq(from = train_window + 1,
                     to   = n - test_window + 1,
                     by   = step)
  
  for (test_start in test_starts) {
    test_end <- test_start + test_window - 1
    
    if (expanding) {
      train_start <- 1
    } else {
      train_start <- test_start - train_window
    }
    
    train_end <- test_start - 1
    
    train_raw <- data[train_start:train_end, , drop = FALSE]
    test_raw  <- data[test_start:test_end, , drop = FALSE]
    
    train_prep <- prep_features(
      data = train_raw,
      target_col = target_col,
      remove_col = remove_col,
      smote = smote_train,
      verbose = prep_verbose
    )
    
    test_prep <- prep_features(
      data = test_raw,
      target_col = target_col,
      remove_col = remove_col,
      smote = FALSE,
      verbose = prep_verbose
    )
    
    if (nrow(test_prep$X) == 0) next
    
    rf_fit <- runrf(
      train_data = train_prep,
      test_data  = test_prep,
      title = paste0(
        target_col, " | train: ",
        min(train_raw$date), " to ", max(train_raw$date),
        " | test: ",
        min(test_raw$date), " to ", max(test_raw$date)
      )
    )
    
    result_dates <- if ("dates" %in% names(test_prep)) test_prep$dates else test_raw$date
    
    res_iter <- data.frame(
      date = result_dates,
      actual = as.character(test_prep$y),
      pred_class = as.character(rf_fit$pred_class),
      pred_prob = as.numeric(rf_fit$pred_prob),
      stringsAsFactors = FALSE
    )
    
    all_results[[iter]] <- res_iter
    save_importance[[iter]] <- rf_fit$importance
    
    if (verbose) {
      cat(
        "iteration", iter,
        "| train rows:", train_start, "-", train_end,
        "| test rows:", test_start, "-", test_end, "\n"
      )
    }
    
    iter <- iter + 1
  }
  
  if (length(all_results) == 0) {
    stop("No rolling-window results were produced. Check prep_features() and missing values.")
  }
  
  results <- bind_rows(all_results)
  
  results$actual <- factor(results$actual, levels = c("0", "1"))
  results$pred_class <- factor(results$pred_class, levels = c("0", "1"))
  
  cm <- table(
    Predicted = factor(results$pred_class, levels = c("0", "1")),
    Actual    = factor(results$actual, levels = c("0", "1"))
  )
  
  accuracy <- mean(results$pred_class == results$actual, na.rm = TRUE)
  
  return(list(
    results = results,
    confusion_matrix = cm,
    accuracy = accuracy,
    save.importance = save_importance
  ))
}
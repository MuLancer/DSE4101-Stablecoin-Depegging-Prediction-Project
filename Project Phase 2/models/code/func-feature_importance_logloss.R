############################################################
### LOG-LOSS PERMUTATION FEATURE IMPORTANCE HELPERS #######
############################################################

# Binary log loss
log_loss_bin <- function(y_true, p_hat, eps = 1e-15) {
  if (is.factor(y_true)) {
    y_num <- as.numeric(as.character(y_true))
  } else {
    y_num <- as.numeric(y_true)
  }
  
  p_hat <- pmin(pmax(p_hat, eps), 1 - eps)
  -mean(y_num * log(p_hat) + (1 - y_num) * log(1 - p_hat))
}

# Safely extract fitted model object
get_model_obj <- function(model_entry) {
  if (!is.null(model_entry$model)) return(model_entry$model)
  if (!is.null(model_entry$fit)) return(model_entry$fit)
  stop("Could not find fitted model object. Expected $model or $fit.")
}

# Robust probability prediction for binary class = 1
get_prob_pred <- function(model_obj, X_new) {
  try_xgb <- try({
    p <- predict(model_obj, as.matrix(X_new))
    if (is.numeric(p) && length(p) == nrow(X_new)) return(as.numeric(p))
    NULL
  }, silent = TRUE)
  if (!inherits(try_xgb, "try-error") && !is.null(try_xgb)) return(try_xgb)
  
  try_prob <- try({
    p <- predict(model_obj, newdata = X_new, type = "prob")
    
    if (is.matrix(p) || is.data.frame(p)) {
      cn <- colnames(p)
      if (!is.null(cn)) {
        if ("1" %in% cn) return(as.numeric(p[, "1"]))
        if ("yes" %in% tolower(cn)) return(as.numeric(p[, which(tolower(cn) == "yes")[1]]))
        if ("true" %in% tolower(cn)) return(as.numeric(p[, which(tolower(cn) == "true")[1]]))
      }
      if (ncol(p) == 2) return(as.numeric(p[, 2]))
    }
    
    if (is.numeric(p) && length(p) == nrow(X_new)) return(as.numeric(p))
    NULL
  }, silent = TRUE)
  if (!inherits(try_prob, "try-error") && !is.null(try_prob)) return(try_prob)
  
  try_resp <- try({
    p <- predict(model_obj, newdata = X_new, type = "response")
    if (is.numeric(p) && length(p) == nrow(X_new)) return(as.numeric(p))
    NULL
  }, silent = TRUE)
  if (!inherits(try_resp, "try-error") && !is.null(try_resp)) return(try_resp)
  
  stop("Could not get predicted probabilities from model.")
}

perm_importance_logloss_one <- function(model_entry, test_X, test_y,
                                        n_repeats = 10, seed = 123) {
  set.seed(seed)
  
  model_obj <- get_model_obj(model_entry)
  p_base <- get_prob_pred(model_obj, test_X)
  base_ll <- log_loss_bin(test_y, p_base)
  
  feats <- colnames(test_X)
  
  imp_tbl <- lapply(feats, function(feat) {
    perm_ll <- numeric(n_repeats)
    
    for (r in seq_len(n_repeats)) {
      X_perm <- test_X
      X_perm[[feat]] <- sample(X_perm[[feat]], replace = FALSE)
      p_perm <- get_prob_pred(model_obj, X_perm)
      perm_ll[r] <- log_loss_bin(test_y, p_perm)
    }
    
    data.frame(
      feature = feat,
      baseline_logloss = base_ll,
      perm_logloss_mean = mean(perm_ll),
      perm_logloss_sd = sd(perm_ll),
      importance_logloss = mean(perm_ll) - base_ll
    )
  }) %>% dplyr::bind_rows()
  
  imp_tbl %>% dplyr::arrange(dplyr::desc(importance_logloss))
}

run_logloss_importance_all <- function(model_results, dfw, coin_list, horizons,
                                       n_repeats = 10, seed = 123,
                                       model_name = "RF", window_name = "Window 1") {
  out <- list()
  k <- 1
  
  for (coin in coin_list) {
    for (h in horizons) {
      cat("Running:", model_name, "|", window_name, "|", coin, "|", h, "\n")
      
      test_X <- dfw[[coin]][[h]]$test$X
      test_y <- dfw[[coin]][[h]]$test$y
      model_entry <- model_results[[coin]][[h]]
      
      imp_tbl <- perm_importance_logloss_one(
        model_entry = model_entry,
        test_X = test_X,
        test_y = test_y,
        n_repeats = n_repeats,
        seed = seed
      )
      
      imp_tbl$model <- model_name
      imp_tbl$window <- window_name
      imp_tbl$coin <- coin
      imp_tbl$horizon <- h
      
      out[[k]] <- imp_tbl
      k <- k + 1
    }
  }
  
  dplyr::bind_rows(out) %>%
    dplyr::select(
      model, window, coin, horizon, feature,
      baseline_logloss, perm_logloss_mean, perm_logloss_sd, importance_logloss
    )
}

summarise_importance <- function(imp_all) {
  imp_all %>%
    dplyr::group_by(model, window, coin, horizon, feature) %>%
    dplyr::summarise(
      mean_importance = mean(importance_logloss, na.rm = TRUE),
      sd_importance = sd(importance_logloss, na.rm = TRUE),
      .groups = "drop"
    )
}

get_top_features_each_group <- function(imp_summary, top_n = 10) {
  imp_summary %>%
    dplyr::group_by(model, window, coin, horizon) %>%
    dplyr::slice_max(order_by = mean_importance, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup()
}

plot_coin_feature_importance <- function(imp_data, 
                                         coin_name,
                                         model_name = "RF", 
                                         window_name = "Window 1",
                                         top_n = 10) {
  
  plot_df <- imp_data %>%
    dplyr::filter(model == model_name, window == window_name, coin == coin_name) %>%
    dplyr::group_by(horizon, feature) %>%
    dplyr::summarise(
      mean_importance = mean(importance_logloss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(horizon) %>%
    dplyr::slice_max(order_by = mean_importance, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      horizon_label = dplyr::case_when(
        horizon == "depeg_1d" ~ "1 Day",
        horizon == "depeg_3d" ~ "3 Day",
        horizon == "depeg_5d" ~ "5 Day",
        horizon == "depeg_7d" ~ "7 Day",
        TRUE ~ horizon
      ),
      horizon_label = factor(
        horizon_label,
        levels = c("1 Day", "3 Day", "5 Day", "7 Day")
      )
    )
  
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = tidytext::reorder_within(feature, mean_importance, horizon),
      y = mean_importance
    )
  ) +
    ggplot2::geom_col(fill = "#2E86AB") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.4f", mean_importance)),
      hjust = -0.1,
      size = 3
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ horizon_label, nrow = 2, scales = "free_y") +
    tidytext::scale_x_reordered() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = paste(coin_name, "Top", top_n, "Feature Importance by Horizon"),
      subtitle = paste(model_name, "|", window_name),
      x = NULL,
      y = "Mean Log Loss Increase"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      panel.spacing = grid::unit(1, "lines"),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )
}
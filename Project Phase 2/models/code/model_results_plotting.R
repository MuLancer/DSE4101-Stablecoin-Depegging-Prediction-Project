library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================
# Plot output folder
# ============================================
plot_dir <- "../../plots/model performance"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

summary_main <- read_csv("../../model performance output/summary_main.csv")
# ============================================
# Prepare plotting data once
# ============================================
summary_plot <- summary_main %>%
  mutate(
    horizon = factor(horizon, levels = c(1, 3, 5, 7), labels = c("1d", "3d", "5d", "7d")),
    window  = factor(window, levels = c("Window 1", "Window 2")),
    model   = factor(model, levels = c("RF", "GB", "PCR", "PLS")),
    coin    = factor(coin, levels = c("DAI", "PAX", "USDC", "USDT", "UST"))
  )

summary_plot_f1 <- summary_plot %>%
  filter(!is.na(f1_score), !is.nan(f1_score))

summary_plot_accuracy <- summary_plot %>%
  filter(!is.na(accuracy), !is.nan(accuracy))

summary_plot_recall <- summary_plot %>%
  filter(!is.na(recall), !is.nan(recall))

summary_plot_precision <- summary_plot %>%
  filter(!is.na(precision), !is.nan(precision))

summary_plot_pr <- summary_plot %>%
  filter(
    !is.na(precision), !is.nan(precision),
    !is.na(recall), !is.nan(recall)
  )

# ============================================
# Shared theme
# ============================================
theme_model_perf <- function() {
  theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# ============================================
# Helper functions
# ============================================
make_heatmap <- function(data, metric, title, fill_label) {
  ggplot(data, aes(x = horizon, y = coin, fill = .data[[metric]])) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = round(.data[[metric]], 2)), size = 3) +
    facet_grid(window ~ model) +
    scale_fill_viridis_c(labels = number_format(accuracy = 0.01), na.value = "grey90") +
    labs(
      title = title,
      x = "Forecast Horizon",
      y = "Coin",
      fill = fill_label
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

make_line_plot <- function(data, metric, title, y_label) {
  ggplot(data, aes(x = horizon, y = .data[[metric]], color = model, group = model)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    facet_grid(window ~ coin, scales = "fixed", drop = TRUE) +
    scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
    labs(
      title = title,
      x = "Forecast Horizon",
      y = y_label,
      color = "Model"
    ) +
    theme_model_perf()
}

save_plot <- function(plot_obj, filename, width = 12, height = 8) {
  ggsave(
    filename = file.path(plot_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 300
  )
}

# ============================================
# Heatmaps
# ============================================
p_f1_heatmap <- make_heatmap(
  summary_plot_f1,
  metric = "f1_score",
  title = "F1 Score by Model, Window, Coin, and Horizon",
  fill_label = "F1 Score"
)

p_accuracy_heatmap <- make_heatmap(
  summary_plot_accuracy,
  metric = "accuracy",
  title = "Accuracy by Model, Window, Coin, and Horizon",
  fill_label = "Accuracy"
)

p_recall_heatmap <- make_heatmap(
  summary_plot_recall,
  metric = "recall",
  title = "Recall by Model, Window, Coin, and Horizon",
  fill_label = "Recall"
)

# Optional precision heatmap
p_precision_heatmap <- make_heatmap(
  summary_plot_precision,
  metric = "precision",
  title = "Precision by Model, Window, Coin, and Horizon",
  fill_label = "Precision"
)

# ============================================
# Line plots
# ============================================
p_f1_lines <- make_line_plot(
  summary_plot_f1,
  metric = "f1_score",
  title = "F1 Score Across Forecast Horizons",
  y_label = "F1 Score"
)

p_accuracy_lines <- make_line_plot(
  summary_plot_accuracy,
  metric = "accuracy",
  title = "Accuracy Across Forecast Horizons",
  y_label = "Accuracy"
)

p_recall_lines <- make_line_plot(
  summary_plot_recall,
  metric = "recall",
  title = "Recall Across Forecast Horizons",
  y_label = "Recall"
)

# ============================================
# Precision vs Recall scatter
# ============================================
p_pr_scatter <- ggplot(
  summary_plot_pr,
  aes(x = recall, y = precision, color = model, shape = window)
) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~ coin) +
  scale_x_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Precision vs Recall by Model",
    x = "Recall",
    y = "Precision",
    color = "Model",
    shape = "Window"
  ) +
  theme_model_perf()

# ============================================
# Confusion matrix components
# ============================================
conf_long <- summary_plot %>%
  select(model, window, coin, horizon, TP, TN, FP, FN) %>%
  pivot_longer(cols = c(TP, TN, FP, FN), names_to = "type", values_to = "count")

p_confusion <- ggplot(conf_long, aes(x = horizon, y = count, fill = type)) +
  geom_col(position = "stack") +
  facet_grid(window + coin ~ model, scales = "free_y") +
  labs(
    title = "Confusion Matrix Components by Model",
    x = "Forecast Horizon",
    y = "Count",
    fill = "Component"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold")
  )

# ============================================
# Best model by F1
# ============================================
best_models_f1 <- summary_plot_f1 %>%
  group_by(window, coin, horizon) %>%
  slice_max(order_by = f1_score, n = 1, with_ties = FALSE) %>%
  ungroup()

p_best_models_f1 <- ggplot(best_models_f1, aes(x = horizon, y = f1_score, fill = model)) +
  geom_col() +
  facet_grid(window ~ coin) +
  scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Best Model by F1 Score",
    x = "Forecast Horizon",
    y = "Best F1 Score",
    fill = "Winning Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold")
  )

# ============================================
# Best model by Accuracy
# ============================================
best_models_accuracy <- summary_plot_accuracy %>%
  group_by(window, coin, horizon) %>%
  slice_max(order_by = accuracy, n = 1, with_ties = FALSE) %>%
  ungroup()

p_best_models_accuracy <- ggplot(best_models_accuracy, aes(x = horizon, y = accuracy, fill = model)) +
  geom_col() +
  facet_grid(window ~ coin) +
  scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Best Model by Accuracy",
    x = "Forecast Horizon",
    y = "Best Accuracy",
    fill = "Winning Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold")
  )

# ============================================
# Save all plots
# ============================================
save_plot(p_f1_heatmap,           "f1_heatmap.png",              width = 12, height = 8)
save_plot(p_accuracy_heatmap,     "accuracy_heatmap.png",        width = 12, height = 8)
save_plot(p_recall_heatmap,       "recall_heatmap.png",          width = 12, height = 8)
save_plot(p_precision_heatmap,    "precision_heatmap.png",       width = 12, height = 8)

save_plot(p_f1_lines,             "f1_lines.png",                width = 12, height = 8)
save_plot(p_accuracy_lines,       "accuracy_lines.png",          width = 12, height = 8)
save_plot(p_recall_lines,         "recall_lines.png",            width = 12, height = 8)

save_plot(p_pr_scatter,           "precision_recall_scatter.png", width = 10, height = 7)
save_plot(p_confusion,            "confusion_components.png",    width = 14, height = 10)
save_plot(p_best_models_f1,       "best_model_f1.png",           width = 12, height = 8)
save_plot(p_best_models_accuracy, "best_model_accuracy.png",     width = 12, height = 8)


p_f1_heatmap
p_accuracy_heatmap
p_recall_heatmap
p_precision_heatmap
p_f1_lines
p_accuracy_lines
p_recall_lines
p_pr_scatter
p_confusion
p_best_models_f1
p_best_models_accuracy
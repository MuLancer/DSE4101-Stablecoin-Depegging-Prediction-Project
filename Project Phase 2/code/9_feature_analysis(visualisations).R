# =============================================================================
# VISUALISATIONS — Stablecoin Depeg Analysis
# Run from: Project Phase 2 working directory
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)

PLOT_DIR <- "plots/9_feature_analysis.R plots/"
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------
df_dai  <- read.csv("data/DAI/DAI_onchain_features.csv")  %>% mutate(date = as.Date(date), coin = "DAI")
df_pax  <- read.csv("data/PAX/PAX_onchain_features.csv")  %>% mutate(date = as.Date(date), coin = "PAX")
df_usdc <- read.csv("data/USDC/USDC_onchain_features.csv") %>% mutate(date = as.Date(date), coin = "USDC")
df_usdt <- read.csv("data/USDT/USDT_onchain_features.csv") %>% mutate(date = as.Date(date), coin = "USDT")
df_ust  <- read.csv("data/UST/UST_onchain_features.csv")  %>% mutate(date = as.Date(date), coin = "UST")

coin_list  <- list(DAI = df_dai, PAX = df_pax, USDC = df_usdc, USDT = df_usdt, UST = df_ust)
depeg_date <- as.Date("2022-05-09")

# NOTE: PAX/USDC/USDT net_swap_flow is in raw token units (not USD).
# DAI and UST are in USD. We handle this by using raw values with no /1e6 scaling
# for PAX/USDC/USDT, and USD millions for DAI/UST.
usd_flow_coins <- c("DAI", "UST")

# Helper: add depeg vline only for UST
depeg_vline <- function(coin_name) {
  if (coin_name == "UST")
    geom_vline(xintercept = depeg_date, linetype = "dashed", color = "red", linewidth = 0.7)
  else NULL
}


# =============================================================================
# PLOT 1: Price + Peg Deviation (dual panel)
# Top:    Close price with depeg events shaded
# Bottom: abs_peg_deviation — shows *how far* it deviated, not just when
# =============================================================================
for (coin_name in names(coin_list)) {
  df <- coin_list[[coin_name]]
  
  p_price <- ggplot(df, aes(x = date)) +
    geom_rect(
      data = df %>% filter(depeg_1d == 1),
      aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
      fill = "tomato", alpha = 0.25, inherit.aes = FALSE
    ) +
    geom_line(aes(y = close), color = "steelblue", linewidth = 0.6, na.rm = TRUE) +
    geom_hline(yintercept = 1.0, linetype = "dotted", color = "grey40") +
    depeg_vline(coin_name) +
    labs(title = paste(coin_name, "— Price & Peg Deviation"),
         subtitle = "Red shading = depeg_1d event | Dotted = $1.00 peg",
         x = "", y = "Close Price (USD)") +
    theme_bw()
  
  p_dev <- ggplot(df %>% filter(!is.na(abs_peg_deviation)), aes(x = date)) +
    geom_area(aes(y = abs_peg_deviation), fill = "tomato", alpha = 0.5) +
    geom_line(aes(y = abs_peg_deviation), color = "darkred", linewidth = 0.4) +
    depeg_vline(coin_name) +
    labs(x = "", y = "Abs. Peg Deviation") +
    theme_bw()
  
  p_combined <- p_price / p_dev + plot_layout(heights = c(2, 1))
  ggsave(paste0(PLOT_DIR, tolower(coin_name), "_01_price_deviation.png"),
         p_combined, width = 12, height = 6, dpi = 300)
  print(p_combined)
}


# =============================================================================
# PLOT 2: Volatility + RVOL (merged 2-panel)
# Top:    Parkinson daily (thin, transparent) vs 7d rolling (bold)
# Bottom: RVOL bar chart — coloured by above/below average
# FIX:    Daily line now thin + alpha so 7d trend is visible
# FIX:    DAI y-axis capped at 0.05 to avoid outlier distortion (noted in subtitle)
# =============================================================================
for (coin_name in names(coin_list)) {
  df <- coin_list[[coin_name]]
  
  # Cap y-axis for DAI due to single extreme outlier on 2021-11-16 (0.786)
  y_cap    <- if (coin_name == "DAI") 0.05 else NULL
  subtitle <- if (coin_name == "DAI")
    "Parkinson estimator: intraday high-low range | y-axis capped at 0.05 (outlier: 0.79 on 2021-11-16)"
  else
    "Parkinson estimator: intraday high-low range"
  
  p_vol <- df %>%
    select(date, parkinson_daily, parkinson_7d) %>%
    pivot_longer(-date, names_to = "metric", values_to = "value") %>%
    ggplot(aes(x = date, y = value, color = metric, alpha = metric, linewidth = metric)) +
    geom_line(na.rm = TRUE) +
    depeg_vline(coin_name) +
    scale_color_manual(
      values = c(parkinson_daily = "steelblue", parkinson_7d = "darkorange"),
      labels = c(parkinson_daily = "Daily", parkinson_7d = "7-Day Rolling")
    ) +
    scale_alpha_manual(values = c(parkinson_daily = 0.3, parkinson_7d = 1.0), guide = "none") +
    scale_linewidth_manual(values = c(parkinson_daily = 0.4, parkinson_7d = 1.0), guide = "none") +
    { if (!is.null(y_cap)) coord_cartesian(ylim = c(0, y_cap)) else NULL } +
    labs(title = paste(coin_name, "— Volatility & Relative Volume"),
         subtitle = subtitle,
         x = "", y = "Volatility", color = "") +
    theme_bw() + theme(legend.position = "top")
  
  p_rvol <- ggplot(df %>% filter(!is.na(rvol)), aes(x = date, y = rvol, fill = rvol > 1)) +
    geom_col(alpha = 0.7, na.rm = TRUE) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    depeg_vline(coin_name) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70"),
                      labels = c("TRUE" = "Above avg", "FALSE" = "Below avg")) +
    labs(x = "", y = "RVOL", fill = "") +
    theme_bw() + theme(legend.position = "top")
  
  p_combined <- p_vol / p_rvol + plot_layout(heights = c(2, 1))
  ggsave(paste0(PLOT_DIR, tolower(coin_name), "_02_volatility_rvol.png"),
         p_combined, width = 12, height = 7, dpi = 300)
  print(p_combined)
}


# =============================================================================
# PLOT 3: On-Chain Metrics — 2x2 panel + net flow separately
# FIX:    net_swap_flow y-axis label reflects correct units per coin
#         PAX/USDC/USDT = raw token units | DAI/UST = USD millions
# =============================================================================
for (coin_name in names(coin_list)) {
  df <- coin_list[[coin_name]]
  
  make_onchain_panel <- function(df, col, avg_col, title, subtitle, color) {
    ggplot(df %>% filter(!is.na(.data[[col]])), aes(x = date)) +
      geom_line(aes(y = .data[[col]]),     color = "grey70", linewidth = 0.4, na.rm = TRUE) +
      geom_line(aes(y = .data[[avg_col]]), color = color,    linewidth = 0.9, na.rm = TRUE) +
      depeg_vline(coin_name) +
      labs(title = title, subtitle = subtitle, x = "", y = "") +
      theme_bw() +
      theme(plot.title    = element_text(size = 9, face = "bold"),
            plot.subtitle = element_text(size = 7.5, color = "grey40"))
  }
  
  p_ent  <- make_onchain_panel(df, "shannon_entropy",    "entropy_7d_avg",
                               "Shannon Entropy",   "Wallet diversity — high = organic",           "steelblue")
  p_gini <- make_onchain_panel(df, "gini_coefficient",   "gini_7d_avg",
                               "Gini Coefficient",  "Tx size inequality — high = whale dominance", "darkorange")
  p_pin  <- make_onchain_panel(df, "pin_proxy",          "pin_7d_avg",
                               "PIN Proxy",         "Order imbalance — high = info asymmetry",     "purple")
  p_bp   <- make_onchain_panel(df, "big_players_vol_pct","big_player_7d_avg",
                               "Big Player Vol %",  "Institutional activity — % of daily volume",  "darkgreen")
  
  p_2x2 <- (p_ent | p_gini) / (p_pin | p_bp) +
    plot_annotation(
      title    = paste(coin_name, "— On-Chain Metrics Panel"),
      subtitle = "Grey = daily | Coloured = 7-day rolling average",
      theme    = theme(plot.title = element_text(face = "bold"))
    )
  
  # Net swap flow — correct units and y-axis label per coin
  is_usd   <- coin_name %in% usd_flow_coins
  flow_y   <- if (is_usd) df$net_swap_flow / 1e6    else df$net_swap_flow
  avg_y    <- if (is_usd) df$net_flow_7d_avg / 1e6  else df$net_flow_7d_avg
  y_label  <- if (is_usd) "Net Flow (USD M)"         else "Net Flow (Token Units)"
  subtitle_flow <- if (is_usd)
    "Green = capital entering | Red = exiting | Black line = 7d avg"
  else
    "Green = token inflow | Red = token outflow | Black line = 7d avg | Note: raw token units, not USD"
  
  df_flow <- df %>%
    filter(!is.na(net_swap_flow)) %>%
    mutate(
      flow_scaled = if (coin_name %in% usd_flow_coins) net_swap_flow / 1e6 else net_swap_flow,
      avg_scaled  = if (coin_name %in% usd_flow_coins) net_flow_7d_avg / 1e6 else net_flow_7d_avg
    )
  
  p_flow <- ggplot(df_flow, aes(x = date)) +
    geom_col(aes(y = flow_scaled, fill = flow_scaled > 0), alpha = 0.7) +
    geom_line(aes(y = avg_scaled), color = "black", linewidth = 0.7, na.rm = TRUE) +
    geom_hline(yintercept = 0, color = "grey40") +
    depeg_vline(coin_name) +
    scale_fill_manual(values = c("TRUE" = "seagreen", "FALSE" = "tomato"),
                      labels = c("TRUE" = "Net Inflow", "FALSE" = "Net Outflow")) +
    labs(title    = paste(coin_name, "— Net Swap Flow (Capital Direction)"),
         subtitle = subtitle_flow,
         x = "", y = y_label, fill = "") +
    theme_bw() + theme(legend.position = "top")
  
  ggsave(paste0(PLOT_DIR, tolower(coin_name), "_03_onchain_panel.png"),
         p_2x2,  width = 12, height = 8, dpi = 300)
  ggsave(paste0(PLOT_DIR, tolower(coin_name), "_03_netflow.png"),
         p_flow, width = 12, height = 4, dpi = 300)
  print(p_2x2)
  print(p_flow)
}


# =============================================================================
# PLOT 4: Fear & Greed vs Price — dual-axis
# FIX:    F&G is now ONE continuous line coloured by sentiment using geom_point
#         instead of 5 separate lines split by classification
# =============================================================================
fng_colors <- c(
  "Extreme Fear"  = "#d73027",
  "Fear"          = "#fc8d59",
  "Neutral"       = "#878787",
  "Greed"         = "#91cf60",
  "Extreme Greed" = "#1a9850"
)

for (coin_name in names(coin_list)) {
  df <- coin_list[[coin_name]] %>%
    filter(!is.na(value), !is.na(value_classification)) %>%
    mutate(value_classification = factor(
      value_classification,
      levels = c("Extreme Fear", "Fear", "Neutral", "Greed", "Extreme Greed")
    ))
  
  price_min  <- min(df$close, na.rm = TRUE)
  price_max  <- max(df$close, na.rm = TRUE)
  fng_scaled <- function(x) price_min + (x / 100) * (price_max - price_min)
  fng_inv    <- function(x) (x - price_min) / (price_max - price_min) * 100
  
  p <- ggplot(df, aes(x = date)) +
    # Depeg shading
    geom_rect(
      data = df %>% filter(depeg_1d == 1),
      aes(xmin = date, xmax = date + 1, ymin = -Inf, ymax = Inf),
      fill = "tomato", alpha = 0.15, inherit.aes = FALSE
    ) +
    # F&G as single grey line + coloured points on top
    geom_line(aes(y = fng_scaled(value)), color = "grey70", linewidth = 0.5, na.rm = TRUE) +
    geom_point(aes(y = fng_scaled(value), color = value_classification),
               size = 0.9, na.rm = TRUE) +
    # Close price
    geom_line(aes(y = close), color = "black", linewidth = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = 1.0, linetype = "dotted", color = "grey50") +
    depeg_vline(coin_name) +
    scale_color_manual(values = fng_colors) +
    scale_y_continuous(
      name     = "Close Price (USD)",
      sec.axis = sec_axis(~ fng_inv(.), name = "Fear & Greed Index (0–100)")
    ) +
    labs(title    = paste(coin_name, "— Price vs Fear & Greed Index"),
         subtitle = "Black = close price | Dots = F&G index coloured by sentiment | Red shading = depeg events",
         x = "", color = "Sentiment") +
    theme_bw() +
    theme(legend.position      = "bottom",
          axis.title.y.right   = element_text(color = "grey40"))
  
  ggsave(paste0(PLOT_DIR, tolower(coin_name), "_04_fng.png"), p, width = 12, height = 5, dpi = 300)
  print(p)
}


# =============================================================================
# PLOT 5: Correlation Heatmap — sorted by |corr| to abs_peg_deviation
# NOTE:   log_diff_net_flow and net_swap_flow are near-perfectly collinear
#         for PAX (-0.97) and USDT (-1.0) — flag this in model selection
# =============================================================================
heatmap_cols <- c(
  "abs_peg_deviation",
  "parkinson_daily", "parkinson_7d", "volatility_7d", "volatility_30d",
  "rvol", "rsi_14", "bb_pctb",
  "shannon_entropy", "gini_coefficient", "pin_proxy",
  "net_swap_flow", "big_players_vol_pct",
  "log_diff_entropy", "log_diff_gini", "log_diff_pin", "log_diff_net_flow",
  "value"
)

for (coin_name in names(coin_list)) {
  df   <- coin_list[[coin_name]]
  cols <- heatmap_cols[heatmap_cols %in% colnames(df)]
  
  cor_mat <- cor(df[, cols], use = "pairwise.complete.obs")
  
  corr_to_target <- abs(cor_mat["abs_peg_deviation", ])
  sorted_cols    <- names(sort(corr_to_target, decreasing = TRUE))
  cor_mat_sorted <- cor_mat[sorted_cols, sorted_cols]
  
  cor_long <- as.data.frame(cor_mat_sorted) %>%
    tibble::rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    mutate(
      var1 = factor(var1, levels = sorted_cols),
      var2 = factor(var2, levels = sorted_cols)
    ) %>%
    filter(as.integer(var1) >= as.integer(var2))
  
  # Flag collinearity warning for PAX and USDT
  collinear_note <- if (coin_name %in% c("PAX", "USDT"))
    " | ⚠ net_swap_flow & log_diff_net_flow near-perfectly collinear — keep only one in model"
  else ""
  
  p <- ggplot(cor_long, aes(x = var2, y = var1, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(correlation, 2)), size = 2.3) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title    = paste(coin_name, "— Feature Correlation Heatmap"),
         subtitle = paste0("Sorted by |corr| to abs_peg_deviation — most predictive features top-left",
                           collinear_note),
         x = "", y = "", fill = "Corr") +
    theme_bw() +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y  = element_text(size = 8),
          plot.subtitle = element_text(size = 7.5))
  
  ggsave(paste0(PLOT_DIR, tolower(coin_name), "_05_heatmap.png"), p, width = 12, height = 10, dpi = 300)
  print(p)
}


# =============================================================================
# PLOT 6 (UST ONLY): Pre-Depeg Early Warning Panel
# 60-day window before May 9 2022
# Panel 1: WLUNA price collapse | Panel 2: Net swap flow | Panel 3: Peg deviation
# =============================================================================
ust_window <- df_ust %>%
  filter(date >= as.Date("2022-03-10"), date <= as.Date("2022-05-08"))

p_wluna <- ggplot(ust_window, aes(x = date, y = close_wluna)) +
  geom_area(fill = "tomato", alpha = 0.3) +
  geom_line(color = "darkred", linewidth = 0.8) +
  geom_vline(xintercept = depeg_date, linetype = "dashed", color = "red", linewidth = 0.7) +
  labs(title    = "UST Pre-Depeg Early Warning (Mar 10 – May 8, 2022)",
       subtitle = "Panel 1: WLUNA price — fell from ~$115 to $64 before UST broke",
       x = "", y = "WLUNA (USD)") +
  theme_bw() + theme(plot.title = element_text(face = "bold"))

p_flow_ust <- ggplot(ust_window %>% filter(!is.na(net_swap_flow)),
                     aes(x = date, y = net_swap_flow / 1e6, fill = net_swap_flow > 0)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_vline(xintercept = depeg_date, linetype = "dashed", color = "red", linewidth = 0.7) +
  scale_fill_manual(values = c("TRUE" = "seagreen", "FALSE" = "tomato"), guide = "none") +
  labs(subtitle = "Panel 2: Net Swap Flow — capital started fleeing weeks before depeg",
       x = "", y = "Net Flow (USD M)") +
  theme_bw()

p_dev_ust <- ggplot(ust_window %>% filter(!is.na(abs_peg_deviation)),
                    aes(x = date, y = abs_peg_deviation * 100)) +
  geom_area(fill = "steelblue", alpha = 0.4) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_vline(xintercept = depeg_date, linetype = "dashed", color = "red", linewidth = 0.7) +
  labs(subtitle = "Panel 3: Absolute Peg Deviation (%) — creeping up in the final days",
       x = "", y = "Deviation (%)") +
  theme_bw()

p_warning <- p_wluna / p_flow_ust / p_dev_ust +
  plot_annotation(
    caption = "Red dashed line = depeg date (2022-05-09)",
    theme   = theme(plot.caption = element_text(color = "red", face = "italic"))
  )

ggsave(paste0(PLOT_DIR, "ust_06_early_warning.png"), p_warning, width = 12, height = 10, dpi = 300)
print(p_warning)
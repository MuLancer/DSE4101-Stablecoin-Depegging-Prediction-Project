library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(zoo)
library(tidyr)

DATA_DIR <- "../data/"
PLOT_DIR <- "../plots/"

df_transactions <- read_csv(file.path(DATA_DIR, "transactions_cleaned.csv"))
df_prices <- read_csv(file.path(DATA_DIR, "prices_with_events_pricefeatures.csv"))

# Add percentage change features
df_prices <- df_prices %>%
  group_by(stablecoin) %>%
  arrange(timestamp) %>%
  mutate(
    # 1-day percentage changes
    pct_open_1d = (open - lag(open, 1)) / lag(open, 1),
    pct_high_1d = (high - lag(high, 1)) / lag(high, 1),
    pct_low_1d = (low - lag(low, 1)) / lag(low, 1),
    pct_close_1d = (close - lag(close, 1)) / lag(close, 1),
    
    # 3-day percentage changes
    pct_open_3d = (open - lag(open, 3)) / lag(open, 3),
    pct_high_3d = (high - lag(high, 3)) / lag(high, 3),
    pct_low_3d = (low - lag(low, 3)) / lag(low, 3),
    pct_close_3d = (close - lag(close, 3)) / lag(close, 3),
    
    # 7-day percentage changes
    pct_open_7d = (open - lag(open, 7)) / lag(open, 7),
    pct_high_7d = (high - lag(high, 7)) / lag(high, 7),
    pct_low_7d = (low - lag(low, 7)) / lag(low, 7),
    pct_close_7d = (close - lag(close, 7)) / lag(close, 7),
    
    # 30-day percentage changes
    pct_open_30d = (open - lag(open, 30)) / lag(open, 30),
    pct_high_30d = (high - lag(high, 30)) / lag(high, 30),
    pct_low_30d = (low - lag(low, 30)) / lag(low, 30),
    pct_close_30d = (close - lag(close, 30)) / lag(close, 30)
  ) %>%
  ungroup()

# Shannon's Entropy
df_entropy <- df_transactions %>%
  group_by(date, token_name) %>%
  summarise(
    shannon_entropy = {
      if (n() > 1 && sum(value) > 0) {
        probs <- value / sum(value)
        probs <- probs[probs > 0]
        -sum(probs * log2(probs))
      } else {
        NA_real_
      }
    },
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(log_diff_entropy = log(1 + shannon_entropy / lag(shannon_entropy))) %>%
  ungroup()

# Gini Coefficient
calculate_gini <- function(x) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  if (n == 0) return(NA_real_)
  x <- sort(x)
  gini <- (2 * sum((1:n) * x)) / (n * sum(x)) - (n + 1) / n
  return(gini)
}

df_gini <- df_transactions %>%
  group_by(date, token_name) %>%
  summarise(
    gini_coefficient = calculate_gini(value),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(log_diff_gini = log(1 + gini_coefficient / lag(gini_coefficient))) %>%
  ungroup()

# Net Swap Flows
df_flows <- df_transactions %>%
  group_by(date, token_name) %>%
  summarise(
    total_inflow = sum(value[to_address != "0x0000000000000000000000000000000000000000"], na.rm = TRUE),
    total_outflow = sum(value[from_address != "0x0000000000000000000000000000000000000000"], na.rm = TRUE),
    net_swap_flow = total_inflow - total_outflow,
    n_transactions = n(),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(
    log_diff_net_flow = log(1 + abs(net_swap_flow) / lag(abs(net_swap_flow) + 1))
  ) %>%
  ungroup()

# PIN
calculate_pin <- function(df) {
  df %>%
    group_by(date, token_name) %>%
    summarise(
      n_buys = sum(value > median(value), na.rm = TRUE),
      n_sells = sum(value <= median(value), na.rm = TRUE),
      total_trades = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      order_imbalance = abs(n_buys - n_sells) / total_trades,
      pin_proxy = order_imbalance
    ) %>%
    group_by(token_name) %>%
    arrange(date) %>%
    mutate(
      pin_7d = rollapply(pin_proxy, width = 7, FUN = mean, fill = NA, align = "right"),
      log_diff_pin = log(1 + pin_7d / lag(pin_7d))
    ) %>%
    ungroup()
}

df_pin <- calculate_pin(df_transactions)

# Markout
df_markout <- df_prices %>%
  group_by(stablecoin) %>%
  arrange(timestamp) %>%
  mutate(
    is_depegged = (abs(close - 1.0) > 0.02)
  )

horizons <- c(1, 3, 7, 14, 30)
for (h in horizons) {
  df_markout <- df_markout %>%
    mutate(
      !!paste0("markout_", h, "d") := log(lead(close, n = h) / close)
    )
}
df_markout <- ungroup(df_markout)

# Shark Trades
df_sharks <- df_transactions %>%
  group_by(from_address, token_name) %>%
  summarise(
    total_volume = sum(value, na.rm = TRUE),
    n_trades = n(),
    avg_trade_size = mean(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  mutate(
    is_shark = (total_volume > quantile(total_volume, 0.9, na.rm = TRUE)) & 
      (n_trades > quantile(n_trades, 0.75, na.rm = TRUE))
  ) %>%
  ungroup()

df_shark_activity <- df_transactions %>%
  left_join(df_sharks %>% select(from_address, token_name, is_shark), 
            by = c("from_address", "token_name")) %>%
  group_by(date, token_name) %>%
  summarise(
    shark_volume = sum(value[is_shark == TRUE], na.rm = TRUE),
    total_volume = sum(value, na.rm = TRUE),
    shark_volume_pct = shark_volume / total_volume,
    n_shark_trades = sum(is_shark == TRUE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(token_name) %>%
  arrange(date) %>%
  mutate(
    log_diff_shark_pct = log(1 + shark_volume_pct / lag(shark_volume_pct))
  ) %>%
  ungroup()

# Combine all features and remove duplicates
df_all <- df_entropy %>%
  left_join(df_gini, by = c("date", "token_name")) %>%
  left_join(df_flows, by = c("date", "token_name")) %>%
  left_join(df_pin %>% select(date, token_name, order_imbalance, pin_7d, log_diff_pin), 
            by = c("date", "token_name")) %>%
  left_join(df_shark_activity, by = c("date", "token_name")) %>%
  left_join(
    df_prices,
    by = c("date" = "timestamp", "token_name" = "stablecoin")
  ) %>%
  left_join(
    df_markout %>% select(timestamp, stablecoin, starts_with("markout_")),
    by = c("date" = "timestamp", "token_name" = "stablecoin")
  ) %>%
  select(-pct_close_1d)  # duplicate of log_return

write_csv(df_all, file.path(DATA_DIR, "df_all.csv"))

# Print final column names
cat("Final columns in df_all:\n")
print(colnames(df_all))
cat("\nTotal columns:", ncol(df_all), "\n")

depeg_date <- ymd("2022-05-09")
p <- df_all %>%
  filter(date >= ymd("2022-04-01"), date <= ymd("2022-06-30")) %>%
  select(date, token_name, shannon_entropy, log_diff_entropy, 
         gini_coefficient, log_diff_gini, 
         volatility_7d, 
         pin_7d, log_diff_pin, 
         shark_volume_pct, log_diff_shark_pct) %>%
  pivot_longer(cols = -c(date, token_name),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = metric)) +
  geom_line(na.rm = TRUE) +
  geom_vline(xintercept = as.numeric(depeg_date), linetype = "dashed", color = "red", linewidth = 0.75) +
  facet_grid(metric ~ token_name, scales = "free_y") +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
  labs(title = "All Features with Depeg Date (2022-05-09)", x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(PLOT_DIR, "transactions_features.png"), p, width = 16, height = 14, dpi = 300)
print(p)

# Correlation heatmap
df_heatmap <- df_all %>%
  filter(date >= ymd("2022-04-01"), date <= ymd("2022-06-30")) %>%
  select(token_name, close, shannon_entropy, gini_coefficient, 
         net_swap_flow, volatility_7d, pin_7d, shark_volume_pct,
         log_diff_entropy, log_diff_gini, log_diff_net_flow,
         log_diff_pin, log_diff_shark_pct) %>%
  group_by(token_name) %>%
  nest() %>%
  mutate(
    corr_matrix = map(data, ~{
      cor_mat <- cor(.x, use = "pairwise.complete.obs")
      cor_mat[upper.tri(cor_mat)] <- NA
      as.data.frame(cor_mat) %>%
        rownames_to_column("var1") %>%
        pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
        filter(!is.na(correlation))
    })
  ) %>%
  select(-data) %>%
  unnest(corr_matrix)

p_heatmap <- ggplot(df_heatmap, aes(x = var2, y = var1, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), size = 2.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  facet_wrap(~token_name, ncol = 3) +
  labs(title = "Feature Correlation Heatmap by Token", x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(face = "bold"))

ggsave(file.path(PLOT_DIR, "transactions_heatmap.png"), p_heatmap, width = 18, height = 12, dpi = 300)
print(p_heatmap)
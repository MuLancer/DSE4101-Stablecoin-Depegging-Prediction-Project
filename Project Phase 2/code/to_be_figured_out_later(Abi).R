df_dai  <- read.csv("data/DAI/DAI_onchain_features.csv")
df_pax  <- read.csv("data/PAX/PAX_onchain_features.csv")
df_usdc <- read.csv("data/USDC/USDC_onchain_features.csv")
df_usdt <- read.csv("data/USDT/USDT_onchain_features.csv")
df_ust  <- read.csv("data/UST/UST_onchain_features.csv")

##below is copy pasted from prev script, dont run##
##plots
depeg_date <- ymd("2022-05-09")
p <- df_features %>%
  filter(date >= ymd("2022-04-01"), date <= ymd("2022-06-30")) %>%
  select(date, token_name, shannon_entropy, log_diff_entropy, 
         gini_coefficient, log_diff_gini, 
         volatility_7d, log_diff_volatility_7d, 
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

#ggsave(file.path(PLOT_DIR, "transactions_features.png"), p, width = 16, height = 14, dpi = 300)
print(p)

# Correlation heatmap
df_heatmap <- df_features %>%
  filter(date >= ymd("2022-04-01"), date <= ymd("2022-06-30")) %>%
  select(token_name, close, shannon_entropy, gini_coefficient, 
         net_swap_flow, volatility_7d, pin_7d, shark_volume_pct,
         log_diff_entropy, log_diff_gini, log_diff_net_flow,
         log_diff_volatility_7d, log_diff_pin, log_diff_shark_pct) %>%
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



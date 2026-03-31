# rf and gb feature importance analysis

library(dplyr)

rf_logloss_imp_all <- read.csv("../../plots/rf_logloss_importance_all.csv")
gb_logloss_imp_all = read.csv("../../plots/gb_logloss_importance_all.csv")




# gb
# Create the top 5 importance summary
gb_top_features <- gb_logloss_imp_all %>%
  group_by(window, coin, horizon) %>%
  # Slice the top 5 based on the importance_logloss value
  slice_max(order_by = importance_logloss, n = 5) %>%
  # Select only the relevant columns for a clean view
  select(window, coin, horizon, feature, importance_logloss) %>%
  # Optional: arrange by coin and horizon for easier reading
  arrange(window, coin, horizon, desc(importance_logloss))

# View the first few rows
print(head(gb_top_features, 10))

# If you want to check a specific coin (e.g., DAI)
gb_top_features %>% 
  ungroup() %>%
  filter(coin == "UST") %>% 
  filter(window == "Window 1") %>% 
  filter(horizon == "depeg_7d") %>%
  select(c(4,5))

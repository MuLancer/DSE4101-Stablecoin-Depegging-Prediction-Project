library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(zoo)
library(TTR)
library(tidyr)

dai <- read_csv("Project Phase 2/data/Dai/dai_price_technical_features.csv")
pax <- read_csv("Project Phase 2/data/Pax Dollar/pax_price_technical_features.csv")
usdc <- read_csv("Project Phase 2/data/USDC/usdc_price_technical_features.csv")
usdt <- read_csv("Project Phase 2/data/Tether USDt/usdt_price_technical_features.csv")
terra_usd <- read_csv("Project Phase 2/data/TerraClassicUSD/terra_usd_price_technical_features.csv")

dai$date <- as.Date(dai$date)
pax$date <- as.Date(pax$date)
usdc$date <- as.Date(usdc$date)
usdt$date <- as.Date(usdt$date)
terra_usd$date <- as.Date(terra_usd$date)

dai <- dai[order(dai$date), ]
pax <- pax[order(pax$date), ]
usdc <- usdc[order(usdc$date), ]
usdt <- usdt[order(usdt$date), ]
terra_usd <- terra_usd[order(terra_usd$date), ]

# Adding fear and greed
fng = read.csv("Project Phase 2/data/fear_and_greed_index_2.csv") %>%
  mutate(date = as.Date(date))

add_fng <- function(df) {
  df %>%
    left_join(fng, by = "date")
}
dai <- add_fng(dai)
pax <- add_fng(pax)
usdc <- add_fng(usdc)
usdt <- add_fng(usdt)
terra_usd <- add_fng(terra_usd)

# Adding momentum/pct change
add_pct_change <- function(df) {
  df %>%
    arrange(date) %>%
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
    ) 
}

dai <- add_pct_change(dai)
pax <- add_pct_change(pax)
usdc <- add_pct_change(usdc)
usdt <- add_pct_change(usdt)
terra_usd <- add_pct_change(terra_usd)


# Adding macro indicators
DATA_DIR <- "Project Phase 2/data/"
PLOT_DIR <- "Project Phase 2/plots/"

df_macro = read.csv(paste0(DATA_DIR,"macro_indicators_all.csv"), colClasses = c("token_address" = "character"))

# 1. (price-related) Markout and will depeg
add_markout <- function(df) {
  df <- df %>%
    arrange(date) %>%
    # 1. First, define if the coin IS currently depegged
    mutate(
      is_depegged = as.integer(close < ThreshD | close > ThreshU)
    )
  
  # 2. Then, calculate future markout and will_depeg labels
  horizons <- c(1, 3, 7, 14, 30)
  for (h in horizons) {
    # Markout: The log-return to the price 'h' days away
    df <- df %>%
      mutate(!!paste0("markout_", h, "d") := log(lead(close, n = h) / close))
    
    # Will Depeg: Looking forward from tomorrow (i+1) to horizon (i+h)
    df[[paste0("will_depeg_", h, "d")]] <- sapply(1:nrow(df), function(i) {
      start_idx <- i + 1
      end_idx <- min(i + h, nrow(df))
      
      # Handle end of dataset
      if (start_idx > nrow(df)) return(NA)
      
      # Slice future prices and their respective future thresholds
      future_prices <- df$close[start_idx:end_idx]
      future_lows   <- df$ThreshD[start_idx:end_idx]
      future_highs  <- df$ThreshU[start_idx:end_idx]
      
      # Check if any future price violates its own future threshold
      any_violation <- any(future_prices < future_lows | future_prices > future_highs, na.rm = TRUE)
      return(as.integer(any_violation))
    })
  }
  return(df)
}


dai       <- add_markout(dai)
pax       <- add_markout(pax)
usdc      <- add_markout(usdc)
usdt      <- add_markout(usdt)
terra_usd <- add_markout(terra_usd)


# 2. Map Addresses to Names
# This makes the df_macro easier to handle and ensures joins are clean
token_mapping <- data.frame(
  token_address = c(
    '0x6b175474e89094c44da98b954eedeac495271d0f', # DAI
    '0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48', # USDC
    '0xdac17f958d2ee523a2206206994597c13d831ec7', # USDT
    '0x89d24a6b4ccb1b6faa2625fe562bdd9a23260359', # PAX
    '0x0000000000085d4780b73119b644ae5ecd22b376'  # UST
  ),
  token_name = c("DAI", "USDC", "USDT", "PAX", "UST"),
  stringsAsFactors = FALSE
)

df_macro <- df_macro %>%
  mutate(date = as.Date(date)) %>%
  mutate(gini_coefficient=abs(gini_coefficient)) %>%
  mutate(token_address = as.character(token_address)) %>%
  left_join(token_mapping, by = "token_address")

# 3. Adding 7d rolling averages and log diffs
clean_macro_features <- function(price_df, name_label) {
  
  # Filter macro data for this specific token name
  token_macro <- df_macro %>%
    filter(token_name == name_label) %>%
    arrange(date)
  
  # Join and calculate features
  df <- price_df %>%
    left_join(token_macro, by = "date") %>%
    arrange(date) %>%
    mutate(
      # --- ENTROPY ---
      entropy_7d_avg   = rollapply(shannon_entropy, 7, mean, fill = NA, align = "right"),
      log_diff_entropy = log(1 + shannon_entropy / (lag(shannon_entropy) + 0.0001)),
      
      # --- GINI (Using the ABS fix we discussed) ---
      gini_7d_avg      = rollapply(gini_coefficient, 7, mean, fill = NA, align = "right"),
      log_diff_gini    = log(1 + gini_coefficient / (lag(gini_coefficient) + 0.0001)),
      
      # --- NET FLOWS ---
      net_flow_7d_avg   = rollapply(net_swap_flow, 7, mean, fill = NA, align = "right"),
      log_diff_net_flow = log(1 + abs(net_swap_flow) / (lag(abs(net_swap_flow)) + 1)),
      
      # --- PIN / ORDER IMBALANCE ---
      pin_7d_avg       = rollapply(pin_proxy, 7, mean, fill = NA, align = "right"),
      log_diff_pin     = log(1 + pin_proxy / (lag(pin_proxy) + 0.0001)),
      
      # --- SHARK ACTIVITY ---
      shark_pct_7d_avg   = rollapply(shark_volume_pct, 7, mean, fill = NA, align = "right"),
      log_diff_shark_pct = log(1 + shark_volume_pct / (lag(shark_volume_pct) + 0.001))
    ) %>%
    # Remove rownames
    select(-any_of("X")) %>%
    # --- REORDERING COLUMNS ---
    relocate(token_name, .after = token_address) %>%
    relocate(entropy_7d_avg, log_diff_entropy, .after = shannon_entropy) %>%
    relocate(gini_7d_avg, log_diff_gini, .after = gini_coefficient) %>%
    relocate(net_flow_7d_avg, log_diff_net_flow, .after = net_swap_flow) %>%
    relocate(pin_7d_avg, log_diff_pin, .after = pin_proxy) %>%
    relocate(shark_pct_7d_avg, log_diff_shark_pct, .after = shark_volume_pct)
  
  return(df)
}

dai       <- clean_macro_features(dai, "DAI")
pax       <- clean_macro_features(pax, "PAX")
usdc      <- clean_macro_features(usdc, "USDC")
usdt      <- clean_macro_features(usdt, "USDT")
terra_usd <- clean_macro_features(terra_usd, "UST")


# Save datasets
write.csv(dai, "Project Phase 2/data/Dai/dai_price_macro_features.csv", row.names = FALSE)
write.csv(pax, "Project Phase 2/data/Pax Dollar/pax_macro_features.csv", row.names = FALSE)
write.csv(usdc, "Project Phase 2/data/USDC/usdc_macro_features.csv", row.names = FALSE)
write.csv(usdt, "Project Phase 2/data/Tether USDt/usdt_macro_features.csv", row.names = FALSE)
write.csv(terra_usd, "Project Phase 2/data/TerraClassicUSD/terra_usd_macro_features.csv", row.names = FALSE)

df_dai = read.csv("Project Phase 2/data/Dai/dai_price_macro_features.csv")
df_pax = read.csv("Project Phase 2/data/Pax Dollar/pax_macro_features.csv")
df_usdc = read.csv("Project Phase 2/data/USDC/usdc_macro_features.csv")
df_usdt = read.csv("Project Phase 2/data/Tether USDt/usdt_macro_features.csv")
df_terra_usd = read.csv("Project Phase 2/data/TerraClassicUSD/terra_usd_macro_features.csv")

# --- ON-CHAIN METRIC DEFINITIONS ---

# 1. Shannon's Entropy (shannon_entropy)
# Measures "Wallet Diversity." 
# High = Volume is spread across many different users (Decentralized/Organic).
# Low = Volume is concentrated in just a few wallets (Centralized/Manipulated).

# 2. Gini Coefficient (gini_coefficient)
# Measures "Size Inequality" of transactions.
# 0 = All transfers are roughly the same size.
# 1 = One single transaction accounts for almost all volume (Whale dominance).

# 3. PIN Proxy / Order Imbalance (pin_proxy)
# Measures "Information Asymmetry" or "Panic."
# Based on the imbalance between "buys" (above median size) and "sells" (below median size).
# High = Extreme lopsided trading, often seen during price discovery or depegs.

# 4. Shark Volume Pct (shark_volume_pct)
# Measures "Smart Money Dominance."
# The percentage of total daily volume coming from "Sharks" (Top 10% Vol + Top 25% Frequency).
# High = Institutional/Sophisticated players are controlling the movement of the coin.

# 5. Net Swap Flow (net_swap_flow)
# Measures "Net Liquidity Direction."
# (Total Inflows to non-zero addresses) - (Total Outflows from non-zero addresses).
# Positive = Capital entering the ecosystem; Negative = Capital exiting (Flight to Safety).

# 6. Log-Differences (log_diff_...)
# Measures "Momentum/Acceleration."
# These calculate the rate of change from yesterday to today.
# High values indicate a "Shock" or sudden move that might trigger a depeg alert.

# 7. 7-Day Averages (..._7d_avg)
# Measures "Baseline Trends."
# These smooth out the daily "noise" to show the underlying health of the stablecoin
# over a rolling week, making it easier to spot long-term decay vs. temporary spikes.

# -----------------------------------------------------------------------------------------------

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

ggsave(file.path(PLOT_DIR, "transactions_features.png"), p, width = 16, height = 14, dpi = 300)
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



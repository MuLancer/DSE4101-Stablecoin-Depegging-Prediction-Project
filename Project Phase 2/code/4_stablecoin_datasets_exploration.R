# =============================================================================
# EDA and Visualisation Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. READ DATASETS
# -----------------------------------------------------------------------------

# --- Full processed datasets (for modelling-related plots) ---
DAI_full   <- read_csv("data/DAI/DAI_full_dataset.csv")
PAX_full   <- read_csv("data/PAX/PAX_full_dataset.csv")
USDC_full  <- read_csv("data/USDC/USDC_full_dataset.csv")
USDT_full  <- read_csv("data/USDT/USDT_full_dataset.csv")
WLUNA_full <- read_csv("data/WLUNA/WLUNA_full_dataset.csv")

# --- UST: use raw combined CSV to retain full price history including post-collapse ---
# Processed UST datasets are truncated at 2022-05-08 by design (pre-collapse only)
# Raw combined CSV retains all observations from 2020-11-25 to 2025-12-31
UST_raw <- read_csv("data/UST/UST_combined.csv") %>%
  mutate(date = as.Date(timeOpen)) %>%
  arrange(date)

# --- Short datasets (trimmed, for modelling checks) ---
DAI_short   <- read_csv("data/DAI/DAI_short_dataset.csv")
PAX_short   <- read_csv("data/PAX/PAX_short_dataset.csv")
USDC_short  <- read_csv("data/USDC/USDC_short_dataset.csv")
USDT_short  <- read_csv("data/USDT/USDT_short_dataset.csv")
UST_short   <- read_csv("data/UST/UST_short_dataset.csv")

# -----------------------------------------------------------------------------
# 2. ADD COIN IDENTIFIERS
# -----------------------------------------------------------------------------

DAI_full$coin   <- "DAI"
PAX_full$coin   <- "PAX"
USDC_full$coin  <- "USDC"
USDT_full$coin  <- "USDT"
UST_raw$coin    <- "UST"
WLUNA_full$coin <- "WLUNA"

DAI_short$coin   <- "DAI"
PAX_short$coin   <- "PAX"
USDC_short$coin  <- "USDC"
USDT_short$coin  <- "USDT"
UST_short$coin   <- "UST"

# -----------------------------------------------------------------------------
# 3. COMBINE DATASETS
# -----------------------------------------------------------------------------

# Full combined: for price path plots only
# UST uses raw combined CSV to show full history including post-collapse
stablecoins_price <- bind_rows(
  DAI_full   %>% select(date, close, coin),
  PAX_full   %>% select(date, close, coin),
  USDC_full  %>% select(date, close, coin),
  USDT_full  %>% select(date, close, coin),
  UST_raw    %>% select(date, close, coin),
  WLUNA_full %>% select(date, close, coin)
)

# Short combined: for modelling-related plots (depeg labels, V_monthly etc.)
# UST short is pre-collapse only (2020-11-25 to 2022-05-08) by design
stablecoins_short <- bind_rows(
  DAI_short,
  PAX_short,
  USDC_short,
  USDT_short,
  UST_short
)

# -----------------------------------------------------------------------------
# 4. DATASET SPAN AND EVENT FREQUENCY CHECKS
# -----------------------------------------------------------------------------

# Date spans
stablecoins_price %>%
  group_by(coin) %>%
  summarise(
    start        = min(date),
    end          = max(date),
    observations = n()
  )

# Event frequencies (short datasets only)
stablecoins_short %>%
  group_by(coin) %>%
  summarise(
    events_1d = sum(depeg_1d, na.rm = TRUE),
    rate_1d   = mean(depeg_1d, na.rm = TRUE),
    events_3d = sum(depeg_3d, na.rm = TRUE),
    rate_3d   = mean(depeg_3d, na.rm = TRUE),
    events_5d = sum(depeg_5d, na.rm = TRUE),
    rate_5d   = mean(depeg_5d, na.rm = TRUE),
    events_7d = sum(depeg_7d, na.rm = TRUE),
    rate_7d   = mean(depeg_7d, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 5. PLOT 1: Price Paths (full history including UST post-collapse)
# -----------------------------------------------------------------------------

# UST collapse date marked with vertical dotted line for reference
collapse_line <- data.frame(
  coin       = "UST",
  xintercept = as.Date("2022-05-09")
)

price_plot <- ggplot(stablecoins_price, aes(date, close)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.4) +
  geom_vline(
    data = collapse_line,
    aes(xintercept = xintercept),
    linetype = "dotted", color = "green", linewidth = 0.6
  ) +
  facet_wrap(~coin, scales = "free_y") +
  theme_minimal() +
  labs(
    title    = "Stablecoin Price Paths (Full History)",
    subtitle = "Red dashed = $1 peg | Green dotted = UST collapse (2022-05-09)",
    y        = "Price (USD)",
    x        = "Date"
  )
price_plot

ggsave(
  filename = "plots/stablecoin_price_paths.png",
  plot     = price_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# 6. PLOT 2: Distribution of Peg Deviations (short datasets)
# -----------------------------------------------------------------------------

peg_deviations_plot <- stablecoins_short %>%
  mutate(dev = abs(close - 1)) %>%
  ggplot(aes(dev)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  facet_wrap(~coin, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distribution of Peg Deviations",
    x     = "|Price - 1|",
    y     = "Frequency"
  )
peg_deviations_plot

ggsave(
  filename = "plots/stablecoin_dist_peg_deviations.png",
  plot     = peg_deviations_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# 7. PLOT 3: Timing of Depegging Events (short datasets, 1-day horizon)
# -----------------------------------------------------------------------------

event_timing_plot <- stablecoins_short %>%
  filter(depeg_1d == 1) %>%
  ggplot(aes(date)) +
  geom_histogram(bins = 100, fill = "orange", color = "black") +
  facet_wrap(~coin, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Timing of Depegging Events (1-Day Horizon)",
    x     = "Date",
    y     = "Number of Depeg Events"
  )
event_timing_plot

ggsave(
  filename = "plots/timing_depegging_events.png",
  plot     = event_timing_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# 8. PLOT 4: Peg Deviation vs Trading Liquidity (short datasets)
# -----------------------------------------------------------------------------

vol_dev_plot <- stablecoins_short %>%
  mutate(dev = abs(close - 1)) %>%
  ggplot(aes(V_monthly, dev)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 0.8) +
  scale_x_log10() +
  facet_wrap(~coin, scales = "free") +
  theme_minimal() +
  labs(
    title = "Peg Deviation vs Trading Liquidity",
    x     = "Rolling 30-Day Trading Volume (log scale)",
    y     = "|Price - 1|"
  )
vol_dev_plot

ggsave(
  filename = "plots/deviation_vs_liquidity.png",
  plot     = vol_dev_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# 9. PLOT 5: Peg Deviation by Depeg Label (short datasets, 1-day horizon)
# -----------------------------------------------------------------------------

depeg_dev_plot <- stablecoins_short %>%
  mutate(dev = abs(close - 1)) %>%
  ggplot(aes(dev, factor(depeg_1d, labels = c("Normal", "Depeg")))) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~coin, scales = "free") +
  theme_minimal() +
  labs(
    title = "Peg Deviations During Depegging vs Normal Periods (1-Day Horizon)",
    x     = "|Price - 1|",
    y     = "Period Type"
  )
depeg_dev_plot

ggsave(
  filename = "plots/deviation_vs_depeg.png",
  plot     = depeg_dev_plot,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# -----------------------------------------------------------------------------
# SUMMARY OF PLOTS
# -----------------------------------------------------------------------------
# Plot 1 (full history) -> complete price paths including UST post-collapse
# Plot 2 (short datasets, exclude WLUNA) -> how tightly each coin tracks $1 peg
# Plot 3 (short datasets, exclude WLUNA) -> when depeg events cluster in time
# Plot 4 (short datasets, exclude WLUNA) -> peg deviations vs trading liquidity
# Plot 5 (short datasets, exclude WLUNA) -> deviation distributions by depeg label

cat("All plots saved to plots/ directory.\n")
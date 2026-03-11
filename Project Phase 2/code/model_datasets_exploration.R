# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(readr)
library(dplyr)
library(ggplot2)

dai <- read_csv("data/Dai/Dai_model_dataset.csv")
pax <- read_csv("data/PAx Dollar/Pax_model_dataset.csv")
usdc <- read_csv("data/USDC/USDC_model_dataset.csv")
usdt <- read_csv("data/Tether USDt/Tether_model_dataset.csv")
terra_usd <- read_csv("data/TerraClassicUSD/TerraClassicUSD_model_dataset.csv")

dim(dai) # good enough history
dim(pax) # longest fiat-backed sample
dim(usdc) # long sample
dim(usdt) # largest dataset
dim(terra_usd) # pre-collapse only

names(dai)
names(pax)
names(usdc)
names(usdt)
names(terra_usd)

# add a coin identifer
dai$coin <- "DAI"
pax$coin <- "PAX"
usdc$coin <- "USDC"
usdt$coin <- "USDT"
terra_usd$coin <- "UST"

# combine datasets for exploration only
stablecoins <- bind_rows(dai, pax, usdc, usdt, terra_usd)

# check dataset spans
stablecoins %>%
  group_by(coin) %>%
  summarise(
    start = min(date),
    end = max(date),
    observations = n())
# check event freqs
stablecoins %>%
  group_by(coin) %>%
  summarise(
    events = sum(depeg),
    event_rate = mean(depeg))

# Visualise price behaviour
price_plot <-ggplot(stablecoins, aes(date, close)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~coin, scales = "free_y") +
  theme_minimal() +
  labs(title = "Stablecoin Price Paths",
       y = "Price",
       x = "Date")
# price_plot
ggsave(
  filename = "plots/stablecoin_price_paths.png",
  plot = price_plot,
  width = 10,
  height = 6,
  dpi = 300
)

peg_deviations_plot <- stablecoins %>%
  mutate(dev = abs(close - 1)) %>%
  ggplot(aes(dev)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  facet_wrap(~coin, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Peg Deviations",
       x = "|Price − 1|",
       y = "Frequency")
# peg_deviations_plot
ggsave(
  filename = "plots/stablecoin_dist_peg_deviations.png",
  plot = price_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Visualise event timing
event_timing_plots <- stablecoins %>%
  filter(depeg == 1) %>%
  ggplot(aes(date)) +
  geom_histogram(bins = 100, fill = "orange", color = "black") +
  facet_wrap(~coin, scales = "free_y") +
  theme_minimal() +
  labs(title = "Timing of Depegging Events")
# event_timing_plots
ggsave(
  filename = "plots/timing_depegging_events.png",
  plot = price_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Visualise deviation vs liquidity
vol_dev_plot <- stablecoins %>%
  mutate(dev = abs(close - 1)) %>%
  ggplot(aes(V_monthly, dev)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  scale_x_log10() +
  facet_wrap(~coin, scales = "free") +
  theme_minimal() +
  labs(
    title = "Peg Deviation vs Trading Liquidity",
    x = "Rolling 30-Day Trading Volume (log scale)",
    y = "|Price − 1|"
  )
# vol_dev_plot
ggsave(
  filename = "plots/deviation_vs_liquidity.png",
  plot = vol_dev_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Visualise Depeg vs Deviation 
depeg_dev_plot <- stablecoins %>%
  mutate(dev = abs(close - 1)) %>%
  ggplot(aes(dev, factor(depeg))) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~coin, scales = "free") +
  theme_minimal() +
  labs(
    title = "Peg Deviations During Depegging vs Normal Periods",
    x = "|Price − 1|",
    y = "Depeg Indicator"
  )
# depeg_dev_plot
ggsave(
  filename = "plots/deviation_vs_depeg.png",
  plot = depeg_dev_plot,
  width = 10,
  height = 6,
  dpi = 300
)

## All in all,
# Plot 1 → price behavior across stablecoins
# Plot 2 → how tightly each coin tracks $1
# Plot 3 → when depeg events cluster in time
# Plot 5 → bigger deviations correspond to depeg events
# Plot 4 → how peg deviations vary with trading liquidity.

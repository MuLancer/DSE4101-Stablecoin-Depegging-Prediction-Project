library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)

#Config
cleanpricedata <- read_csv("data/prices_with_events_pricefeatures.csv")

#Plotting Closing Prices for all stablecoins excluding WLUNA and UST
plot_data <- cleanpricedata %>% 
  filter(!(stablecoin %in% c("WLUNA", "UST")))
  
ggplot(plot_data, aes(x = date, y = close, color = stablecoin)) +
  geom_line() +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  labs(title = "Stablecoin Close Prices (Excluding WLUNA, UST)",
       y = "Closing Price (USD)") +
  theme_minimal()

#Summary Stats
summary_stats <- cleanpricedata %>%
  group_by(stablecoin) %>% 
  summarize(
    mean_close = mean(close, na.rm = TRUE),      
    min_price  = min(close, na.rm = TRUE),  
    max_price = max(close, na.rm = TRUE),     
    volatility = sd(close, na.rm = TRUE)
  )
print(summary_stats)

#Depeg Threshold Analysis
depeg_threshold_analysis <- cleanpricedata %>%
    group_by(stablecoin) %>%
    summarize(
        pct_time_depegged_01 = mean(abs_peg_deviation > 0.01, na.rm = TRUE) * 100, #absolute peg deviation > 0.01
        pct_time_depegged_005 = mean(abs_peg_deviation > 0.005, na.rm = TRUE) * 100, #absolute peg deviation > 0.005
        pct_time_depegged_002 = mean(abs_peg_deviation > 0.001, na.rm = TRUE) * 100, #absolute peg deviation > 0.001    
        pct_time_depegged_001 = mean(abs_peg_deviation > 0.001, na.rm = TRUE) * 100 #absolute peg deviation > 0.001 
    )

print(depeg_threshold_analysis)



#################
### Load Data ###
#################

data <- read.csv("df_all.csv") 

############################
### Candlestick Analysis ###
############################
library(lubridate)
library(ggplot2)
library(tidyquant)
library(dplyr)
library(gridExtra)

# Full timeframe
data$date <- as.Date(data$date)

plot_full_list <- list()
for (i in unique(data$stablecoin)){
  plot <- data %>% filter(stablecoin == i) %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen",
                     colour_down = "red3",
                     fill_up = "forestgreen",
                     fill_down = "red") +
    theme_tq() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(title = paste(i, "Candlestick Chart"), y = "Price", x = "Date")
  
  plot_full_list[[i]] <- plot
}

grid.arrange(grobs = plot_full_list, nrow = 2, ncol = 3)

# Crash timeframe: 2 May 2022 - 20 May 2022 
# Crash occurred 9 May - 15 May
crash_data <- data %>% filter(date <= as.Date("2022-05-20"))

plot_list <- list()
for (i in unique(crash_data$stablecoin)){
  plot <- crash_data %>% filter(stablecoin == i) %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen",
                     colour_down = "red3",
                     fill_up = "forestgreen",
                     fill_down = "red") +
    theme_tq() +
    scale_x_date(date_breaks = "1 day", date_labels = "%d") +
    labs(title = paste(i, "Candlestick Chart"), y = "Price", x = "Date (May 2022)")
  
  plot_list[[i]] <- plot
}

grid.arrange(grobs = plot_list, nrow = 2, ncol = 3)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)

# =============================================================================
# CONFIGURATION
# =============================================================================
# setwd to project phase 2
PLOT_DIR <- "plots/animals_in_trading"
TOKENS <- c('USDT', 'USDC', 'DAI', 'UST', 'PAX')

DEPEG_DATE <- as.Date('2022-05-09')
DATE_START <- as.Date('2022-05-02')
DATE_END <- as.Date('2022-05-20')

SIZE_LABELS <- c(
  'size_1m_plus' = '$1M+',
  'size_100k_1m' = '$100K-$1M',
  'size_1k_100k' = '$1K-$100K',
  'size_under_1k' = '<$1K'
)

SIZE_CATEGORIES <- c('size_1m_plus', 'size_100k_1m', 'size_1k_100k', 'size_under_1k')
WHALE_THRESHOLD <- 1e6

# =============================================================================
# DATA LOADING
# =============================================================================
load_data <- function() {
  price_dfs <- lapply(TOKENS, function(token) {
    df <- read_csv(
      paste0("data/", token, "/", token, "_full_dataset.csv"),
      show_col_types = FALSE
    )
    df$token_name <- token
    df
  })
  price_df <- bind_rows(price_dfs)
  price_df$date <- as.Date(price_df$date)
  
  df <- read_csv("../data/token_transfers_cleaned.csv", show_col_types = FALSE)
  df$date <- as.Date(df$date)
  
  df <- df %>% left_join(price_df %>% select(date, token_name, close),
                         by = c("date", "token_name")) %>%
    mutate(value_usd = value * close) %>%
    filter(date >= DATE_START & date <= DATE_END)
  
  list(df = df, price_df = price_df)
}

run_load <- function() {
  message("Loading data...")
  data <- load_data()
  message("Data loaded successfully.")
  message("  Transfers: ", nrow(data$df), " rows")
  message("  Price data: ", nrow(data$price_df), " rows")
  return(data)
}

# =============================================================================
# CLASSIFICATION
# =============================================================================
classify_transaction_size <- function(v) {
  if (is.na(v)) return(NA_character_)
  if (v >= 1e6) return('size_1m_plus')
  if (v >= 1e5) return('size_100k_1m')
  if (v >= 1e3) return('size_1k_100k')
  return('size_under_1k')
}

classify_address_size <- function(vol) {
  if (is.na(vol)) return(NA_character_)
  if (vol >= WHALE_THRESHOLD) return('whale')
  return('other')
}

add_classifications <- function(df, price_df) {
  df <- df %>% mutate(
    size_category = sapply(value_usd, classify_transaction_size)
  )
  
  # Daily volume per address
  daily_addr_vol <- df %>% 
    group_by(date, token_name, from_address) %>% 
    summarise(daily_volume = sum(value_usd, na.rm = TRUE), .groups = 'drop') %>%
    mutate(addr_size_category = sapply(daily_volume, classify_address_size))
  
  df <- df %>% left_join(
    daily_addr_vol %>% select(date, token_name, from_address, addr_size_category),
    by = c("date", "token_name", "from_address")
  )
  
  # Performance-based: top 1% by markout
  price_df <- price_df %>% mutate(future_date = date + 1)
  future_prices <- price_df %>% select(future_date, token_name, close) %>% 
    rename(date = future_date, future_close = close)
  
  df <- df %>% left_join(future_prices, by = c("date", "token_name")) %>%
    mutate(markout_1d = value * (future_close - close))
  
  address_markouts <- df %>% group_by(from_address) %>%
    summarise(cumulative_markout = sum(markout_1d, na.rm = TRUE), .groups = 'drop')
  
  threshold <- quantile(address_markouts$cumulative_markout, 0.99, na.rm = TRUE)
  address_markouts <- address_markouts %>% mutate(is_shark = cumulative_markout >= threshold)
  
  df <- df %>% left_join(address_markouts %>% select(from_address, is_shark), by = 'from_address') %>%
    mutate(is_shark = ifelse(is.na(is_shark), FALSE, is_shark)) %>%
    select(-future_close)
  
  df
}

# =============================================================================
# PLOTS
# =============================================================================
add_depeg_line <- function(p) {
  p + geom_vline(xintercept = DEPEG_DATE, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 1)
}

plot_whale_volume_proportion <- function(df) {
  daily_total <- df %>% 
    group_by(date, token_name) %>% 
    summarise(total = sum(value_usd, na.rm = TRUE), .groups = 'drop')
  
  daily_whale <- df %>% 
    filter(addr_size_category == 'whale') %>%
    group_by(date, token_name) %>% 
    summarise(whale = sum(value_usd, na.rm = TRUE), .groups = 'drop')
  
  daily <- daily_total %>% 
    left_join(daily_whale, by = c('date', 'token_name')) %>%
    mutate(whale = replace_na(whale, 0),
           pct_whale = whale / total * 100)
  
  p <- ggplot(daily, aes(x = date)) +
    geom_bar(aes(y = whale / 1e9), stat = 'identity', fill = '#1a5276', alpha = 0.7) +
    geom_line(aes(y = pct_whale), color = '#c0392b', linewidth = 1) +
    geom_vline(xintercept = DEPEG_DATE, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.8) +
    scale_y_continuous(
      name = 'Volume (B USD)',
      sec.axis = sec_axis(~., name = 'Whale %')
    ) +
    facet_wrap(~token_name, scales = 'free_y', ncol = 2) +
    labs(title = "Whale Volume and Proportion by Token") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold", size = 11))
  
  ggsave(paste0(PLOT_DIR, "/whale_all_tokens.png"), p, width = 12, height = 10)
}

plot_stacked_size_composition <- function(df) {
  daily <- df %>% 
    group_by(date, token_name, size_category) %>% 
    summarise(vol = sum(value_usd, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = size_category, values_from = vol, values_fill = 0)
  
  # Ensure all size categories exist
  for (cat in SIZE_CATEGORIES) {
    if (!(cat %in% names(daily))) {
      daily[[cat]] <- 0
    }
  }
  
  daily <- daily %>%
    mutate(total = rowSums(select(., all_of(SIZE_CATEGORIES)), na.rm = TRUE),
           across(all_of(SIZE_CATEGORIES), ~ . / total * 100))
  
  daily_long <- daily %>% 
    pivot_longer(all_of(SIZE_CATEGORIES), names_to = 'size', values_to = 'pct') %>%
    mutate(size = factor(size, levels = SIZE_CATEGORIES))
  
  p <- ggplot(daily_long, aes(x = date, y = pct, fill = size)) +
    geom_area() +
    geom_vline(xintercept = DEPEG_DATE, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.8) +
    scale_fill_manual(values = c('#1a5276', '#2980b9', '#5dade2', '#aed6f1'),
                      labels = SIZE_LABELS,
                      name = "Transaction Size") +
    facet_wrap(~token_name, ncol = 2) +
    labs(title = "Daily Volume Composition by Transaction Size",
         y = "Volume %",
         x = "Date") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold", size = 11),
          legend.position = "bottom")
  
  ggsave(paste0(PLOT_DIR, "/stacked_all_tokens.png"), p, width = 12, height = 10)
}

plot_shark_volume_proportion <- function(df) {
  daily_total <- df %>% 
    group_by(date, token_name) %>% 
    summarise(total = sum(value_usd, na.rm = TRUE), .groups = 'drop')
  
  daily_shark <- df %>% 
    filter(is_shark) %>% 
    group_by(date, token_name) %>% 
    summarise(shark = sum(value_usd, na.rm = TRUE), .groups = 'drop')
  
  daily <- daily_total %>% 
    left_join(daily_shark, by = c('date', 'token_name')) %>%
    mutate(shark = replace_na(shark, 0), 
           pct_shark = shark / total * 100)
  
  p <- ggplot(daily, aes(x = date)) +
    geom_bar(aes(y = shark / 1e9), stat = 'identity', fill = '#27ae60', alpha = 0.7) +
    geom_line(aes(y = pct_shark), color = '#8e44ad', linewidth = 1) +
    geom_vline(xintercept = DEPEG_DATE, linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.8) +
    scale_y_continuous(
      name = 'Volume (B USD)',
      sec.axis = sec_axis(~., name = 'Shark %')
    ) +
    facet_wrap(~token_name, scales = 'free_y', ncol = 2) +
    labs(title = "Shark Volume and Proportion by Token") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold", size = 11))
  
  ggsave(paste0(PLOT_DIR, "/shark_all_tokens.png"), p, width = 12, height = 10)
}

# =============================================================================
# ANALYSIS (run after loading)
# =============================================================================
run_analysis <- function(data) {
  df <- data$df
  price_df <- data$price_df
  
  dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)
  
  message("Adding classifications...")
  df <- add_classifications(df, price_df)
  
  message("Generating whale plots...")
  plot_whale_volume_proportion(df)
  
  message("Generating stacked size plots...")
  plot_stacked_size_composition(df)
  
  message("Generating shark plots...")
  plot_shark_volume_proportion(df)
  
  message("All plots saved to ", PLOT_DIR)
  return(df)
}

# =============================================================================
# MAIN
# =============================================================================
run_all <- function() {
  data <- run_load()
  df <- run_analysis(data)
  return(df)
}

# Run the workflow
df <- run_all()
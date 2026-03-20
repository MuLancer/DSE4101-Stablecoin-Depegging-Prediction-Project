# =============================================================================
# Fear & Greed Index Data Collection Script
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================

# setwd("~/DSE4101 Project/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

library(jsonlite)
library(tidyverse)

# -----------------------------------------------------------------------------
# 1. FETCH RAW DATA FROM API
# -----------------------------------------------------------------------------

# Request 3000 days of history from the Alternative.me Fear & Greed Index API
# This covers the full sample period from 2018 through 2025
# The API returns the most recent N days, so limit=3000 ensures full coverage
url      <- "https://api.alternative.me/fng/?limit=3000"
response <- fromJSON(url)

# -----------------------------------------------------------------------------
# 2. CLEAN AND FILTER
# -----------------------------------------------------------------------------

# Convert raw API response to a data frame
# Timestamps are returned as Unix epoch seconds and converted to UTC datetime
# Index values are cast from character to numeric (0 = Extreme Fear, 100 = Extreme Greed)
# Filter to start exactly from Jan 1, 2018 to align with stablecoin sample periods
fng_history <- as.data.frame(response$data) %>%
  mutate(
    date  = as.POSIXct(as.numeric(timestamp), origin = "1970-01-01", tz = "UTC"),
    value = as.numeric(value)
  ) %>%
  filter(date >= as.POSIXct("2018-01-01")) %>% # Filter to start exactly from Jan 1st, 2018
  select(date, value, value_classification)

# -----------------------------------------------------------------------------
# 3. DIAGNOSTIC PLOT
# -----------------------------------------------------------------------------

# Line plot of daily index values with a GAM trend overlay
# Used to visually verify the fetched history spans the expected date range
# and to inspect broad sentiment regimes across the sample period
ggplot(fng_history, aes(x = date, y = value)) +
  geom_line(color = "darkgreen") +
  geom_smooth(method = "gam", color = "red") +   # Smoothed trend line via GAM
  labs(
    title = "Crypto Fear & Greed Index (2018 - Present)",
    y     = "Index Value (0 = Extreme Fear, 100 = Extreme Greed)",
    x     = "Year"
  ) +
  theme_minimal()

# Abigail to help create another plot for this index and all stablecoin prices across time (grid format)

# -----------------------------------------------------------------------------
# 4. SAVE DATASET
# -----------------------------------------------------------------------------

write_csv(fng_history, "data/fear_and_greed_index_2.csv")

cat("Fear & Greed Index data fetched and cleaned.\n")

# =============================================================================
# FEATURE SUMMARY
# =============================================================================
#
# This dataset contains daily Crypto Fear & Greed Index readings sourced
# from the Alternative.me public API, used as a macro-sentiment feature
# for stablecoin depegging prediction models.
#
# Columns retained:
# - date:                 UTC date of the index reading
# - value:                numeric index score (0 = Extreme Fear, 100 = Extreme Greed)
# - value_classification: categorical label (e.g. "Fear", "Greed", "Neutral")
#
# Coverage: Jan 1, 2018 onwards, aligned with stablecoin sample periods
# Source:   https://api.alternative.me/fng/
# =============================================================================

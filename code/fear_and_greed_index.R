library(jsonlite)
library(tidyverse)

# 1. Increase limit to 2500 to cover 2020-2025
# The API will return the most recent 2500 days
url <- "https://api.alternative.me/fng/?limit=2500"
response <- fromJSON(url)

# 2. Clean and Filter
fng_history <- as.data.frame(response$data) %>%
  mutate(
    date = as.POSIXct(as.numeric(timestamp), origin="1970-01-01", tz="UTC"),
    value = as.numeric(value)
  ) %>%
  # Filter to start exactly from Jan 1st, 2020
  filter(date >= as.POSIXct("2020-01-01")) %>%
  select(date, value, value_classification)

# 3. Quick plot to verify the history
ggplot(fng_history, aes(x = date, y = value)) +
  geom_line(color = "darkgreen") +
  geom_smooth(method = "gam", color = "red") + # Trend line
  labs(title = "Crypto Fear & Greed Index (2020 - Present)",
       y = "Index Value (0-100)", x = "Year") +
  theme_minimal()

write_csv(fng_history, "data/fear_and_greed_index.csv")

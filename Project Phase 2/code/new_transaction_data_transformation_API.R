## DONT ANYHOW RUN THIS SCRIPT BC IT WILL BILL YOUR 1 TB OF BIGQUERY SPACE ##
# ^PLEASE READ!!!!!!

library(bigrquery)
library(dplyr)
library(glue)

options(timeout = 600)

project_id <- "dse4101"
FILEPATH <- "Project Phase 2/data/"

years_to_run <- 2018:2025

sql_template <- "
WITH raw_transfers AS (
  SELECT 
    DATE(block_timestamp) AS date,
    token_address,
    from_address,
    to_address,
    SAFE_CAST(value AS FLOAT64) / 1e18 AS value
  FROM `bigquery-public-data.crypto_ethereum.token_transfers`
  WHERE block_timestamp >= '{year}-01-01' AND block_timestamp < '{next_year}-01-01'
    AND token_address IN (
      '0x6b175474e89094c44da98b954eedeac495271d0f', -- DAI
      '0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48', -- USDC
      '0xdac17f958d2ee523a2206206994597c13d831ec7', -- USDT
      '0x89d24a6b4ccb1b6faa2625fe562bdd9a23260359', -- PAX
      '0x0000000000085d4780b73119b644ae5ecd22b376'  -- UST
    )
    AND SAFE_CAST(value AS FLOAT64) > 0
),
entropy_calc AS (
  SELECT date, token_address, 
         -SUM((wallet_vol/day_vol) * LOG(wallet_vol/day_vol, 2)) AS shannon_entropy
  FROM (
    SELECT date, token_address, from_address, SUM(value) as wallet_vol,
           SUM(SUM(value)) OVER(PARTITION BY date, token_address) as day_vol
    FROM raw_transfers GROUP BY 1, 2, 3
  ) GROUP BY 1, 2
),
gini_calc AS (
  SELECT date, token_address,
         1 - 2 * SUM((value * (idx - 0.5)) / (total_val * n_rows)) AS gini_coefficient
  FROM (
    SELECT date, token_address, value,
           ROW_NUMBER() OVER(PARTITION BY date, token_address ORDER BY value) as idx,
           SUM(value) OVER(PARTITION BY date, token_address) as total_val,
           COUNT(*) OVER(PARTITION BY date, token_address) as n_rows
    FROM raw_transfers
  ) GROUP BY 1, 2
),
pin_calc AS (
  SELECT date, token_address,
         ABS(COUNTIF(value > med) - COUNTIF(value <= med)) / NULLIF(COUNT(*), 0) AS pin_proxy
  FROM (
    SELECT date, token_address, value,
           PERCENTILE_CONT(value, 0.5) OVER(PARTITION BY date, token_address) as med
    FROM raw_transfers
  ) GROUP BY 1, 2
),
shark_calc AS (
  SELECT date, token_address,
         COUNTIF(vol_rank > 0.9 AND count_rank > 0.75) AS n_shark_trades,
         SUM(CASE WHEN vol_rank > 0.9 AND count_rank > 0.75 THEN wallet_volume ELSE 0 END) AS shark_volume,
         SUM(wallet_volume) AS total_volume
  FROM (
    SELECT date, token_address, from_address, SUM(value) as wallet_volume,
           PERCENT_RANK() OVER(PARTITION BY date, token_address ORDER BY SUM(value)) as vol_rank,
           PERCENT_RANK() OVER(PARTITION BY date, token_address ORDER BY COUNT(*)) as count_rank
    FROM raw_transfers GROUP BY 1, 2, 3
  ) GROUP BY 1, 2
),
flow_calc AS (
  SELECT date, token_address,
         SUM(CASE WHEN to_address != '0x0000000000000000000000000000000000000000' THEN value ELSE 0 END) - 
         SUM(CASE WHEN from_address != '0x0000000000000000000000000000000000000000' THEN value ELSE 0 END) AS net_swap_flow
  FROM raw_transfers GROUP BY 1, 2
)
SELECT 
  e.date, e.token_address, e.shannon_entropy, g.gini_coefficient,
  p.pin_proxy, s.n_shark_trades, 
  s.shark_volume / NULLIF(s.total_volume, 0) AS shark_volume_pct,
  f.net_swap_flow
FROM entropy_calc e
JOIN gini_calc g ON e.date = g.date AND e.token_address = g.token_address
JOIN pin_calc p ON e.date = p.date AND e.token_address = p.token_address
JOIN shark_calc s ON e.date = s.date AND e.token_address = s.token_address
JOIN flow_calc f ON e.date = f.date AND e.token_address = f.token_address
ORDER BY e.date, e.token_address"

# --- THE LOOP ---
for (year in years_to_run) {
  
  file_name <- paste0(FILEPATH, "macro_indicators_", year, ".csv")
  
  # Skip if file already exists (protects your credits if you restart the script)
  if (file.exists(file_name)) {
    message(glue("Year {year} already exists. Skipping..."))
    next
  }
  
  message(glue("Processing Year: {year}..."))
  
  next_year <- year + 1
  current_sql <- glue(sql_template)
  
  # Run and download
  bq_table <- bq_project_query(project_id, current_sql)
  df_result <- bq_table_download(bq_table)
  
  # Save
  write.csv(df_result, file_name, row.names = FALSE)
  message(glue("Successfully saved macro_indicators_{year}.csv"))
}

# --- FINAL STEP: COMBINE EVERYTHING (Including 2018) ---
all_files <- list.files(FILEPATH, pattern = "macro_indicators_.*\\.csv", full.names = TRUE)
df_master <- lapply(all_files, function(file) {
  read.csv(file, colClasses = c("token_address" = "character"))
}) %>% bind_rows()

# Check the date range to make sure everything is there
print(range(as.Date(df_master$date)))

# save final file
write.csv(df_master, "Project Phase 2/data/macro_indicators_all.csv", row.names = FALSE)

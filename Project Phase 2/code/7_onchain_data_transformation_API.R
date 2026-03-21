# =============================================================================
# On-Chain Macro Indicator Extraction Script (BigQuery)
# Project: DSE4101 - Stablecoin Depegging Prediction
# =============================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# WARNING: DO NOT RUN THIS SCRIPT CARELESSLY.
# Each query scans billions of Ethereum token transfer records on BigQuery.
# Running all 8 years will consume a significant portion of your 1TB free quota.
# Only run years that are missing from the data/ directory.
# The loop has a file.exists() guard — do not bypass it.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(bigrquery)
library(dplyr)
library(glue)

# Extend timeout to 10 minutes to accommodate large BigQuery downloads
options(timeout = 600)

# Working directory is set at Project Phase 2 root — paths are relative from here
# setwd("~/DSE4101/DSE4101-Stablecoin-Depegging-Prediction-Project/Project Phase 2")

# GCP project ID used for BigQuery billing and job submission
project_id <- "dse4101"

years_to_run <- 2018:2025

# -----------------------------------------------------------------------------
# 2. SQL TEMPLATE
# -----------------------------------------------------------------------------

# Parameterised SQL template using glue for year injection ({year}, {next_year})
# Queries the public Ethereum token transfers table on BigQuery
# Filters to the 5 stablecoins of interest by contract address
# Token value is divided by 1e18 to convert from raw wei to token units
#
# Five CTEs compute daily on-chain metrics per token:
#   entropy_calc      — Shannon entropy of wallet-level volume distribution
#   gini_calc         — Gini coefficient of transaction size inequality
#   pin_calc          — PIN proxy: order imbalance relative to median trade size
#   big_players_calc  — Institutional/Whale activity: volume and count of top-tier, high-frequency wallets
#   flow_calc         — Ecosystem liquidity: gross inflows, gross outflows, and net supply change (mints vs. burns)

# All CTEs are joined on (date, token_address) and results ordered chronologically

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
big_players_calc AS (
  SELECT date, token_address,
         -- Count how many unique Big Player wallets were active
         COUNTIF(vol_rank > 0.9 AND count_rank > 0.75) AS n_big_players,
         
         -- Total volume moved by these specific Big Players
         SUM(CASE WHEN vol_rank > 0.9 AND count_rank > 0.75 THEN wallet_volume ELSE 0 END) AS big_players_volume,
         
         -- Total volume of all wallets (used to calculate the percentage)
         SUM(wallet_volume) AS total_volume
  FROM (
    SELECT date, token_address, from_address, 
           SUM(value) as wallet_volume,
           PERCENT_RANK() OVER(PARTITION BY date, token_address ORDER BY SUM(value)) as vol_rank,
           PERCENT_RANK() OVER(PARTITION BY date, token_address ORDER BY COUNT(*)) as count_rank
    FROM raw_transfers 
    GROUP BY 1, 2, 3
  ) GROUP BY 1, 2
),
flow_calc AS (
  SELECT date, token_address,
         -- 1. Gross Inflow: Everything moving INTO real wallets
         SUM(CASE WHEN to_address != '0x0000000000000000000000000000000000000000' THEN value ELSE 0 END) AS gross_inflow,
         
         -- 2. Gross Outflow: Everything moving OUT of real wallets
         SUM(CASE WHEN from_address != '0x0000000000000000000000000000000000000000' THEN value ELSE 0 END) AS gross_outflow,
         
         -- 3. Net Swap Flow: (Total In - Total Out)
         -- This will be positive if Minting > Burning, and negative if Burning > Minting.
         SUM(CASE WHEN to_address != '0x0000000000000000000000000000000000000000' THEN value ELSE 0 END) - 
         SUM(CASE WHEN from_address != '0x0000000000000000000000000000000000000000' THEN value ELSE 0 END) AS net_swap_flow
  FROM raw_transfers GROUP BY 1, 2
)
SELECT 
  e.date, 
  e.token_address, 
  e.shannon_entropy, 
  g.gini_coefficient,
  p.pin_proxy, 
  b.n_big_players,
  b.big_players_volume / NULLIF(b.total_volume, 0) AS big_players_vol_pct,
  f.gross_inflow, 
  f.gross_outflow, 
  f.net_swap_flow
FROM entropy_calc e
JOIN gini_calc g ON e.date = g.date AND e.token_address = g.token_address
JOIN pin_calc p ON e.date = p.date AND e.token_address = p.token_address
JOIN big_players_calc b ON e.date = b.date AND e.token_address = b.token_address
JOIN flow_calc f ON e.date = f.date AND e.token_address = f.token_address
ORDER BY e.date, e.token_address"

# -----------------------------------------------------------------------------
# 3. YEAR-BY-YEAR BIGQUERY EXTRACTION LOOP
# -----------------------------------------------------------------------------

# Iterates over each year in the sample period and submits one BigQuery job per year
# file.exists() guard skips years already downloaded — safe to re-run after interruptions
# glue() injects {year} and {next_year} into the SQL template before submission
# Results are downloaded directly from BigQuery and written to annual CSV files
for (year in years_to_run) {
  
  file_name <- paste0("data/onchain_metrics_", year, ".csv")
  
  # Skip if this year's file already exists to avoid re-billing
  if (file.exists(file_name)) {
    message(glue("Year {year} already exists. Skipping..."))
    next
  }
  
  message(glue("Processing Year: {year}..."))
  
  next_year <- year + 1
  current_sql <- glue(sql_template)
  
  # Submit query to BigQuery and download result to local data frame
  bq_table  <- bq_project_query(project_id, current_sql)
  df_result <- bq_table_download(bq_table)
  
  # Write annual output to disk
  write.csv(df_result, file_name, row.names = FALSE)
  message(glue("Successfully saved onchain_metrics_{year}.csv"))
}


# -----------------------------------------------------------------------------
# 4. COMBINE ANNUAL FILES INTO MASTER DATASET
# -----------------------------------------------------------------------------

# Glob all annual macro indicator CSVs from the data directory
# token_address read as character to preserve leading zeros in hex addresses
# bind_rows() stacks all years into a single master data frame
all_files <- list.files(pattern = "data/onchain_metrics_.*\\.csv", full.names = TRUE)
df_master <- lapply(all_files, function(file) {
  read.csv(file, colClasses = c("token_address" = "character"))
}) %>% bind_rows()

# Sanity check: verify the combined dataset spans the full 2018–2025 sample period
print(range(as.Date(df_master$date)))

# Save consolidated master file for use in fng_and_macro_indicators.R
write.csv(df_master, "/data/onchain_metrics_all.csv", row.names = FALSE)

cat("Onchain metrics master dataset saved.\n")

# =============================================================================
# FEATURE SUMMARY
# =============================================================================
#
# This script queries the public Ethereum token transfer table on Google
# BigQuery to compute daily on-chain macro indicators for each stablecoin.
# Results are saved annually then combined into a single master CSV.
#
# Output: macro_indicators_all.csv
#
# Columns produced:
# - date:               calendar date of the aggregated transfers
# - token_address:      ERC-20 contract address of the stablecoin
# - shannon_entropy:    wallet diversity index (higher = more decentralised flow)
# - gini_coefficient:   transaction size inequality (higher = more whale dominance)
# - pin_proxy:          order imbalance proxy (higher = more one-sided trading)
# - n_shark_trades:     count of trades from high-volume, high-frequency wallets
# - shark_volume_pct:   share of daily volume attributable to shark wallets
# - net_swap_flow:      net capital flow (inflows minus outflows, burn address excluded)
#
# Tokens covered:
# - DAI, USDC, USDT, PAX, UST (identified by ERC-20 contract address)
#
# Source: bigquery-public-data.crypto_ethereum.token_transfers
# Sample: 2018-01-01 to 2025-12-31 (one BigQuery job per calendar year)
# =============================================================================
# =============================================================================
# Cross-Market Contagion Feature Merging
# =============================================================================

library(readr)
library(dplyr)

# -----------------------------------------------------------------------------
# Read all datasets (full and short)
# -----------------------------------------------------------------------------

DAI_full   <- read_csv("data/DAI/DAI_full_dataset.csv")
DAI_short  <- read_csv("data/DAI/DAI_short_dataset.csv")

PAX_full   <- read_csv("data/PAX/PAX_full_dataset.csv")
PAX_short  <- read_csv("data/PAX/PAX_short_dataset.csv")

USDC_full  <- read_csv("data/USDC/USDC_full_dataset.csv")
USDC_short <- read_csv("data/USDC/USDC_short_dataset.csv")

USDT_full  <- read_csv("data/USDT/USDT_full_dataset.csv")
USDT_short <- read_csv("data/USDT/USDT_short_dataset.csv")

UST_full   <- read_csv("data/UST/UST_full_dataset.csv")
UST_short  <- read_csv("data/UST/UST_short_dataset.csv")

WLUNA_full <- read_csv("data/WLUNA/WLUNA_full_dataset.csv") 

# -----------------------------------------------------------------------------
# Extract UST and WLUNA price series for merging
# -----------------------------------------------------------------------------

UST_price <- UST_full %>%
  select(date, close) %>%
  rename(UST_close = close)

WLUNA_price <- WLUNA_full %>%
  select(date, close) %>%
  rename(WLUNA_close = close)

# -----------------------------------------------------------------------------
# Add UST price to DAI, PAX, USDC, USDT (both full and short)
# NAs left where dates don't overlap — not filled
# -----------------------------------------------------------------------------

DAI_full   <- DAI_full   %>% left_join(UST_price, by = "date")
DAI_short  <- DAI_short  %>% left_join(UST_price, by = "date")

PAX_full   <- PAX_full   %>% left_join(UST_price, by = "date")
PAX_short  <- PAX_short  %>% left_join(UST_price, by = "date")

USDC_full  <- USDC_full  %>% left_join(UST_price, by = "date")
USDC_short <- USDC_short %>% left_join(UST_price, by = "date")

USDT_full  <- USDT_full  %>% left_join(UST_price, by = "date")
USDT_short <- USDT_short %>% left_join(UST_price, by = "date")

# -----------------------------------------------------------------------------
# Add WLUNA price to UST (both full and short)
# -----------------------------------------------------------------------------

UST_full  <- UST_full  %>% left_join(WLUNA_price, by = "date")
UST_short <- UST_short %>% left_join(WLUNA_price, by = "date")

# -----------------------------------------------------------------------------
# Quick NA checks
# -----------------------------------------------------------------------------

cat("--- NA counts for UST_close ---\n")
cat("DAI full:  ",  sum(is.na(DAI_full$UST_close)),   "\n")
cat("DAI short: ",  sum(is.na(DAI_short$UST_close)),  "\n")
cat("PAX full:  ",  sum(is.na(PAX_full$UST_close)),   "\n")
cat("PAX short: ",  sum(is.na(PAX_short$UST_close)),  "\n")
cat("USDC full:  ", sum(is.na(USDC_full$UST_close)),  "\n")
cat("USDC short: ", sum(is.na(USDC_short$UST_close)), "\n")
cat("USDT full:  ", sum(is.na(USDT_full$UST_close)),  "\n")
cat("USDT short: ", sum(is.na(USDT_short$UST_close)), "\n")
# Too many NA values meaning This means UST_close is NOT a useful contagion feature for DAI, PAX, USDC, USDT

cat("--- NA counts for WLUNA_close ---\n")
cat("UST full:  ", sum(is.na(UST_full$WLUNA_close)),  "\n")
cat("UST short: ", sum(is.na(UST_short$WLUNA_close)), "\n")
# No missing values here

# -----------------------------------------------------------------------------
# Extract WLUNA price series for contagion feature
# -----------------------------------------------------------------------------

# WLUNA is used as the primary cross-market contagion feature for all coins.
# Rationale: WLUNA/UST represented the major systemic risk event of 2022.
# Stress in WLUNA likely preceded or coincided with stress in other stablecoins,
# making it a meaningful early warning signal across the sample.
# WLUNA coverage (2019-07-26 to 2025-12-31) overlaps well with all coins.
#
# UST_close was initially considered as a contagion feature for DAI/PAX/USDC/USDT
# but was dropped due to excessive NAs (~76% missing for most coins), as UST only
# existed from Nov 2020 to May 2022. Adding it would cause severe data loss
# or require arbitrary NA imputation, both of which distort model training.

WLUNA_price <- WLUNA_full %>%
  select(date, close) %>%
  rename(WLUNA_close = close)


# -----------------------------------------------------------------------------
# Add WLUNA price to DAI, PAX, USDC, USDT, UST (both full and short)
# Matched by date, NAs left where dates don't overlap
# -----------------------------------------------------------------------------

DAI_full   <- DAI_full   %>% left_join(WLUNA_price, by = "date")
DAI_short  <- DAI_short  %>% left_join(WLUNA_price, by = "date")

PAX_full   <- PAX_full   %>% left_join(WLUNA_price, by = "date")
PAX_short  <- PAX_short  %>% left_join(WLUNA_price, by = "date")

USDC_full  <- USDC_full  %>% left_join(WLUNA_price, by = "date")
USDC_short <- USDC_short %>% left_join(WLUNA_price, by = "date")

USDT_full  <- USDT_full  %>% left_join(WLUNA_price, by = "date")
USDT_short <- USDT_short %>% left_join(WLUNA_price, by = "date")

# UST already has WLUNA_close from previous merge — skip to avoid duplicate
# UST_full and UST_short already contain WLUNA_close with 0 NAs

# -----------------------------------------------------------------------------
# Drop UST_close if it exists in any dataset (too many NAs, dropped by design)
# -----------------------------------------------------------------------------

DAI_full   <- DAI_full   %>% select(-any_of("UST_close"))
DAI_short  <- DAI_short  %>% select(-any_of("UST_close"))

PAX_full   <- PAX_full   %>% select(-any_of("UST_close"))
PAX_short  <- PAX_short  %>% select(-any_of("UST_close"))

USDC_full  <- USDC_full  %>% select(-any_of("UST_close"))
USDC_short <- USDC_short %>% select(-any_of("UST_close"))

USDT_full  <- USDT_full  %>% select(-any_of("UST_close"))
USDT_short <- USDT_short %>% select(-any_of("UST_close"))

# -----------------------------------------------------------------------------
# NA checks for WLUNA_close after merge
# -----------------------------------------------------------------------------

cat("--- NA counts for WLUNA_close ---\n")
cat("DAI full:   ", sum(is.na(DAI_full$WLUNA_close)),   "\n")
cat("DAI short:  ", sum(is.na(DAI_short$WLUNA_close)),  "\n")
cat("PAX full:   ", sum(is.na(PAX_full$WLUNA_close)),   "\n") # some NAs
cat("PAX short:  ", sum(is.na(PAX_short$WLUNA_close)),  "\n") # some NAs
cat("USDC full:  ", sum(is.na(USDC_full$WLUNA_close)),  "\n") # some NAs
cat("USDC short: ", sum(is.na(USDC_short$WLUNA_close)), "\n") # some NAs
cat("USDT full:  ", sum(is.na(USDT_full$WLUNA_close)),  "\n") # some NAs
cat("USDT short: ", sum(is.na(USDT_short$WLUNA_close)), "\n") # some NAs
cat("UST full:   ", sum(is.na(UST_full$WLUNA_close)),   "\n")
cat("UST short:  ", sum(is.na(UST_short$WLUNA_close)),  "\n")

# Fill WLUNA_close NAs with 1 for pre-launch period
# WLUNA did not exist before 2019-07-26, so no contagion risk during this period
# Filling with 1 (par value) is conservative and avoids data loss

DAI_full   <- DAI_full   %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))
DAI_short  <- DAI_short  %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))

PAX_full   <- PAX_full   %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))
PAX_short  <- PAX_short  %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))

USDC_full  <- USDC_full  %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))
USDC_short <- USDC_short %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))

USDT_full  <- USDT_full  %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))
USDT_short <- USDT_short %>% mutate(WLUNA_close = ifelse(is.na(WLUNA_close), 1, WLUNA_close))

# verify no NAs remain
cat("Remaining WLUNA_close NAs after fill:\n")
cat("PAX full:  ", sum(is.na(PAX_full$WLUNA_close)), "\n")
cat("USDC full: ", sum(is.na(USDC_full$WLUNA_close)), "\n")
cat("USDT full: ", sum(is.na(USDT_full$WLUNA_close)), "\n")

# -----------------------------------------------------------------------------
# Save updated datasets
# -----------------------------------------------------------------------------

write_csv(DAI_full,   "data/DAI/DAI_full_dataset.csv")
write_csv(DAI_short,  "data/DAI/DAI_short_dataset.csv")

write_csv(PAX_full,   "data/PAX/PAX_full_dataset.csv")
write_csv(PAX_short,  "data/PAX/PAX_short_dataset.csv")

write_csv(USDC_full,  "data/USDC/USDC_full_dataset.csv")
write_csv(USDC_short, "data/USDC/USDC_short_dataset.csv")

write_csv(USDT_full,  "data/USDT/USDT_full_dataset.csv")
write_csv(USDT_short, "data/USDT/USDT_short_dataset.csv")

write_csv(UST_full,   "data/UST/UST_full_dataset.csv")
write_csv(UST_short,  "data/UST/UST_short_dataset.csv")


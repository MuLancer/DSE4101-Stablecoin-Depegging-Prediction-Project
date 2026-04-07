# DSE4101-Stablecoin-Depegging-Prediction-Project

## Overview
This project builds a machine learning-based early warning system for predicting stablecoin depegging events. Binary depeg indicators are constructed across four forecast horizons (1, 3, 5, and 7 days ahead) for five stablecoins: DAI, USDC, USDT, PAX, and UST. Models evaluated include Random Forest, XGBoost (Gradient Boosting), Principal Components Regression (PCR), Partial Least Squares (PLS), Logistic Regression, and LASSO.
> **Note:** Only Project Phase 2 contains the final code and should be used for reproducing results.
> Project Phase 1 contains earlier exploratory work and can be skipped entirely.


## Project Phase 2 - Folder Structure
```
Project Phase 2/
│
├── code/                                              # Feature engineering pipeline (run in order)
│   ├── 1_combine_cmc.R  # Combines raw CoinMarketCap CSVs by coin
│   ├── 2_DAI_Prep.R        # Cleans data and constructs depeg labels for DAI
│   ├── 2_PAX_Prep.R       # Same for PAX
│   ├── 2_USDC_Prep.R    # Same for USDC
│   ├── 2_USDT_Prep.R    # Same for USDT
│   ├── 2_UST_Prep.R       # Same for UST
│   ├── 2_WLUNA_Prep.R # Cleans WLUNA; used as contagion feature
│   ├── 3_contagion_feature_adding.R  # Merges WLUNA price into all stablecoin datasets
│   ├── 5_technical_indicators.R  # Computes technical features like RSI, Bollinger Bands etc.
│   ├── 6_fear_and_greed.R   # Fetches Fear & Greed Index from API; saves to CSV
│   ├── 7_onchain_data_transformation_API.R   # Extracts on-chain metrics from Google BigQuery (see warning)
│   ├── 8_add_fng_and_onchain_indicators.R   # Merges all features into final per-coin CSVs
│   ├── 10_rf_gb_feature_imp_analysis.R  # Uses importance CSVs and prints top features
│   ├── 0_animals_in_trading.R            # (Optional EDA) Whale/shark wallet activity plots
│   ├── 0.5_animals_in_trading.ipynb   # (Optional EDA) Jupyter notebook version of above
│   ├── 4_stablecoin_datasets_exploration.R    # (Optional EDA) Exploratory data analysis
│   └── 9_feature_analysis(visualisations).R  # (Optional EDA) Feature visualisation plots
│
├── models/
│   └── code/
│       ├── 1_final_data_prep.R # Recomputes depeg labels in <coin>_onchain_features.csv
│       ├── v3_model.R                         # MAIN script: run this to reproduce all results
│       ├── v1_model.R                         # Earlier version (kept for reference)
│       ├── v2_model.R                         # Earlier version containing rolling window (kept for reference)
│       ├── model_results_plotting.R  # Reads summary_main.csv; run to generate performance plots
│       ├── func-rf.R                            # Random Forest model function
│       ├── func-gb.R                          # XGBoost model function
│       ├── func-pcr.R                         # PCR model function
│       ├── func-pls.R                         # PLS model function
│       ├── func-logit.R                       # Logistic Regression model function
│       ├── func-lasso.R                     # LASSO model function (requires glmnet)
│       ├── func-feature_importance_logloss.R  # Log-loss permutation feature importance
│       ├── func-depeg_metrics.R               # Computes all evaluation metrics
│       ├── func-plots.R                       # Prediction timeline and ROC/AUC plots
│       └── func-rf_roll.R                     # Rolling-window RF (used in v2; reference only)
│
├── model performance output/
│   ├── summary_main.csv                       # Final metrics table (written by v3_model.R)
│   └── archive/                               # Earlier versions of metrics output
│
├── data/
│   ├── CoinMarketCap pulled data/      # Raw OHLCV CSVs from CMC (2018-2025, per coin per year)
│   ├── Dai/             # All DAI datasets (combined, full, short, technical, onchain)
│   ├── PAX/           # Same structure for PAX
│   ├── USDC/        # Same structure for USDC
│   ├── USDT/        # Same structure for USDT
│   ├── UST/           # Same structure for UST
│   ├── WLUNA/     # WLUNA has only combined and full datasets (dropped otherwise)
│   ├── onchain_metrics_YYYY.csv    # Raw on-chain data per year (from BigQuery, 2018-2025)
│   ├── onchain_metrics_all.csv          # Concatenated on-chain data across all years
│   └── fear_and_greed_index_2.csv  # Fear & Greed Index time series (2018-2025) 
│
└── plots/
    ├── feature_importance/   # Feature importance PNGs (written by v3_model.R)
    ├── model performance/   # Model performance plots (written by model_results_plotting.R)
    ├── rf_logloss_importance_all.csv  # RF permutation importance data (written by v3_model.R)
    ├── gb_logloss_importance_all.csv  # GB permutation importance data (written by v3_model.R)
    └── ...    # Other EDA and visualisation plots
```

## Requirements
R version: 4.2 or later recommended.
Install all required packages by running this once in R:
```r
install.packages(c("tidyverse", "dplyr", "readr", "tidyr", "lubridate", "zoo", "TTR", "jsonlite", "randomForest", "xgboost", "pls", "glmnet", "smotefamily", "pROC", "ggplot2", "gridExtra", "patchwork", "scales","tibble", "purrr", "stringr", "forcats", "tidytext", "grid", "bigrquery", "glue"))
```
Notes:
- bigrquery and glue are only required if re-running 7_onchain_data_transformation_API.R. If using the pre-extracted on-chain CSV files in the data/ directory, these dependencies can be skipped.
- Re-running 7_onchain_data_transformation_API.R is not recommended, as it is highly time-intensive.
- glmnet is required for implementing the LASSO model in v3_model.R.

## How to Reproduce the Results
### *The short path (recommended)*
The final model-ready datasets (`[COIN]_onchain_features.csv`) are already included in the data/ subdirectories. To go straight to running the models, skip to **Step 2** below.
### *Step 1 - (Optional) Regenerate feature-engineered datasets from scratch*
Only needed if the `[COIN]_onchain_features.csv` files are missing or need to be rebuilt. 
Run the scripts in code/ in the numbered order below. All scripts expect the working directory set to Project Phase 2/.
| S | Script | What it does | Output |
|---|--------|--------------|--------|
|1|`1_combine_cmc.R`|Reads all raw CoinMarketCap CSVs and combines by coin|`data/[COIN]/[COIN]_combined.csv`|
|2|`[COIN]_Prep.R` (×6)|Cleans each coin's data; constructs depeg labels and dynamic thresholds (ThreshD/ThreshU) (Some wrong calculations here changed later but need to be run for code completeness)|`data/[COIN]/[COIN]_full_dataset.csv`, `[COIN]_short_dataset.csv`|
|3|`3_contagion_feature_adding.R`|Left-joins WLUNA closing price into each stablecoin dataset as a contagion featuren|Updates `[COIN]_full_dataset.csv` and `[COIN]_short_dataset.csv`|
|4|`5_technical_indicators.R`|Computes RSI, MACD, Bollinger Bands, ATR, and other price-based features|`data/[COIN]/[COIN]_technical_features.csv`|
|5|`6_fear_and_greed.R`|Fetches the Crypto Fear & Greed Index from api.alternative.me (see warning below)|`data/fear_and_greed_index_2.csv`|
|6|`7_onchain_data_transformation_API.R`|Queries Google BigQuery for Ethereum on-chain metrics (see warning below)|`data/onchain_metrics_YYYY.csv (one file per year)`|
|7|`8_add_fng_and_onchain_indicators.R`|Merges technical features, Fear & Greed Index, and on-chain metrics into one dataset per coin|`data/[COIN]/[COIN]_onchain_features.csv`|

**Notes:**
- Scripts 0_animals_in_trading.R, 0.5_animals_in_trading.ipynb, 4_stablecoin_datasets_exploration.R, and 9_feature_analysis(visualisations).R are optional EDA/visualisation scripts that do not affect any model inputs and can be skipped.
- Crypto Fear and Greed Index warning (6_fear_and_greed.R): This script queries FnG index until present time so every time it is rerun, the output: fear_and_greed_index_2.csv is a bit bigger but because of the data windows that are being used in the project, the newer indices are not needed. Recommendation is to skip running this file and use fear_and_greed_index_2.csv that is already there.
- BigQuery warning (7_onchain_data_transformation_API.R): This script queries the public Ethereum token transfers table on Google BigQuery and scans billions of records. Running all eight years will consume a large portion of the 1 TB free-tier quota. Do not run this unless the onchain_metrics_YYYY.csv files are missing from data/. The script has a file.exists() guard per year -> do not bypass it. A GCP project with billing enabled and bigrquery authenticated is required. The project ID is hardcoded as "dse4101" and must be updated to your own GCP project.

### *Step 2 - Recompute depeg labels (models/code/1_final_data_prep.R)*
This script redefines the binary depeg target variables across all four horizons using a rolling-window approach based on each coin's dynamic thresholds (ThreshD, ThreshU).
It reads from and overwrites the five `[COIN]_onchain_features.csv` files.
Set the working directory to Project Phase 2/ and run:
```r
setwd("path/to/Project Phase 2")
source("models/code/1_final_data_prep.R")
```
 If the `[COIN]_onchain_features.csv` files are already present with correctly defined depeg columns, this step can be skipped but running it first is recommended to ensure label consistency before modelling.

### *Step 3 -  Run all models (models/code/v3_model.R)*
v3_model.R is the main script.
Set the working directory to Project Phase 2/models/code/ and run:
```r
setwd("path/to/Project Phase 2/models/code")
source("v3_model.R")
```
The script will:
- Load the five `[COIN]_onchain_features.csv` datasets.
- Define two out-of-sample evaluation windows and apply SMOTE on each training set to address class imbalance:
  - Window 1 - Train: 2020-11-25 to 2021-11-25 | Test: 2021-11-26 to 2022-05-08 (stress-period window covering the UST collapse)
  - Window 2 - Train: 2019-11-22 to 2022-12-31 | Test: 2023-01-01 to 2025-12-31 (recent out-of-sample window; UST excluded)
- Train and evaluate six models across all coins and all four forecast horizons: Random Forest, XGBoost, PCR, PLS, Logistic Regression, and LASSO.
- Compute log-loss permutation feature importance for RF and GB (10 repeats, seed 123) and save results to:
  - `plots/rf_logloss_importance_all.csv`
  - `plots/gb_logloss_importance_all.csv`
  - `plots/feature_importance/[MODEL]_[COIN]_feature_importance_[WINDOW].png`
- Compile all evaluation metrics (accuracy, sensitivity, specificity, precision, recall, F1, AUC) into a single summary table and save to: `model performance output/summary_main.csv`
- The script runs sequentially across all coins, horizons, windows, and models. Expect a runtime of several minutes.

> **Note on warnings:** PCR and Logistic Regression may emit `glm.fit: fitted probabilities numerically 0 or 1 occurred.` This is expected and caused by small sample size and class imbalance when certain components predict the class perfectly in-sample. LASSO may emit convergence warnings for the least-regularised end of the path; the model selected at optimal lambda is not affected.

### *Step 4 - Generate model performance plots (models/code/model_results_plotting.R)*
This script reads `model performance output/summary_main.csv` (written in Step 3) and generates comparative performance plots across models, coins, and horizons. Run it after `v3_model.R` has completed.
```R
setwd("path/to/Project Phase 2/models/code")
source("model_results_plotting.R")
```
Plots are saved to `plots/model performance/`.

### *Step 5 - (Optional) Feature importance analysis (code/10_rf_gb_feature_imp_analysis.R)*
This script reads the saved importance CSVs and prints top-feature summaries per coin, window, and horizon. Run it after `v3_model.R`.
```R
setwd("path/to/Project Phase 2/models/code")
source("../../code/10_rf_gb_feature_imp_analysis.R")
```

## Key Configuration
| Parameter | Value | Notes |
|-----------|-------|-------|
| `set.seed` | 99 | Applied globally in `1_final_data_prep.R` and all `v*_model.R `scripts |
| Forecast horizons | `depeg_1d`, `depeg_3d`, `depeg_5d`, `depeg_7d` | Binary: 1 if price breaches ThreshD or ThreshU within the horizon |
| Depeg thresholds | `ThreshD`, `ThreshU` | Dynamic per-coin thresholds constructed in `2_[COIN]_Prep.R` |
| SMOTE | `dup_size = 1`, `nn = 3` | Applied to training sets only; not applied to test sets |
| RF hyperparameters | `ntree` , `mtry`, `nodesize` | Applied to each coin per forecast horizon, saved in `rfw1$[COIN]$[horizon]$tuning_params` or  `rfw2$[COIN]$[horizon]$tuning_params`|
| XGBoost hyperparameters | `eta` , `max_depth`, `subsample`, `colsample_bytree`, `nrounds` | Applied to each coin per forecast horizon, saved in `gbw1$[COIN]$[horizon]$tuning_params` or  `gbw2$[COIN]$[horizon]$tuning_params`. Features are z-score scaled using train-set statistics before training|
| Feature Importance | `n_repeats = 10`, `seed = 123` | Log-loss permutation importance via `func-feature_importance_logloss.R` |
| UST availability | Window 1 only | UST data ends 2022-05-08; excluded from Window 2 |


## Script Version Notes
Three model script versions are present in `models/code/`. 
Only `v3_model.R` should be used for reproducing final results:
- `v3_model.R` - Final version. Runs all six models; saves `summary_main.csv`, `feature importance CSVs`, and importance plots.
- `v1_model.R` - Earlier exploratory version with granular per-coin result blocks and inline metric inspection. Kept for reference.
- v2_model.R - Intermediate version using a rolling-window RF implementation. References some function files that may not be present (`func-gradboost.R`, `func-pcr_edited.R`); kept for reference only.


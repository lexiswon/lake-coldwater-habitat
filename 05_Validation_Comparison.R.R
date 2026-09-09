# ==============================================================================
# 05_Validation_Comparison.R
# Compares LOOCV vs 80/20 time split vs K-Fold (k=5 and k=10)
# All three models: PCR, Random Forest, XGBoost
# All five lakes
# ==============================================================================

rm(list = ls())
set.seed(42)

base_dir <- "~/Downloads/LakeData"

options(repos = c(CRAN = "https://cloud.r-project.org"),
        dplyr.summarise.inform = FALSE)

# --- Packages ---
need <- c("readr", "dplyr", "tidyr", "recipes", "workflows",
          "rsample", "yardstick", "parsnip", "ranger", "xgboost")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(recipes); library(workflows); library(rsample)
  library(yardstick); library(parsnip); library(ranger); library(xgboost)
})

# --- Load Data ---
dat <- read_csv(file.path(base_dir, "Final_Analysis_Ready_Data.csv"),
                show_col_types = FALSE)

outcome_col <- "CVHT_standardized"
year_col    <- "Year"
lake_col    <- "lakeid"

# --- Model Specs ---
pcr_spec <- linear_reg() |> set_engine("lm") |> set_mode("regression")

rf_spec <- rand_forest(trees = 1000, mtry = 3, min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

xgb_spec <- boost_tree(
  trees      = 500,
  learn_rate = 0.01,
  tree_depth = 3,
  min_n      = 10
) |>
  set_engine("xgboost") |>
  set_mode("regression")

# ==============================================================================
# Helper: fit and predict one train/test split
# ==============================================================================

fit_predict <- function(train_dat, test_dat, outcome_col,
                        predictor_cols, model_spec, use_pca = FALSE) {
  
  formula_str <- paste(outcome_col, "~",
                       paste(predictor_cols, collapse = " + "))
  
  if (use_pca) {
    rec <- recipe(as.formula(formula_str), data = train_dat) |>
      step_impute_median(all_predictors()) |>
      step_zv(all_predictors()) |>
      step_normalize(all_numeric_predictors()) |>
      step_pca(all_numeric_predictors(), num_comp = 3)
  } else {
    rec <- recipe(as.formula(formula_str), data = train_dat) |>
      step_impute_median(all_predictors()) |>
      step_zv(all_predictors()) |>
      step_normalize(all_numeric_predictors())
  }
  
  wf <- workflow() |> add_model(model_spec) |> add_recipe(rec)
  
  suppressMessages({
    fit <- wf |> fit(train_dat)
  })
  
  preds <- predict(fit, test_dat) |> pull(.pred)
  obs   <- test_dat[[outcome_col]]
  years <- test_dat[[year_col]]
  
  tibble(year     = years,
         observed = obs,
         predicted = preds)
}

# ==============================================================================
# Helper: calculate R2 from predictions
# ==============================================================================

calc_r2 <- function(df) {
  cor(df$observed, df$predicted, use = "complete.obs")^2
}

# ==============================================================================
# Main Lake Loop
# ==============================================================================

lake_list    <- unique(dat[[lake_col]])
all_results  <- list()

for (current_lake in lake_list) {
  
  cat(sprintf("\n========== Lake: %s ==========\n", current_lake))
  
  lake_dat <- dat |>
    filter(.data[[lake_col]] == current_lake) |>
    arrange(.data[[year_col]])
  
  predictor_cols <- setdiff(names(lake_dat),
                            c(outcome_col, year_col, lake_col,
                              "n_months_sampled"))
  
  n      <- nrow(lake_dat)
  n_train <- floor(0.8 * n)
  
  # ============================================================
  # METHOD 1: LOOCV
  # ============================================================
  
  cat("  Running LOOCV...\n")
  
  loocv_pcr <- loocv_rf <- loocv_xgb <- list()
  
  for (i in seq_len(n)) {
    train <- lake_dat[-i, ]
    test  <- lake_dat[i, , drop = FALSE]
    
    loocv_pcr[[i]] <- fit_predict(train, test, outcome_col,
                                  predictor_cols, pcr_spec, use_pca = TRUE)
    loocv_rf[[i]]  <- fit_predict(train, test, outcome_col,
                                  predictor_cols, rf_spec,  use_pca = FALSE)
    loocv_xgb[[i]] <- fit_predict(train, test, outcome_col,
                                  predictor_cols, xgb_spec, use_pca = FALSE)
  }
  
  r2_loocv_pcr <- calc_r2(bind_rows(loocv_pcr))
  r2_loocv_rf  <- calc_r2(bind_rows(loocv_rf))
  r2_loocv_xgb <- calc_r2(bind_rows(loocv_xgb))
  
  cat(sprintf("  LOOCV  — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_loocv_pcr, r2_loocv_rf, r2_loocv_xgb))
  
  # ============================================================
  # METHOD 2: 80/20 Time Split
  # ============================================================
  
  cat("  Running 80/20 time split...\n")
  
  train_8020 <- lake_dat[1:n_train, ]
  test_8020  <- lake_dat[(n_train + 1):n, ]
  
  pred_8020_pcr <- fit_predict(train_8020, test_8020, outcome_col,
                               predictor_cols, pcr_spec, use_pca = TRUE)
  pred_8020_rf  <- fit_predict(train_8020, test_8020, outcome_col,
                               predictor_cols, rf_spec,  use_pca = FALSE)
  pred_8020_xgb <- fit_predict(train_8020, test_8020, outcome_col,
                               predictor_cols, xgb_spec, use_pca = FALSE)
  
  r2_8020_pcr <- calc_r2(pred_8020_pcr)
  r2_8020_rf  <- calc_r2(pred_8020_rf)
  r2_8020_xgb <- calc_r2(pred_8020_xgb)
  
  cat(sprintf("  80/20  — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_8020_pcr, r2_8020_rf, r2_8020_xgb))
  cat(sprintf("  Train: %d-%d (%d yrs) | Test: %d-%d (%d yrs)\n",
              min(train_8020[[year_col]]), max(train_8020[[year_col]]),
              nrow(train_8020),
              min(test_8020[[year_col]]), max(test_8020[[year_col]]),
              nrow(test_8020)))
  
  # ============================================================
  # METHOD 3: K-Fold (k=5)
  # ============================================================
  
  cat("  Running 5-Fold CV...\n")
  
  k5      <- 5
  fold_ids <- cut(seq_len(n), breaks = k5, labels = FALSE)
  
  kfold5_pcr <- kfold5_rf <- kfold5_xgb <- list()
  
  for (k in 1:k5) {
    train_k <- lake_dat[fold_ids != k, ]
    test_k  <- lake_dat[fold_ids == k, ]
    
    kfold5_pcr[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                   predictor_cols, pcr_spec, use_pca = TRUE)
    kfold5_rf[[k]]  <- fit_predict(train_k, test_k, outcome_col,
                                   predictor_cols, rf_spec,  use_pca = FALSE)
    kfold5_xgb[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                   predictor_cols, xgb_spec, use_pca = FALSE)
  }
  
  r2_kfold5_pcr <- calc_r2(bind_rows(kfold5_pcr))
  r2_kfold5_rf  <- calc_r2(bind_rows(kfold5_rf))
  r2_kfold5_xgb <- calc_r2(bind_rows(kfold5_xgb))
  
  cat(sprintf("  5-Fold — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_kfold5_pcr, r2_kfold5_rf, r2_kfold5_xgb))
  
  # ============================================================
  # METHOD 4: K-Fold (k=10)
  # ============================================================
  
  cat("  Running 10-Fold CV...\n")
  
  k10      <- 10
  fold_ids10 <- cut(seq_len(n), breaks = k10, labels = FALSE)
  
  kfold10_pcr <- kfold10_rf <- kfold10_xgb <- list()
  
  for (k in 1:k10) {
    train_k <- lake_dat[fold_ids10 != k, ]
    test_k  <- lake_dat[fold_ids10 == k, ]
    
    kfold10_pcr[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, pcr_spec, use_pca = TRUE)
    kfold10_rf[[k]]  <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, rf_spec,  use_pca = FALSE)
    kfold10_xgb[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, xgb_spec, use_pca = FALSE)
  }
  
  r2_kfold10_pcr <- calc_r2(bind_rows(kfold10_pcr))
  r2_kfold10_rf  <- calc_r2(bind_rows(kfold10_rf))
  r2_kfold10_xgb <- calc_r2(bind_rows(kfold10_xgb))
  
  cat(sprintf("  10-Fold— PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_kfold10_pcr, r2_kfold10_rf, r2_kfold10_xgb))
  
  # ============================================================
  # Store all results
  # ============================================================
  
  all_results[[current_lake]] <- tibble(
    lake_id    = current_lake,
    validation = c("LOOCV", "LOOCV", "LOOCV",
                   "80/20", "80/20", "80/20",
                   "5-Fold", "5-Fold", "5-Fold",
                   "10-Fold", "10-Fold", "10-Fold"),
    model      = rep(c("PCR", "Random Forest", "XGBoost"), 4),
    R2         = round(c(r2_loocv_pcr,   r2_loocv_rf,   r2_loocv_xgb,
                         r2_8020_pcr,    r2_8020_rf,    r2_8020_xgb,
                         r2_kfold5_pcr,  r2_kfold5_rf,  r2_kfold5_xgb,
                         r2_kfold10_pcr, r2_kfold10_rf, r2_kfold10_xgb), 3)
  )
}

# ==============================================================================
# Summary Table
# ==============================================================================

results_df <- bind_rows(all_results)

cat("\n\n========== VALIDATION METHOD COMPARISON ==========\n")

# Wide format — one row per lake+model, columns per validation method
results_wide <- results_df %>%
  pivot_wider(names_from  = validation,
              values_from = R2) %>%
  arrange(lake_id, model)

print(results_wide)

# Also print by validation method for easy reading
cat("\n--- By Validation Method (BM only) ---\n")
print(results_df %>% filter(lake_id == "BM"))

# Save
write_csv(results_df,   file.path(base_dir, "Validation_Comparison_Long.csv"))
write_csv(results_wide, file.path(base_dir, "Validation_Comparison_Wide.csv"))

cat("\nWrote: Validation_Comparison_Long.csv\n")
cat("Wrote: Validation_Comparison_Wide.csv\n")
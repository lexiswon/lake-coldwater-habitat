# ==============================================================================
# 06_Validation_Comparison_Extended.R
# Adds Blocked K-Fold and Forward Chaining to existing validation comparison
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
  
  # need at least 5 training rows to fit reliably
  if (nrow(train_dat) < 5) return(NULL)
  
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
    fit <- tryCatch(wf |> fit(train_dat), error = function(e) NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  preds <- predict(fit, test_dat) |> pull(.pred)
  obs   <- test_dat[[outcome_col]]
  years <- test_dat[[year_col]]
  
  tibble(year      = years,
         observed  = obs,
         predicted = preds)
}

# ==============================================================================
# Helper: calculate R2
# ==============================================================================

calc_r2 <- function(df) {
  if (is.null(df) || nrow(df) < 3) return(NA_real_)
  r <- cor(df$observed, df$predicted, use = "complete.obs")
  round(r^2, 3)
}

# ==============================================================================
# Main Lake Loop
# ==============================================================================

lake_list   <- unique(dat[[lake_col]])
all_results <- list()

for (current_lake in lake_list) {
  
  cat(sprintf("\n========== Lake: %s ==========\n", current_lake))
  
  lake_dat <- dat |>
    filter(.data[[lake_col]] == current_lake) |>
    arrange(.data[[year_col]])
  
  predictor_cols <- setdiff(names(lake_dat),
                            c(outcome_col, year_col, lake_col,
                              "n_months_sampled"))
  
  n       <- nrow(lake_dat)
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
  
  r2_loocv <- c(
    PCR           = calc_r2(bind_rows(loocv_pcr)),
    `Random Forest` = calc_r2(bind_rows(loocv_rf)),
    XGBoost       = calc_r2(bind_rows(loocv_xgb))
  )
  cat(sprintf("  LOOCV       — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_loocv["PCR"], r2_loocv["Random Forest"], r2_loocv["XGBoost"]))
  
  # ============================================================
  # METHOD 2: 80/20 Time Split
  # ============================================================
  
  cat("  Running 80/20...\n")
  
  train_8020 <- lake_dat[1:n_train, ]
  test_8020  <- lake_dat[(n_train + 1):n, ]
  
  r2_8020 <- c(
    PCR           = calc_r2(fit_predict(train_8020, test_8020, outcome_col,
                                        predictor_cols, pcr_spec, TRUE)),
    `Random Forest` = calc_r2(fit_predict(train_8020, test_8020, outcome_col,
                                          predictor_cols, rf_spec,  FALSE)),
    XGBoost       = calc_r2(fit_predict(train_8020, test_8020, outcome_col,
                                        predictor_cols, xgb_spec, FALSE))
  )
  cat(sprintf("  80/20       — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_8020["PCR"], r2_8020["Random Forest"], r2_8020["XGBoost"]))
  
  # ============================================================
  # METHOD 3: 5-Fold CV
  # ============================================================
  
  cat("  Running 5-Fold...\n")
  
  fold_ids5   <- cut(seq_len(n), breaks = 5, labels = FALSE)
  kfold5_pcr  <- kfold5_rf <- kfold5_xgb <- list()
  
  for (k in 1:5) {
    train_k <- lake_dat[fold_ids5 != k, ]
    test_k  <- lake_dat[fold_ids5 == k, ]
    kfold5_pcr[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                   predictor_cols, pcr_spec, TRUE)
    kfold5_rf[[k]]  <- fit_predict(train_k, test_k, outcome_col,
                                   predictor_cols, rf_spec,  FALSE)
    kfold5_xgb[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                   predictor_cols, xgb_spec, FALSE)
  }
  
  r2_kfold5 <- c(
    PCR           = calc_r2(bind_rows(kfold5_pcr)),
    `Random Forest` = calc_r2(bind_rows(kfold5_rf)),
    XGBoost       = calc_r2(bind_rows(kfold5_xgb))
  )
  cat(sprintf("  5-Fold      — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_kfold5["PCR"], r2_kfold5["Random Forest"],
              r2_kfold5["XGBoost"]))
  
  # ============================================================
  # METHOD 4: 10-Fold CV
  # ============================================================
  
  cat("  Running 10-Fold...\n")
  
  fold_ids10   <- cut(seq_len(n), breaks = 10, labels = FALSE)
  kfold10_pcr  <- kfold10_rf <- kfold10_xgb <- list()
  
  for (k in 1:10) {
    train_k <- lake_dat[fold_ids10 != k, ]
    test_k  <- lake_dat[fold_ids10 == k, ]
    kfold10_pcr[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, pcr_spec, TRUE)
    kfold10_rf[[k]]  <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, rf_spec,  FALSE)
    kfold10_xgb[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, xgb_spec, FALSE)
  }
  
  r2_kfold10 <- c(
    PCR           = calc_r2(bind_rows(kfold10_pcr)),
    `Random Forest` = calc_r2(bind_rows(kfold10_rf)),
    XGBoost       = calc_r2(bind_rows(kfold10_xgb))
  )
  cat(sprintf("  10-Fold     — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_kfold10["PCR"], r2_kfold10["Random Forest"],
              r2_kfold10["XGBoost"]))
  
  # ============================================================
  # METHOD 5: Blocked K-Fold (k=5, consecutive year blocks)
  # ============================================================
  
  cat("  Running Blocked 5-Fold...\n")
  
  block_size  <- floor(n / 5)
  block_ids   <- c(rep(1:5, each = block_size),
                   rep(5, n - block_size * 5))[1:n]
  
  blocked_pcr <- blocked_rf <- blocked_xgb <- list()
  
  for (k in 1:5) {
    train_k <- lake_dat[block_ids != k, ]
    test_k  <- lake_dat[block_ids == k, ]
    
    cat(sprintf("    Block %d: train %d-%d + %d-%d | test %d-%d\n",
                k,
                min(train_k[[year_col]]),
                max(lake_dat[[year_col]][block_ids < k | block_ids == 1]),
                ifelse(k < 5,
                       min(lake_dat[[year_col]][block_ids > k]),
                       NA),
                ifelse(k < 5,
                       max(train_k[[year_col]]),
                       max(train_k[[year_col]])),
                min(test_k[[year_col]]),
                max(test_k[[year_col]])))
    
    blocked_pcr[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, pcr_spec, TRUE)
    blocked_rf[[k]]  <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, rf_spec,  FALSE)
    blocked_xgb[[k]] <- fit_predict(train_k, test_k, outcome_col,
                                    predictor_cols, xgb_spec, FALSE)
  }
  
  r2_blocked <- c(
    PCR           = calc_r2(bind_rows(blocked_pcr)),
    `Random Forest` = calc_r2(bind_rows(blocked_rf)),
    XGBoost       = calc_r2(bind_rows(blocked_xgb))
  )
  cat(sprintf("  Blocked     — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_blocked["PCR"], r2_blocked["Random Forest"],
              r2_blocked["XGBoost"]))
  
  # ============================================================
  # METHOD 6: Forward Chaining
  # ============================================================
  
  cat("  Running Forward Chaining...\n")
  
  min_train      <- 10
  forward_pcr    <- forward_rf <- forward_xgb <- list()
  fc_idx         <- 1
  
  for (i in min_train:(n - 1)) {
    train_k <- lake_dat[1:i, ]
    test_k  <- lake_dat[i + 1, , drop = FALSE]
    
    forward_pcr[[fc_idx]] <- fit_predict(train_k, test_k, outcome_col,
                                         predictor_cols, pcr_spec, TRUE)
    forward_rf[[fc_idx]]  <- fit_predict(train_k, test_k, outcome_col,
                                         predictor_cols, rf_spec,  FALSE)
    forward_xgb[[fc_idx]] <- fit_predict(train_k, test_k, outcome_col,
                                         predictor_cols, xgb_spec, FALSE)
    fc_idx <- fc_idx + 1
  }
  
  r2_forward <- c(
    PCR           = calc_r2(bind_rows(forward_pcr)),
    `Random Forest` = calc_r2(bind_rows(forward_rf)),
    XGBoost       = calc_r2(bind_rows(forward_xgb))
  )
  cat(sprintf("  Forward     — PCR: %.3f | RF: %.3f | XGB: %.3f\n",
              r2_forward["PCR"], r2_forward["Random Forest"],
              r2_forward["XGBoost"]))
  
  # ============================================================
  # Store results
  # ============================================================
  
  all_results[[current_lake]] <- tibble(
    lake_id    = current_lake,
    validation = c(rep("LOOCV",    3),
                   rep("80/20",    3),
                   rep("5-Fold",   3),
                   rep("10-Fold",  3),
                   rep("Blocked",  3),
                   rep("Forward",  3)),
    model      = rep(c("PCR", "Random Forest", "XGBoost"), 6),
    R2         = c(r2_loocv,   r2_8020,
                   r2_kfold5,  r2_kfold10,
                   r2_blocked, r2_forward)
  )
}

# ==============================================================================
# Summary Tables
# ==============================================================================

results_df   <- bind_rows(all_results)

results_wide <- results_df %>%
  pivot_wider(names_from  = validation,
              values_from = R2) %>%
  arrange(lake_id, model) %>%
  select(lake_id, model,
         LOOCV, `80/20`, `5-Fold`, `10-Fold`,
         Blocked, Forward)

cat("\n\n========== FULL VALIDATION COMPARISON ==========\n")
print(results_wide, n = 30)

cat("\n--- Big Muskellunge Only ---\n")
print(results_wide %>% filter(lake_id == "BM"))

cat("\n--- Which model wins each validation method? (BM) ---\n")
results_df %>%
  filter(lake_id == "BM") %>%
  group_by(validation) %>%
  slice_max(R2, n = 1) %>%
  select(validation, model, R2) %>%
  print()

# Save
write_csv(results_df,   file.path(base_dir, "Validation_Extended_Long.csv"))
write_csv(results_wide, file.path(base_dir, "Validation_Extended_Wide.csv"))

cat("\nSaved: Validation_Extended_Long.csv\n")
cat("Saved: Validation_Extended_Wide.csv\n")
cat("\nAll done!\n")


# ==============================================================================
# ==============================================================================
# Additional Model Specs — add these to your existing script
# ==============================================================================
# ==============================================================================

library(glmnet)   # Ridge, Lasso, Elastic Net
library(kernlab)  # SVR
library(pls)      # Partial Least Squares

# --- Ridge Regression (alpha = 0 means pure ridge penalty) ---
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) |>
  set_engine("glmnet") |>
  set_mode("regression")

# --- Lasso (alpha = 1 means pure lasso penalty) ---
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("regression")

# --- Elastic Net (alpha = 0.5 mix of ridge and lasso) ---
enet_spec <- linear_reg(penalty = 0.1, mixture = 0.5) |>
  set_engine("glmnet") |>
  set_mode("regression")

# --- Support Vector Regression ---
svr_spec <- svm_rbf(
  cost      = 1,
  rbf_sigma = 0.1
) |>
  set_engine("kernlab") |>
  set_mode("regression")

# --- Partial Least Squares ---
# PLS needs its own recipe — uses step_pls instead of step_pca
fit_predict_pls <- function(train_dat, test_dat, outcome_col,
                            predictor_cols, num_comp = 3) {
  
  if (nrow(train_dat) < 5) return(NULL)
  
  formula_str <- paste(outcome_col, "~",
                       paste(predictor_cols, collapse = " + "))
  
  rec <- recipe(as.formula(formula_str), data = train_dat) |>
    step_impute_median(all_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_pls(all_numeric_predictors(),
             outcome   = outcome_col,
             num_comp  = num_comp)
  
  wf <- workflow() |>
    add_model(linear_reg() |> set_engine("lm")) |>
    add_recipe(rec)
  
  suppressMessages({
    fit <- tryCatch(wf |> fit(train_dat), error = function(e) NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  preds <- predict(fit, test_dat) |> pull(.pred)
  
  tibble(year      = test_dat[[year_col]],
         observed  = test_dat[[outcome_col]],
         predicted = preds)
}

# ==============================================================================
# LOOCV for all new models on BM first to see if worth running all lakes
# ==============================================================================

cat("\n========== NEW MODELS — BM LOOCV TEST ==========\n")

bm_dat <- dat |>
  filter(lakeid == "BM") |>
  arrange(Year)

predictor_cols <- setdiff(names(bm_dat),
                          c("CVHT_standardized", "Year",
                            "lakeid", "n_months_sampled"))

n <- nrow(bm_dat)

ridge_preds <- lasso_preds <- enet_preds <- list()
svr_preds   <- pls_preds   <- list()

for (i in seq_len(n)) {
  train <- bm_dat[-i, ]
  test  <- bm_dat[i, , drop = FALSE]
  
  ridge_preds[[i]] <- fit_predict(train, test, "CVHT_standardized",
                                  predictor_cols, ridge_spec, FALSE)
  lasso_preds[[i]] <- fit_predict(train, test, "CVHT_standardized",
                                  predictor_cols, lasso_spec, FALSE)
  enet_preds[[i]]  <- fit_predict(train, test, "CVHT_standardized",
                                  predictor_cols, enet_spec,  FALSE)
  svr_preds[[i]]   <- fit_predict(train, test, "CVHT_standardized",
                                  predictor_cols, svr_spec,   FALSE)
  pls_preds[[i]]   <- fit_predict_pls(train, test, "CVHT_standardized",
                                      predictor_cols, num_comp = 3)
}

results_new_bm <- tibble(
  model = c("PCR (baseline)",
            "Ridge",
            "Lasso",
            "Elastic Net",
            "SVR",
            "PLS"),
  R2_LOOCV = round(c(
    calc_r2(bind_rows(loocv_pcr)),
    calc_r2(bind_rows(ridge_preds)),
    calc_r2(bind_rows(lasso_preds)),
    calc_r2(bind_rows(enet_preds)),
    calc_r2(bind_rows(svr_preds)),
    calc_r2(bind_rows(pls_preds))
  ), 3)
) |>
  arrange(desc(R2_LOOCV))

cat("\nBig Muskellunge LOOCV R² — all models:\n")
print(results_new_bm)

# ==============================================================================
# If any new model beats PCR run it on all 5 lakes
# ==============================================================================

best_new <- results_new_bm |>
  filter(model != "PCR (baseline)") |>
  slice_max(R2_LOOCV, n = 1)

pcr_r2 <- results_new_bm$R2_LOOCV[results_new_bm$model == "PCR (baseline)"]

cat(sprintf("\nPCR baseline: %.3f\n", pcr_r2))
cat(sprintf("Best new model: %s (%.3f)\n",
            best_new$model, best_new$R2_LOOCV))

if (best_new$R2_LOOCV[1] > pcr_r2[1]) {
  cat("\n*** A new model beat PCR! Consider running on all 5 lakes. ***\n")
} else {
  cat("\nPCR still wins — consistent with previous findings.\n")
}

write_csv(results_new_bm,
          file.path(base_dir, "New_Models_BM_LOOCV.csv"))
cat("\nSaved: New_Models_BM_LOOCV.csv\n")
# ==============================================================================
# Fair Comparison — All models get PCA preprocessed predictors
# ==============================================================================

cat("\n========== FAIR COMPARISON — ALL MODELS WITH PCA ==========\n")

bm_dat <- dat |>
  filter(lakeid == "BM") |>
  arrange(Year)

predictor_cols <- setdiff(names(bm_dat),
                          c("CVHT_standardized", "Year",
                            "lakeid", "n_months_sampled"))

n <- nrow(bm_dat)

# All models now use PCA preprocessing
models_list <- list(
  "PCR"         = list(spec = pcr_spec,   pca = TRUE),
  "Ridge+PCA"   = list(spec = ridge_spec, pca = TRUE),
  "Lasso+PCA"   = list(spec = lasso_spec, pca = TRUE),
  "ENet+PCA"    = list(spec = enet_spec,  pca = TRUE),
  "RF+PCA"      = list(spec = rf_spec,    pca = TRUE),
  "XGB+PCA"     = list(spec = xgb_spec,   pca = TRUE),
  "SVR+PCA"     = list(spec = svr_spec,   pca = TRUE)
)

# Also run without PCA for direct comparison
models_no_pca <- list(
  "RF (raw)"    = list(spec = rf_spec,    pca = FALSE),
  "XGB (raw)"   = list(spec = xgb_spec,   pca = FALSE),
  "Ridge (raw)" = list(spec = ridge_spec, pca = FALSE),
  "Lasso (raw)" = list(spec = lasso_spec, pca = FALSE),
  "SVR (raw)"   = list(spec = svr_spec,   pca = FALSE)
)

all_models <- c(models_list, models_no_pca)

# Run LOOCV for all models
results_fair <- list()

for (model_name in names(all_models)) {
  cat(sprintf("  Running LOOCV: %s...\n", model_name))
  
  spec    <- all_models[[model_name]]$spec
  use_pca <- all_models[[model_name]]$pca
  
  preds_list <- list()
  
  for (i in seq_len(n)) {
    train <- bm_dat[-i, ]
    test  <- bm_dat[i, , drop = FALSE]
    
    preds_list[[i]] <- fit_predict(train, test,
                                   "CVHT_standardized",
                                   predictor_cols,
                                   spec,
                                   use_pca = use_pca)
  }
  
  r2_val <- calc_r2(bind_rows(preds_list))
  
  results_fair[[model_name]] <- tibble(
    model      = model_name,
    has_pca    = use_pca,
    R2_LOOCV   = round(r2_val, 3)
  )
  
  cat(sprintf("    R² = %.3f\n", r2_val))
}

# Summary table
results_fair_df <- bind_rows(results_fair) |>
  arrange(desc(R2_LOOCV))

cat("\n========== RESULTS: FAIR PCA COMPARISON (BM LOOCV) ==========\n")
cat("\nWith PCA preprocessing:\n")
results_fair_df |>
  filter(has_pca) |>
  print()

cat("\nWithout PCA preprocessing:\n")
results_fair_df |>
  filter(!has_pca) |>
  print()

cat("\nFull ranking:\n")
print(results_fair_df, n = 20)

# ==============================================================================
# Interpretation
# ==============================================================================

pcr_r2    <- results_fair_df$R2_LOOCV[results_fair_df$model == "PCR"]
best_pca  <- results_fair_df |>
  filter(has_pca, model != "PCR") |>
  slice_max(R2_LOOCV, n = 1)
best_raw  <- results_fair_df |>
  filter(!has_pca) |>
  slice_max(R2_LOOCV, n = 1)

cat(sprintf("\nPCR:                    %.3f\n", pcr_r2))
cat(sprintf("Best model with PCA:    %s (%.3f)\n",
            best_pca$model[1], best_pca$R2_LOOCV[1]))
cat(sprintf("Best model without PCA: %s (%.3f)\n",
            best_raw$model[1], best_raw$R2_LOOCV[1]))

if (best_pca$R2_LOOCV[1] > pcr_r2) {
  cat("\n*** A model WITH PCA beat PCR! The preprocessing helps but the model matters too. ***\n")
} else if (best_raw$R2_LOOCV[1] > pcr_r2) {
  cat("\n*** A model WITHOUT PCA beat PCR! Raw predictors work better for that model. ***\n")
} else {
  cat("\nPCR still wins — PCA compression + linear regression is the optimal combination.\n")
  cat("This confirms the PCA step AND the linear model are both contributing to PCR's advantage.\n")
}

write_csv(results_fair_df,
          file.path(base_dir, "Fair_PCA_Comparison_BM.csv"))
cat("\nSaved: Fair_PCA_Comparison_BM.csv\n")

# ==============================================================================
# No PCA Comparison — All models with raw predictors only
# ==============================================================================

cat("\n========== NO PCA — ALL MODELS RAW PREDICTORS (BM LOOCV) ==========\n")

bm_dat <- dat |>
  filter(lakeid == "BM") |>
  arrange(Year)

predictor_cols <- setdiff(names(bm_dat),
                          c("CVHT_standardized", "Year",
                            "lakeid", "n_months_sampled"))

n <- nrow(bm_dat)

# All models with raw predictors — no PCA at all
models_raw <- list(
  "PCR (no PCA)"      = list(spec = pcr_spec,   pca = FALSE),
  "Ridge (no PCA)"    = list(spec = ridge_spec,  pca = FALSE),
  "Lasso (no PCA)"    = list(spec = lasso_spec,  pca = FALSE),
  "ENet (no PCA)"     = list(spec = enet_spec,   pca = FALSE),
  "RF (no PCA)"       = list(spec = rf_spec,     pca = FALSE),
  "XGB (no PCA)"      = list(spec = xgb_spec,    pca = FALSE),
  "SVR (no PCA)"      = list(spec = svr_spec,    pca = FALSE)
)

results_raw <- list()

for (model_name in names(models_raw)) {
  cat(sprintf("  Running LOOCV: %s...\n", model_name))
  
  spec    <- models_raw[[model_name]]$spec
  use_pca <- models_raw[[model_name]]$pca
  
  preds_list <- list()
  
  for (i in seq_len(n)) {
    train <- bm_dat[-i, ]
    test  <- bm_dat[i, , drop = FALSE]
    
    preds_list[[i]] <- fit_predict(train, test,
                                   "CVHT_standardized",
                                   predictor_cols,
                                   spec,
                                   use_pca = use_pca)
  }
  
  r2_val <- calc_r2(bind_rows(preds_list))
  
  results_raw[[model_name]] <- tibble(
    model    = model_name,
    R2_LOOCV = round(r2_val, 3)
  )
  
  cat(sprintf("    R² = %.3f\n", r2_val))
}

# Summary table
results_raw_df <- bind_rows(results_raw) |>
  arrange(desc(R2_LOOCV))

cat("\n========== RESULTS: ALL MODELS NO PCA (BM LOOCV) ==========\n")
print(results_raw_df, n = 20)

# Compare against PCA versions
cat("\n========== PCA vs NO PCA SIDE BY SIDE ==========\n")

comparison_df <- tibble(
  model     = c("PCR/Linear", "Ridge", "Lasso",
                "Elastic Net", "RF", "XGBoost", "SVR"),
  with_PCA  = c(
    results_fair_df$R2_LOOCV[results_fair_df$model == "PCR"],
    results_fair_df$R2_LOOCV[results_fair_df$model == "Ridge+PCA"],
    results_fair_df$R2_LOOCV[results_fair_df$model == "Lasso+PCA"],
    results_fair_df$R2_LOOCV[results_fair_df$model == "ENet+PCA"],
    results_fair_df$R2_LOOCV[results_fair_df$model == "RF+PCA"],
    results_fair_df$R2_LOOCV[results_fair_df$model == "XGB+PCA"],
    results_fair_df$R2_LOOCV[results_fair_df$model == "SVR+PCA"]
  ),
  without_PCA = c(
    results_raw_df$R2_LOOCV[results_raw_df$model == "PCR (no PCA)"],
    results_raw_df$R2_LOOCV[results_raw_df$model == "Ridge (no PCA)"],
    results_raw_df$R2_LOOCV[results_raw_df$model == "Lasso (no PCA)"],
    results_raw_df$R2_LOOCV[results_raw_df$model == "ENet (no PCA)"],
    results_raw_df$R2_LOOCV[results_raw_df$model == "RF (no PCA)"],
    results_raw_df$R2_LOOCV[results_raw_df$model == "XGB (no PCA)"],
    results_raw_df$R2_LOOCV[results_raw_df$model == "SVR (no PCA)"]
  )
) |>
  mutate(PCA_gain = round(with_PCA - without_PCA, 3)) |>
  arrange(desc(with_PCA))

print(comparison_df)

# Winner
best_raw_model <- results_raw_df |> slice_max(R2_LOOCV, n = 1)
cat(sprintf("\nBest model WITHOUT PCA: %s (R² = %.3f)\n",
            best_raw_model$model[1],
            best_raw_model$R2_LOOCV[1]))

cat(sprintf("Best model WITH PCA:    PCR (R² = %.3f)\n",
            results_fair_df$R2_LOOCV[results_fair_df$model == "PCR"]))

write_csv(results_raw_df,   file.path(base_dir, "NoPCA_Comparison_BM.csv"))
write_csv(comparison_df,    file.path(base_dir, "PCA_vs_NoPCA_Comparison.csv"))

cat("\nSaved: NoPCA_Comparison_BM.csv\n")
cat("Saved: PCA_vs_NoPCA_Comparison.csv\n")



# ==============================================================================
# 02_Deterministic_Models.R
# PCR, Random Forest, and XGBoost with LOOCV — Lake Loop
# KEY FIX: Tree models (RF, XGB) use raw features. Only PCR uses PCA.
# ==============================================================================

rm(list = ls())  # Always start fresh
set.seed(42)

base_dir <- "~/Downloads/LakeData"

options(repos = c(CRAN = "https://cloud.r-project.org"),
        dplyr.summarise.inform = FALSE)

# --- Packages ---
need <- c("readr","dplyr","tidyr","janitor","recipes","workflows",
          "rsample","yardstick","parsnip","ranger","xgboost")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(recipes); library(workflows); library(rsample); library(yardstick)
  library(parsnip); library(ranger); library(xgboost)
})

# --- User Inputs ---
data_path   <- file.path(base_dir, "Final_Analysis_Ready_Data.csv")
outcome_col <- "CVHT_standardized"
year_col    <- "Year"
lake_col    <- "lakeid"

# --- Load Data ---
stopifnot(file.exists(path.expand(data_path)))
dat <- read_csv(data_path, show_col_types = FALSE)

if (!(outcome_col %in% names(dat))) {
  stop(sprintf("Outcome column '%s' not found. Check your CSV headers.", outcome_col))
}

# ==============================================================================
# --- Define Models ---
# ==============================================================================

# PCR: Linear regression — PCA makes sense here
pcr_spec <- linear_reg() |> set_engine("lm") |> set_mode("regression")

# Random Forest: Tree-based, does NOT want PCA
rf_spec <- rand_forest(trees = 1000, mtry = 3, min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

# XGBoost: Tuned for small N (44 years). Shallower trees prevent overfitting.
xgb_spec <- boost_tree(
  trees      = 500,
  learn_rate = 0.01,
  tree_depth = 3,
  min_n      = 10,
  loss_reduction = 0.1
) |>
  set_engine("xgboost") |>
  set_mode("regression")

# ==============================================================================
# --- Lake Loop ---
# ==============================================================================

lake_list    <- unique(dat[[lake_col]])
results_list <- list()

for (current_lake in lake_list) {
  
  cat(sprintf("\n--- Processing Lake: %s ---\n", current_lake))
  
  lake_dat <- dat |> filter(.data[[lake_col]] == current_lake)
  
  if (nrow(lake_dat) < 10) {
    cat(sprintf("  Skipping %s — too few rows (%d)\n", current_lake, nrow(lake_dat)))
    next
  }
  
  # Remove n_months_sampled — data quality flag, not a real predictor
  predictor_cols <- setdiff(names(lake_dat),
                            c(outcome_col, year_col, lake_col, "n_months_sampled"))
  
  if (!is.numeric(lake_dat[[outcome_col]])) {
    stop(sprintf("Outcome '%s' must be numeric.", outcome_col))
  }
  
  # ============================================================
  # TWO SEPARATE RECIPES — this is the key fix
  # rec_pcr: PCA for the linear model only
  # rec_tree: Raw normalized features for RF and XGBoost
  # ============================================================
  
  rec_pcr <- recipe(
    as.formula(paste(outcome_col, "~", paste(predictor_cols, collapse = " + "))),
    data = lake_dat
  ) |>
    step_impute_median(all_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_pca(all_numeric_predictors(), num_comp = 3)
  
  rec_tree <- recipe(
    as.formula(paste(outcome_col, "~", paste(predictor_cols, collapse = " + "))),
    data = lake_dat
  ) |>
    step_impute_median(all_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
  # NO PCA — trees see all raw features
  
  wf_pcr <- workflow() |> add_model(pcr_spec) |> add_recipe(rec_pcr)
  wf_rf  <- workflow() |> add_model(rf_spec)  |> add_recipe(rec_tree)
  wf_xgb <- workflow() |> add_model(xgb_spec) |> add_recipe(rec_tree)
  
  # --- Leave-One-Out Cross Validation ---
  lake_dat$pred_pcr <- NA_real_
  lake_dat$pred_rf  <- NA_real_
  lake_dat$pred_xgb <- NA_real_
  
  for (i in seq_len(nrow(lake_dat))) {
    train_dat <- lake_dat[-i, ]
    test_dat  <- lake_dat[i, , drop = FALSE]
    
    suppressMessages({
      fit_pcr <- wf_pcr |> fit(train_dat)
      fit_rf  <- wf_rf  |> fit(train_dat)
      fit_xgb <- wf_xgb |> fit(train_dat)
    })
    
    lake_dat$pred_pcr[i] <- predict(fit_pcr, test_dat) |> pull(.pred)
    lake_dat$pred_rf[i]  <- predict(fit_rf,  test_dat) |> pull(.pred)
    lake_dat$pred_xgb[i] <- predict(fit_xgb, test_dat) |> pull(.pred)
  }
  
  cat(sprintf("  Finished LOOCV for %s (%d folds)\n", current_lake, nrow(lake_dat)))
  
  results_list[[current_lake]] <- tibble(
    lake_id       = lake_dat[[lake_col]],
    year          = lake_dat[[year_col]],
    observed_cvht = lake_dat[[outcome_col]],
    pred_pcr      = lake_dat$pred_pcr,
    pred_rf       = lake_dat$pred_rf,
    pred_xgb      = lake_dat$pred_xgb
  )
}

# ==============================================================================
# --- Bind and Export ---
# ==============================================================================

export_all <- bind_rows(results_list)

write_csv(export_all, file.path(base_dir, "cvht_obs_preds_all_models.csv"))
write_csv(select(export_all, lake_id, year, observed_cvht, pred_pcr),
          file.path(base_dir, "cvht_obs_pred_pcr.csv"))
write_csv(select(export_all, lake_id, year, observed_cvht, pred_rf),
          file.path(base_dir, "cvht_obs_pred_rf.csv"))
write_csv(select(export_all, lake_id, year, observed_cvht, pred_xgb),
          file.path(base_dir, "cvht_obs_pred_xgb.csv"))

cat("\nAll CSVs written.\n")

# ==============================================================================
# --- Performance Metrics ---
# ==============================================================================

reg_metrics <- yardstick::metric_set(rsq, rmse, mae)

scores <- bind_rows(
  export_all |> group_by(lake_id) |>
    reg_metrics(truth = observed_cvht, estimate = pred_pcr) |>
    mutate(model = "PCR"),
  export_all |> group_by(lake_id) |>
    reg_metrics(truth = observed_cvht, estimate = pred_rf) |>
    mutate(model = "Random Forest"),
  export_all |> group_by(lake_id) |>
    reg_metrics(truth = observed_cvht, estimate = pred_xgb) |>
    mutate(model = "XGBoost")
) |>
  select(lake_id, model, .metric, .estimate) |>
  arrange(lake_id, model, .metric)

print(scores)
write_csv(scores, file.path(base_dir, "cvht_model_scores.csv"))

# ==============================================================================
# --- Poster Metrics Table ---
# pmax applied consistently to ALL models
# ==============================================================================

n              <- 44
df_crit        <- n - 2
alpha          <- 0.05
critical_value <- qt(1 - alpha/2, df_crit)
corr_threshold <- critical_value / sqrt(critical_value^2 + df_crit)
cat(sprintf("\nSignificance threshold (r): %.4f\n", corr_threshold))

poster_metrics <- export_all |>
  mutate(
    pred_pcr_c = pmax(pred_pcr, 0),
    pred_rf_c  = pmax(pred_rf,  0),
    pred_xgb_c = pmax(pred_xgb, 0)
  ) |>
  group_by(lake_id) |>
  summarise(
    PCR_R2  = round(cor(observed_cvht, pred_pcr_c,  use = "complete.obs")^2, 3),
    RF_R2   = round(cor(observed_cvht, pred_rf_c,   use = "complete.obs")^2, 3),
    XGB_R2  = round(cor(observed_cvht, pred_xgb_c,  use = "complete.obs")^2, 3),
    .groups = "drop"
  ) |>
  mutate(
    Best_Model = case_when(
      XGB_R2 >= PCR_R2 & XGB_R2 >= RF_R2 ~ "XGBoost",
      RF_R2  >= PCR_R2 & RF_R2  >= XGB_R2 ~ "Random Forest",
      TRUE ~ "PCR"
    )
  )

print("--- POSTER METRICS ---")
print(poster_metrics)
write_csv(poster_metrics, file.path(base_dir, "Poster_Final_Metrics.csv"))
cat("Wrote: Poster_Final_Metrics.csv\n")

# ==============================================================================
# --- Significance Check ---
# ==============================================================================

sig_check <- export_all %>%
  group_by(lake_id) %>%
  summarise(
    cor_pcr = cor(pred_pcr, observed_cvht, use = "complete.obs"),
    cor_rf  = cor(pred_rf,  observed_cvht, use = "complete.obs"),
    cor_xgb = cor(pred_xgb, observed_cvht, use = "complete.obs")
  ) %>%
  pivot_longer(cols = -lake_id,
               names_to = "model",
               values_to = "correlation") %>%
  mutate(is_significant = abs(correlation) > corr_threshold)

print(sig_check)
write_csv(sig_check, file.path(base_dir, "Predictor_Significance_Check.csv"))
cat("Wrote: Predictor_Significance_Check.csv\n")
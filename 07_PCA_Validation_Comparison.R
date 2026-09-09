# ==============================================================================
# 07_PCA_Validation_Comparison.R
# All models x PCA/no PCA x all validation methods
# Big Muskellunge Lake
# ==============================================================================

rm(list = ls())
set.seed(42)

base_dir <- "~/Downloads/LakeData"

options(repos = c(CRAN = "https://cloud.r-project.org"),
        dplyr.summarise.inform = FALSE)

# --- Packages ---
need <- c("readr", "dplyr", "tidyr", "recipes", "workflows",
          "rsample", "yardstick", "parsnip", "ranger",
          "xgboost", "glmnet", "kernlab")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(recipes); library(workflows); library(rsample)
  library(yardstick); library(parsnip); library(ranger)
  library(xgboost); library(glmnet); library(kernlab)
})

# --- Load Data ---
dat <- read_csv(file.path(base_dir, "Final_Analysis_Ready_Data.csv"),
                show_col_types = FALSE)

outcome_col <- "CVHT_standardized"
year_col    <- "Year"
lake_col    <- "lakeid"

# --- Model Specs ---
pcr_spec <- linear_reg() |> set_engine("lm") |> set_mode("regression")

ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) |>
  set_engine("glmnet") |> set_mode("regression")

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) |>
  set_engine("glmnet") |> set_mode("regression")

enet_spec <- linear_reg(penalty = 0.1, mixture = 0.5) |>
  set_engine("glmnet") |> set_mode("regression")

rf_spec <- rand_forest(trees = 1000, mtry = 3, min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

xgb_spec <- boost_tree(
  trees = 500, learn_rate = 0.01,
  tree_depth = 3, min_n = 10
) |>
  set_engine("xgboost") |> set_mode("regression")

svr_spec <- svm_rbf(cost = 1, rbf_sigma = 0.1) |>
  set_engine("kernlab") |> set_mode("regression")

# ==============================================================================
# Helper functions
# ==============================================================================

fit_predict <- function(train_dat, test_dat, outcome_col,
                        predictor_cols, model_spec, use_pca = FALSE) {
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
  
  tibble(year      = test_dat[[year_col]],
         observed  = test_dat[[outcome_col]],
         predicted = preds)
}

calc_r2 <- function(df) {
  if (is.null(df) || nrow(df) < 3) return(NA_real_)
  round(cor(df$observed, df$predicted, use = "complete.obs")^2, 3)
}

run_loocv <- function(lake_dat, outcome_col, predictor_cols,
                      spec, use_pca) {
  n     <- nrow(lake_dat)
  preds <- list()
  for (i in seq_len(n)) {
    preds[[i]] <- fit_predict(lake_dat[-i, ], lake_dat[i, , drop = FALSE],
                              outcome_col, predictor_cols, spec, use_pca)
  }
  calc_r2(bind_rows(preds))
}

run_8020 <- function(lake_dat, outcome_col, predictor_cols,
                     spec, use_pca) {
  n       <- nrow(lake_dat)
  n_train <- floor(0.8 * n)
  calc_r2(fit_predict(lake_dat[1:n_train, ],
                      lake_dat[(n_train + 1):n, ],
                      outcome_col, predictor_cols, spec, use_pca))
}

run_kfold <- function(lake_dat, outcome_col, predictor_cols,
                      spec, use_pca, k = 5) {
  n        <- nrow(lake_dat)
  fold_ids <- cut(seq_len(n), breaks = k, labels = FALSE)
  preds    <- list()
  for (fold in 1:k) {
    preds[[fold]] <- fit_predict(lake_dat[fold_ids != fold, ],
                                 lake_dat[fold_ids == fold, ],
                                 outcome_col, predictor_cols, spec, use_pca)
  }
  calc_r2(bind_rows(preds))
}

run_blocked <- function(lake_dat, outcome_col, predictor_cols,
                        spec, use_pca, k = 5) {
  n          <- nrow(lake_dat)
  block_size <- floor(n / k)
  block_ids  <- c(rep(1:k, each = block_size),
                  rep(k, n - block_size * k))[1:n]
  preds      <- list()
  for (fold in 1:k) {
    preds[[fold]] <- fit_predict(lake_dat[block_ids != fold, ],
                                 lake_dat[block_ids == fold, ],
                                 outcome_col, predictor_cols, spec, use_pca)
  }
  calc_r2(bind_rows(preds))
}

run_forward <- function(lake_dat, outcome_col, predictor_cols,
                        spec, use_pca, min_train = 10) {
  n     <- nrow(lake_dat)
  preds <- list()
  idx   <- 1
  for (i in min_train:(n - 1)) {
    preds[[idx]] <- fit_predict(lake_dat[1:i, ],
                                lake_dat[i + 1, , drop = FALSE],
                                outcome_col, predictor_cols, spec, use_pca)
    idx <- idx + 1
  }
  calc_r2(bind_rows(preds))
}

# ==============================================================================
# Setup
# ==============================================================================

bm_dat <- dat |>
  filter(lakeid == "BM") |>
  arrange(Year)

predictor_cols <- setdiff(names(bm_dat),
                          c(outcome_col, year_col, lake_col,
                            "n_months_sampled"))

# All model + PCA combinations
model_configs <- list(
  list(name = "PCR",          spec = pcr_spec,   pca = TRUE),
  list(name = "Ridge+PCA",    spec = ridge_spec, pca = TRUE),
  list(name = "Lasso+PCA",    spec = lasso_spec, pca = TRUE),
  list(name = "ENet+PCA",     spec = enet_spec,  pca = TRUE),
  list(name = "RF+PCA",       spec = rf_spec,    pca = TRUE),
  list(name = "XGB+PCA",      spec = xgb_spec,   pca = TRUE),
  list(name = "SVR+PCA",      spec = svr_spec,   pca = TRUE),
  list(name = "RF (raw)",     spec = rf_spec,    pca = FALSE),
  list(name = "XGB (raw)",    spec = xgb_spec,   pca = FALSE),
  list(name = "Ridge (raw)",  spec = ridge_spec, pca = FALSE),
  list(name = "Lasso (raw)",  spec = lasso_spec, pca = FALSE),
  list(name = "SVR (raw)",    spec = svr_spec,   pca = FALSE),
  list(name = "Linear (raw)", spec = pcr_spec,   pca = FALSE)
)

validation_methods <- c("LOOCV", "80/20", "5-Fold",
                        "Blocked", "Forward")

# ==============================================================================
# Run everything
# ==============================================================================

cat("Running all models x all validation methods...\n")
cat("This will take a few minutes — grab a coffee!\n\n")

all_results <- list()

for (cfg in model_configs) {
  cat(sprintf("Model: %-15s | ", cfg$name))
  
  row_results <- tibble(model = cfg$name)
  
  # LOOCV
  r2 <- run_loocv(bm_dat, outcome_col, predictor_cols,
                  cfg$spec, cfg$pca)
  row_results$LOOCV <- r2
  cat(sprintf("LOOCV=%.3f | ", r2))
  
  # 80/20
  r2 <- run_8020(bm_dat, outcome_col, predictor_cols,
                 cfg$spec, cfg$pca)
  row_results$`80/20` <- r2
  cat(sprintf("80/20=%.3f | ", r2))
  
  # 5-Fold
  r2 <- run_kfold(bm_dat, outcome_col, predictor_cols,
                  cfg$spec, cfg$pca, k = 5)
  row_results$`5-Fold` <- r2
  cat(sprintf("5-Fold=%.3f | ", r2))
  
  # Blocked
  r2 <- run_blocked(bm_dat, outcome_col, predictor_cols,
                    cfg$spec, cfg$pca, k = 5)
  row_results$Blocked <- r2
  cat(sprintf("Blocked=%.3f | ", r2))
  
  # Forward
  r2 <- run_forward(bm_dat, outcome_col, predictor_cols,
                    cfg$spec, cfg$pca, min_train = 10)
  row_results$Forward <- r2
  cat(sprintf("Forward=%.3f\n", r2))
  
  all_results[[cfg$name]] <- row_results
}

# ==============================================================================
# Summary tables
# ==============================================================================

results_df <- bind_rows(all_results) |>
  mutate(avg_R2 = round(rowMeans(across(where(is.numeric)),
                                 na.rm = TRUE), 3)) |>
  arrange(desc(avg_R2))

cat("\n\n========== FULL RESULTS — ALL MODELS x ALL VALIDATION METHODS ==========\n")
print(results_df, n = 20)

cat("\n--- PCA models only (ranked by average R2) ---\n")
results_df |>
  filter(grepl("PCA|PCR", model)) |>
  print()

cat("\n--- Raw models only (ranked by average R2) ---\n")
results_df |>
  filter(grepl("raw|Linear", model)) |>
  print()

cat("\n--- Which model wins each validation method? ---\n")
results_df |>
  select(-avg_R2) |>
  pivot_longer(-model,
               names_to  = "validation",
               values_to = "R2") |>
  group_by(validation) |>
  slice_max(R2, n = 1) |>
  select(validation, model, R2) |>
  print()

# Save
write_csv(results_df,
          file.path(base_dir, "Full_Model_Validation_Comparison.csv"))
cat("\nSaved: Full_Model_Validation_Comparison.csv\n")
cat("All done!\n")
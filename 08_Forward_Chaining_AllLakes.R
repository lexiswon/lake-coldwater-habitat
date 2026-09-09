# ==============================================================================
# 08_Forward_Chaining_AllLakes.R
# Run top models under Forward Chaining on all 5 lakes
# Compare against LOOCV results
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

run_forward <- function(lake_dat, outcome_col, predictor_cols,
                        spec, use_pca, min_train = 10) {
  n     <- nrow(lake_dat)
  preds <- list()
  idx   <- 1
  for (i in min_train:(n - 1)) {
    preds[[idx]] <- fit_predict(lake_dat[1:i, ],
                                lake_dat[i + 1, , drop = FALSE],
                                outcome_col, predictor_cols,
                                spec, use_pca)
    idx <- idx + 1
  }
  calc_r2(bind_rows(preds))
}

# ==============================================================================
# Top models to test — based on BM results
# ==============================================================================

model_configs <- list(
  list(name = "PCR",       spec = pcr_spec,   pca = TRUE),
  list(name = "Lasso+PCA", spec = lasso_spec, pca = TRUE),
  list(name = "ENet+PCA",  spec = enet_spec,  pca = TRUE),
  list(name = "RF (raw)",  spec = rf_spec,    pca = FALSE)
)

lake_list <- c("AL", "BM", "CR", "SP", "TR")

lake_labels <- c(
  "AL" = "Allequash",
  "BM" = "Big Muskellunge",
  "CR" = "Crystal",
  "SP" = "Sparkling",
  "TR" = "Trout"
)

# ==============================================================================
# Run LOOCV and Forward Chaining for all lakes x top models
# ==============================================================================

all_results <- list()

for (current_lake in lake_list) {
  
  cat(sprintf("\n========== %s ==========\n",
              lake_labels[current_lake]))
  
  lake_dat <- dat |>
    filter(.data[[lake_col]] == current_lake) |>
    arrange(.data[[year_col]])
  
  predictor_cols <- setdiff(names(lake_dat),
                            c(outcome_col, year_col, lake_col,
                              "n_months_sampled"))
  
  for (cfg in model_configs) {
    cat(sprintf("  %-12s | ", cfg$name))
    
    r2_loocv   <- run_loocv(lake_dat, outcome_col, predictor_cols,
                            cfg$spec, cfg$pca)
    r2_forward <- run_forward(lake_dat, outcome_col, predictor_cols,
                              cfg$spec, cfg$pca, min_train = 10)
    
    cat(sprintf("LOOCV=%.3f | Forward=%.3f\n",
                r2_loocv, r2_forward))
    
    all_results[[paste(current_lake, cfg$name)]] <- tibble(
      lake_id    = current_lake,
      lake_name  = lake_labels[current_lake],
      model      = cfg$name,
      LOOCV      = r2_loocv,
      Forward    = r2_forward,
      difference = round(r2_forward - r2_loocv, 3)
    )
  }
}

# ==============================================================================
# Summary tables
# ==============================================================================

results_df <- bind_rows(all_results)

cat("\n\n========== LOOCV vs FORWARD CHAINING — ALL LAKES ==========\n")
print(results_df |> arrange(lake_id, model), n = 30)

cat("\n--- Which model wins Forward Chaining per lake? ---\n")
results_df |>
  group_by(lake_id) |>
  slice_max(Forward, n = 1) |>
  select(lake_name, model, LOOCV, Forward) |>
  print()

cat("\n--- Is Forward consistently higher than LOOCV? ---\n")
results_df |>
  group_by(model) |>
  summarise(
    avg_LOOCV      = round(mean(LOOCV,   na.rm = TRUE), 3),
    avg_Forward    = round(mean(Forward, na.rm = TRUE), 3),
    avg_difference = round(mean(difference, na.rm = TRUE), 3),
    forward_wins   = sum(Forward > LOOCV, na.rm = TRUE),
    out_of         = n()
  ) |>
  arrange(desc(avg_Forward)) |>
  print()

cat("\n--- Forward Chaining consistency check ---\n")
cat("If Forward >> LOOCV consistently, Forward may be optimistic\n")
cat("If Forward ≈ LOOCV, both methods are telling the same story\n\n")

results_df |>
  summarise(
    total_comparisons  = n(),
    forward_higher     = sum(Forward > LOOCV, na.rm = TRUE),
    loocv_higher       = sum(LOOCV > Forward, na.rm = TRUE),
    tied               = sum(Forward == LOOCV, na.rm = TRUE),
    avg_forward_gain   = round(mean(difference, na.rm = TRUE), 3)
  ) |>
  print()

# Save
write_csv(results_df,
          file.path(base_dir, "Forward_vs_LOOCV_AllLakes.csv"))
cat("\nSaved: Forward_vs_LOOCV_AllLakes.csv\n")
cat("All done!\n")

####################################
# Observed CVHT Values for heat map
####################################


library(readr)
library(dplyr)

base_dir <- "~/Downloads/LakeData"
dat <- read_csv(file.path(base_dir, "Final_Analysis_Ready_Data.csv"),
                show_col_types = FALSE)

dat %>%
  group_by(lakeid) %>%
  summarise(
    mean_CVHT = round(mean(CVHT_standardized, na.rm = TRUE), 2),
    sd_CVHT   = round(sd(CVHT_standardized,   na.rm = TRUE), 2),
    min_CVHT  = round(min(CVHT_standardized,  na.rm = TRUE), 2),
    max_CVHT  = round(max(CVHT_standardized,  na.rm = TRUE), 2),
    n         = n()
  ) %>%
  arrange(desc(mean_CVHT)) %>%
  print()
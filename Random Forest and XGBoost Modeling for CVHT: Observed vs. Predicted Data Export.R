# fit_rf_xgb_export.R
# Train RF & XGB on CVHT, export observed & predicted for Emma's probabilistic step

## --------- User inputs (EDIT THESE) ----------
data_path   <- "~/Downloads/CVHT_Predictors.csv"  # your CSV
outcome_col <- "CVHT"                              # numeric outcome column
year_col    <- "Year"                              # set to column name if present, else NULL
## ---------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"),
        dplyr.summarise.inform = FALSE)
set.seed(42)

# Packages
need <- c("readr","dplyr","tidyr","janitor","recipes","workflows",
          "rsample","yardstick","parsnip","ranger","xgboost")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(recipes); library(workflows); library(rsample); library(yardstick)
  library(parsnip); library(ranger); library(xgboost)
})

# Load data
stopifnot(file.exists(path.expand(data_path)))
dat0 <- read_csv(data_path, show_col_types = FALSE) |> clean_names()

outcome <- tolower(outcome_col)
yr_col  <- if (!is.null(year_col)) tolower(year_col) else NULL

if (!(outcome %in% names(dat0))) {
  stop(sprintf("Outcome '%s' not found. Available cols: %s",
               outcome, paste(head(names(dat0), 40), collapse=", ")))
}
dat <- dat0

# Extract Year or synthesize a sequence (needed for Emma's plot/timeline)
Years <- if (!is.null(yr_col) && yr_col %in% names(dat)) {
  dat[[yr_col]]
} else {
  seq_len(nrow(dat))
}

# Build modeling matrix:
# - keep all predictors except outcome and (optional) Year
predictor_cols <- setdiff(names(dat), c(outcome, yr_col))
# Quick sanity: ensure outcome numeric
if (!is.numeric(dat[[outcome]])) {
  stop(sprintf("Outcome '%s' must be numeric for these regressors.", outcome))
}

# Recipe: median impute, remove zero-variance, (no scaling needed for tree models)
rec <-
  recipe(as.formula(paste(outcome, "~", paste(predictor_cols, collapse = " + "))), data = dat) |>
  step_impute_median(all_predictors()) |>
  step_zv(all_predictors())

# Models
rf_spec  <- rand_forest(trees = 1000, mtry = min(8, length(predictor_cols)), min_n = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

xgb_spec <- boost_tree(
  trees = 1000, learn_rate = 0.05, tree_depth = 6, min_n = 5, loss_reduction = 0
) |>
  set_engine("xgboost", nthread = max(1, parallel::detectCores() - 1)) |>
  set_mode("regression")

# Workflows
wf_rf  <- workflow() |> add_model(rf_spec)  |> add_recipe(rec)
wf_xgb <- workflow() |> add_model(xgb_spec) |> add_recipe(rec)

# Fit on ALL rows (you want one prediction per observed value)
fit_rf  <- wf_rf  |> fit(dat)
fit_xgb <- wf_xgb |> fit(dat)

# Predict on all rows
pred_rf  <- predict(fit_rf,  dat) |> pull(.pred)
pred_xgb <- predict(fit_xgb, dat) |> pull(.pred)

# Export observed + predicted (one wide for convenience, two per-model for Emma)
export_all <- tibble(
  row_id        = seq_len(nrow(dat)),
  year          = Years,
  observed_cvht = dat[[outcome]],
  pred_rf       = pred_rf,
  pred_xgb      = pred_xgb
)

write_csv(export_all, "cvht_obs_preds_all_models.csv")
write_csv(select(export_all, row_id, year, observed_cvht, pred_rf),  "cvht_obs_pred_rf.csv")
write_csv(select(export_all, row_id, year, observed_cvht, pred_xgb), "cvht_obs_pred_xgb.csv")

cat("Wrote: cvht_obs_preds_all_models.csv, cvht_obs_pred_rf.csv, cvht_obs_pred_xgb.csv\n")

#  Metrics (R² / RMSE / MAE) vs. observed  --- FIXED
reg_metrics <- yardstick::metric_set(rsq, rmse, mae)

scores_rf <- reg_metrics(
  export_all,
  truth   = observed_cvht,
  estimate= pred_rf
) |>
  dplyr::mutate(model = "Random Forest")

scores_xgb <- reg_metrics(
  export_all,
  truth   = observed_cvht,
  estimate= pred_xgb
) |>
  dplyr::mutate(model = "XGBoost")

scores <- dplyr::bind_rows(scores_rf, scores_xgb) |>
  dplyr::select(model, .metric, .estimate)

print(scores)
readr::write_csv(scores, "cvht_model_scores.csv")
cat("Wrote: cvht_model_scores.csv\n")


# Optional: save fitted models
saveRDS(fit_rf,  "rf_fit_cvht.rds")
saveRDS(fit_xgb, "xgb_fit_cvht.rds")
cat("Saved models: rf_fit_cvht.rds, xgb_fit_cvht.rds\n")

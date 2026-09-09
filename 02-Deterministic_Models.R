# fit_rf_xgb_export.R
# Train RF & XGB on CVHT, export observed & predicted for Emma's probabilistic step


## --------- User inputs (EDIT THESE) ----------
data_path   <- "~/Downloads/LakeData/Final_Analysis_Ready_Data.csv" # Match your path
outcome_col <- "CVHT_standardized"                                 # Updated from CVHT
year_col    <- "Year"                                              # Capital Y
lake_col    <- "lakeid"                                            # Lowercase l
#add lake ID here (needs to go lake by lake) -- DONE
#Add PCA code to here as well -- Done

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
dat0 <- read_csv(data_path, show_col_types = FALSE) 

dat <- dat0
if (!(outcome_col %in% names(dat))) {
  stop(sprintf("Outcome '%s' not found.", outcome_col))
}

# Define Models
pcr_spec <- linear_reg() |> set_engine("lm") |> set_mode("regression")

rf_spec  <- rand_forest(trees = 1000, mtry = 3, min_n = 5) |> 
  set_engine("ranger", importance = "impurity") |> set_mode("regression")

xgb_spec <- boost_tree(trees = 1000, learn_rate = 0.05, tree_depth = 6, min_n = 5, loss_reduction = 0) |> 
  set_engine("xgboost", nthread = max(1, parallel::detectCores() - 1)) |> set_mode("regression")


# --- Loop through each lake ---
lake_list <- unique(dat[[lake_col]])
results_list <- list()

for (current_lake in lake_list) {
  # Subset data for this specific lake
  lake_dat <- dat |> filter(.data[[lake_col]] == current_lake)
  


# ==============================================================================
  # --- 1. Recipe & PCA ---
# ==============================================================================
  # Keep predictors except outcome, year, and lake
  # Remove n_months_sampled — it's a data quality flag, not a real predictor
  predictor_cols <- setdiff(names(lake_dat), 
                            c(outcome_col, year_col, lake_col, "n_months_sampled"))  
  # Ensure outcome is numeric
  if (!is.numeric(lake_dat[[outcome_col]])) {
    stop(sprintf("Outcome '%s' must be numeric.", outcome_col))
  }
  
  # Recipe: impute, remove zero-variance, scale, and PCA (Emma's 3 components)
  rec <- recipe(as.formula(paste(outcome_col, "~", paste(predictor_cols, collapse = " + "))), data = lake_dat) |>
    step_impute_median(all_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_pca(all_numeric_predictors(), num_comp = 3)
  
  # Workflows
  wf_pcr <- workflow() |> add_model(pcr_spec) |> add_recipe(rec)
  wf_rf  <- workflow() |> add_model(rf_spec)  |> add_recipe(rec)
  wf_xgb <- workflow() |> add_model(xgb_spec) |> add_recipe(rec)
# ==============================================================================
# --- 2. Leave-One-Out Cross-Validation Loop ---
# ==============================================================================
  
  # Initialize empty prediction columns
  lake_dat$pred_pcr <- NA
  lake_dat$pred_rf  <- NA
  lake_dat$pred_xgb <- NA
  
  for (i in seq_len(nrow(lake_dat))) {
    train_dat <- lake_dat[-i, ]
    test_dat  <- lake_dat[i, , drop = FALSE]
    
    # Train
    fit_pcr <- wf_pcr |> fit(train_dat)
    fit_rf  <- wf_rf  |> fit(train_dat)
    fit_xgb <- wf_xgb |> fit(train_dat)
    
    # Predict
    lake_dat$pred_pcr[i] <- predict(fit_pcr, test_dat) |> pull(.pred)
    lake_dat$pred_rf[i]  <- predict(fit_rf, test_dat)  |> pull(.pred)
    lake_dat$pred_xgb[i] <- predict(fit_xgb, test_dat) |> pull(.pred)
  }
# ==============================================================================  
# --- 3. Format output for this lake ---
# ==============================================================================
  export_lake <- tibble(
    lake_id       = lake_dat[[lake_col]],
    year          = if (!is.null(year_col) && year_col %in% names(lake_dat)) lake_dat[[year_col]] else seq_len(nrow(lake_dat)),
    observed_cvht = lake_dat[[outcome_col]],
    pred_pcr      = lake_dat$pred_pcr,
    pred_rf       = lake_dat$pred_rf,
    pred_xgb      = lake_dat$pred_xgb
  )
  
  results_list[[current_lake]] <- export_lake
} # <--- CLOSES THE LAKE LOOP!


# ==============================================================================
# --- 4. Bind Lakes, Export, and Calculate Metrics ---
# ==============================================================================

export_all <- bind_rows(results_list)

write_csv(export_all, "cvht_obs_preds_all_models.csv")
write_csv(select(export_all, lake_id, year, observed_cvht, pred_pcr), "cvht_obs_pred_pcr.csv")
write_csv(select(export_all, lake_id, year, observed_cvht, pred_rf),  "cvht_obs_pred_rf.csv")
write_csv(select(export_all, lake_id, year, observed_cvht, pred_xgb), "cvht_obs_pred_xgb.csv")

cat("\nWrote output CSVs for PCR, RF, and XGBoost.\n")

# Metrics (R² / RMSE / MAE) grouped by Lake
reg_metrics <- yardstick::metric_set(rsq, rmse, mae)

scores_pcr <- export_all |> group_by(lake_id) |> reg_metrics(truth = observed_cvht, estimate = pred_pcr) |> mutate(model = "PCR")
scores_rf  <- export_all |> group_by(lake_id) |> reg_metrics(truth = observed_cvht, estimate = pred_rf) |> mutate(model = "Random Forest")
scores_xgb <- export_all |> group_by(lake_id) |> reg_metrics(truth = observed_cvht, estimate = pred_xgb) |> mutate(model = "XGBoost")

scores <- bind_rows(scores_pcr, scores_rf, scores_xgb) |>
  select(lake_id, model, .metric, .estimate) |>
  arrange(lake_id, model, .metric)

print(scores)
write_csv(scores, "cvht_model_scores.csv")
cat("Wrote: cvht_model_scores.csv\n")

# ==========================================================
# FINAL POSTER DATA PREP: SIGNIFICANCE & PERFORMANCE TABLE
# ==========================================================

# 1. Calculate Significance Threshold 
n <- 44
df <- n - 2
alpha <- 0.05
critical_value <- qt(1 - alpha/2, df)
corr_threshold <- critical_value / sqrt(critical_value^2 + df) 

# 2. Screening Predictors for Transferability
# Checks which climate/lake variables are significant for each lake
sig_check <- export_all %>%
  group_by(lake_id) %>%
  summarise(across(starts_with("pred_"), 
                   ~cor(.x, observed_cvht, use = "complete.obs"))) %>%
  pivot_longer(cols = -lake_id, names_to = "predictor", values_to = "correlation") %>%
  mutate(is_significant = abs(correlation) > corr_threshold)

write.csv(sig_check, file.path(base_dir, "Predictor_Significance_Check.csv"), row.names = FALSE)

# 3. Generate Final Metrics for Poster Table
poster_metrics <- export_all %>%
  mutate(pred_xgb_cleaned = pmax(pred_xgb, 0)) %>% 
  group_by(lake_id) %>%
  summarise(
    PCR_R2   = round(cor(observed_cvht, pred_pcr)^2, 2),
    RF_R2    = round(cor(observed_cvht, pred_rf)^2, 2),
    XGB_R2   = round(cor(observed_cvht, pred_xgb_cleaned)^2, 2),
    Best_Model = case_when(
      PCR_R2 >= RF_R2 & PCR_R2 >= XGB_R2 ~ "PCR",
      RF_R2 >= PCR_R2 & RF_R2 >= XGB_R2 ~ "Random Forest",
      TRUE ~ "XGBoost"
    )
  )

print("--- FINAL POSTER METRICS ---")
print(poster_metrics)

# Save so we can open it in Excel 
write.csv(poster_metrics, file.path(base_dir, "Poster_Final_Metrics.csv"), row.names = FALSE)
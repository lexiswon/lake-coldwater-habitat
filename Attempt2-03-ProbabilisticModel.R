# ==============================================================================
# 03_Probabilistic_Model.R
# Loops through all lakes AND all three models (PCR, RF, XGB)
# Outputs comparison table of HSS and RPSS for poster
# ==============================================================================

library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)

set.seed(42)

base_dir <- "~/Downloads/LakeData"

# Load the output from Script 02
all_preds <- read_csv(file.path(base_dir, "cvht_obs_preds_all_models.csv"),
                      show_col_types = FALSE)

lake_list  <- unique(all_preds$lake_id)
model_list <- c("PCR" = "pred_pcr", 
                "Random Forest" = "pred_rf", 
                "XGBoost" = "pred_xgb")

skill_results <- list()

# ==============================================================================
# --- Loop through each lake AND each model ---
# ==============================================================================

for (current_lake in lake_list) {
  
  cat(sprintf("\n========== Lake: %s ==========\n", current_lake))
  
  lake_data <- all_preds %>% filter(lake_id == current_lake)
  
  Observed <- lake_data$observed_cvht
  Years    <- lake_data$year
  
  # Define categories based on observed data for this lake
  AN     <- quantile(Observed, 0.67)
  BN     <- quantile(Observed, 0.33)
  bins   <- c(-Inf, BN, AN, Inf)
  labels <- c("Below Normal", "Near Normal", "Above Normal")
  ncat   <- 3
  
  for (model_name in names(model_list)) {
    
    pred_col  <- model_list[[model_name]]
    Predicted <- lake_data[[pred_col]]
    
    cat(sprintf("\n  --- Model: %s ---\n", model_name))
    
    # --- Build Ensemble ---
    errors <- Observed - Predicted
    mu     <- mean(errors)
    std    <- sd(errors)
    
    ensembles <- list()
    for (i in seq_along(Predicted)) {
      error_samples    <- rnorm(100, mean = mu, sd = std)
      ensemble_members <- pmax(0, error_samples + Predicted[i])
      ensembles[[i]]   <- ensemble_members
    }
    
    EnsPreds <- as.data.frame(do.call(rbind, ensembles))
    colnames(EnsPreds) <- paste0("V", 1:100)
    EnsPreds$variable  <- Years
    
    EnsPreds <- pivot_longer(EnsPreds,
                             cols      = -variable,
                             names_to  = "ensemble",
                             values_to = "value")
    
    # --- Plot ---
    p <- ggplot(EnsPreds, aes(x = factor(variable), y = value)) +
      geom_boxplot(fill = "#0868ac", outlier.size = 0.5) +
      geom_line(data = data.frame(variable = Years, value = Observed),
                aes(x = factor(variable), y = value, group = 1),
                color = "black", linewidth = 0.8) +
      geom_hline(yintercept = AN, color = "red", linetype = "dashed") +
      geom_hline(yintercept = BN, color = "red", linetype = "dashed") +
      labs(
        title = sprintf("Lake %s — %s Probabilistic CVHT", current_lake, model_name),
        y     = "CVHT (m)",
        x     = "Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    
    ggsave(
      file.path(base_dir, sprintf("prob_plot_%s_%s.png", 
                                  current_lake, 
                                  gsub(" ", "_", model_name))),
      plot   = p,
      width  = 12,
      height = 5
    )
    
    # --- Skill Scores ---
    EnsPreds$categories <- cut(EnsPreds$value, breaks = bins, labels = labels)
    
    EnsCats <- EnsPreds %>%
      group_by(variable, categories) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(variable) %>%
      mutate(Proportion = n / sum(n)) %>%
      select(-n) %>%
      ungroup()
    
    # HSS
    idx <- EnsCats %>%
      group_by(variable) %>%
      slice_max(Proportion, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    PredictedCategories <- idx %>% arrange(variable) %>% pull(categories)
    ObservedCategories  <- cut(Observed, breaks = bins, labels = labels)
    
    Hits     <- sum(PredictedCategories == ObservedCategories)
    Expected <- length(Observed) / ncat
    Total    <- length(Observed)
    HSS      <- (Hits - Expected) / (Total - Expected)
    
    # RPSS
    ObsCatsDf <- data.frame(year = Years, categories = ObservedCategories)
    Obs <- list()
    for (yr in unique(EnsCats$variable)) {
      Ocat <- ObsCatsDf$categories[ObsCatsDf$year == yr]
      Pcat <- EnsCats[EnsCats$variable == yr, ]
      Obs[[length(Obs) + 1]] <- as.integer(Pcat$categories %in% Ocat)
    }
    EnsCats$Obs         <- unlist(Obs)
    EnsCats$Climatology <- 0.33
    
    calc_rps <- function(predicted, observed) {
      rps <- 0
      for (i in 2:length(predicted)) {
        rps <- rps + (sum(predicted[1:(i-1)]) - sum(observed[1:(i-1)]))^2
      }
      return(rps)
    }
    
    rps_predicted   <- c()
    rps_climatology <- c()
    
    for (yr in unique(EnsCats$variable)) {
      year_data <- EnsCats[EnsCats$variable == yr, ] %>%
        arrange(factor(categories, levels = labels))
      rps_predicted   <- c(rps_predicted,
                           calc_rps(year_data$Proportion, year_data$Obs))
      rps_climatology <- c(rps_climatology,
                           calc_rps(year_data$Climatology, year_data$Obs))
    }
    
    RPSS <- 1 - (median(rps_predicted  / (ncat - 1)) /
                   median(rps_climatology / (ncat - 1)))
    
    cat(sprintf("    HSS  = %.3f\n", HSS))
    cat(sprintf("    RPSS = %.3f\n", RPSS))
    
    # --- Confusion Matrix ---
    category_mapping <- c("Below Normal" = 0, "Near Normal" = 1, "Above Normal" = 2)
    
    predicted_categories <- EnsCats %>%
      group_by(variable) %>%
      slice_max(Proportion, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(variable, categories) %>%
      mutate(predicted = category_mapping[as.character(categories)])
    
    observed_categories <- EnsCats %>%
      filter(Obs == 1) %>%
      select(variable, categories) %>%
      mutate(observed = category_mapping[as.character(categories)])
    
    comparison_df <- merge(predicted_categories, observed_categories, by = "variable")
    
    accuracy <- sum(comparison_df$observed == comparison_df$predicted) / nrow(comparison_df)
    cat(sprintf("    Accuracy = %.2f\n", accuracy))
    
    # Store results
    skill_results[[paste(current_lake, model_name)]] <- tibble(
      lake_id  = current_lake,
      model    = model_name,
      HSS      = round(HSS,      3),
      RPSS     = round(RPSS,     3),
      Accuracy = round(accuracy, 3)
    )
  }
}

# ==============================================================================
# --- Final Summary Table ---
# ==============================================================================

skill_summary <- bind_rows(skill_results) %>%
  arrange(lake_id, model)

cat("\n\n========== FINAL SKILL SCORE SUMMARY ==========\n")
print(skill_summary)

# Wide format for easy poster comparison
skill_wide <- skill_summary %>%
  select(lake_id, model, RPSS) %>%
  pivot_wider(names_from = model, values_from = RPSS) %>%
  rename(PCR_RPSS = PCR, RF_RPSS = `Random Forest`, XGB_RPSS = XGBoost)

cat("\n--- RPSS Comparison Table (Wide Format) ---\n")
print(skill_wide)

# Save both formats
write_csv(skill_summary, file.path(base_dir, "Skill_Scores_All_Lakes_All_Models.csv"))
write_csv(skill_wide,    file.path(base_dir, "Skill_Scores_Wide_Poster.csv"))

cat("\nWrote: Skill_Scores_All_Lakes_All_Models.csv\n")
cat("Wrote: Skill_Scores_Wide_Poster.csv\n")
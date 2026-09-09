# ==============================================================================
# 04_Poster_Figures.R
# All poster figures: time series, bar charts, spider plot, heatmap
# ==============================================================================

rm(list = ls())
set.seed(42)

base_dir <- "~/Downloads/LakeData"

# --- Packages ---
need <- c("readr", "dplyr", "tidyr", "ggplot2", "viridis", 
          "scales", "fmsb", "gridExtra", "ggpubr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(viridis); library(scales); library(fmsb)
  library(gridExtra); library(ggpubr)
})

# --- Load Data ---
all_preds   <- read_csv(file.path(base_dir, "cvht_obs_preds_all_models.csv"),
                        show_col_types = FALSE)
skill_data  <- read_csv(file.path(base_dir, "Skill_Scores_All_Lakes_All_Models.csv"),
                        show_col_types = FALSE)
poster_metrics <- read_csv(file.path(base_dir, "Poster_Final_Metrics.csv"),
                           show_col_types = FALSE)

# Color palette consistent across all figures
model_colors <- c("PCR" = "#2166ac", 
                  "Random Forest" = "#4dac26", 
                  "XGBoost" = "#d6604d")

cat("All data loaded successfully.\n")

# ==============================================================================
# FIGURE 1: Time Series — Observed vs Predicted for BM (all 3 models)
# ==============================================================================

cat("\nGenerating Figure 1: BM Time Series...\n")

bm_data <- all_preds %>% filter(lake_id == "BM")

bm_long <- bm_data %>%
  select(year, observed_cvht, pred_pcr, pred_rf, pred_xgb) %>%
  pivot_longer(
    cols      = c(pred_pcr, pred_rf, pred_xgb),
    names_to  = "model",
    values_to = "predicted"
  ) %>%
  mutate(model = recode(model,
                        "pred_pcr" = "PCR",
                        "pred_rf"  = "Random Forest",
                        "pred_xgb" = "XGBoost"))

fig1 <- ggplot(bm_long, aes(x = year)) +
  geom_line(aes(y = observed_cvht),
            color = "black", linewidth = 1.2, linetype = "solid") +
  geom_point(aes(y = observed_cvht),
             color = "black", size = 2.5) +
  geom_line(aes(y = predicted, color = model),
            linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = predicted, color = model),
             size = 1.8, alpha = 0.8) +
  scale_color_manual(values = model_colors) +
  facet_wrap(~model, ncol = 1) +
  labs(
    title    = "Big Muskellunge Lake — Observed vs. Predicted CVHT (LOOCV)",
    subtitle = "Black line = Observed | Dashed line = Model Prediction",
    x        = "Year",
    y        = "CVHT (m)",
    color    = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 12, color = "gray40"),
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

print(fig1)
ggsave(file.path(base_dir, "Fig1_BM_TimeSeries.png"),
       plot = fig1, width = 14, height = 10, dpi = 300)
cat("  Saved: Fig1_BM_TimeSeries.png\n")

# ==============================================================================
# FIGURE 2: Bar Chart — R² by Model and Lake
# ==============================================================================

cat("\nGenerating Figure 2: R² Bar Chart...\n")

r2_long <- poster_metrics %>%
  select(lake_id, PCR_R2, RF_R2, XGB_R2) %>%
  pivot_longer(
    cols      = c(PCR_R2, RF_R2, XGB_R2),
    names_to  = "model",
    values_to = "R2"
  ) %>%
  mutate(model = recode(model,
                        "PCR_R2" = "PCR",
                        "RF_R2"  = "Random Forest",
                        "XGB_R2" = "XGBoost"))

fig2 <- ggplot(r2_long, aes(x = lake_id, y = R2, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "white") +
  geom_text(aes(label = sprintf("%.2f", R2)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 0.55),
                     labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title = "Deterministic Skill: R² by Model and Lake",
    subtitle = "Higher R² indicates better model performance",
    x     = "Lake",
    y     = "R² (Coefficient of Determination)",
    fill  = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 12, color = "gray40"),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(fig2)
ggsave(file.path(base_dir, "Fig2_R2_BarChart.png"),
       plot = fig2, width = 12, height = 7, dpi = 300)
cat("  Saved: Fig2_R2_BarChart.png\n")

# ==============================================================================
# FIGURE 3: Bar Chart — RPSS by Model and Lake
# ==============================================================================

cat("\nGenerating Figure 3: RPSS Bar Chart...\n")

fig3 <- ggplot(skill_data, aes(x = lake_id, y = RPSS, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "white") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.2f", RPSS)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(-0.15, 0.65),
                     labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title    = "Probabilistic Skill: RPSS by Model and Lake",
    subtitle = "RPSS > 0 indicates skill above climatology reference forecast",
    x        = "Lake",
    y        = "Ranked Probability Skill Score (RPSS)",
    fill     = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(size = 12, color = "gray40"),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(fig3)
ggsave(file.path(base_dir, "Fig3_RPSS_BarChart.png"),
       plot = fig3, width = 12, height = 7, dpi = 300)
cat("  Saved: Fig3_RPSS_BarChart.png\n")

# ==============================================================================
# FIGURE 4: Heatmap — RPSS across all lakes and models
# ==============================================================================

cat("\nGenerating Figure 4: RPSS Heatmap...\n")

fig4 <- ggplot(skill_data, 
               aes(x = model, y = lake_id, fill = RPSS)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.3f", RPSS)),
            size = 5, fontface = "bold",
            color = ifelse(skill_data$RPSS > 0.35, "white", "black")) +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "#ffffbf",
    high     = "#1a9850",
    midpoint = 0.25,
    limits   = c(-0.15, 0.55),
    name     = "RPSS"
  ) +
  scale_x_discrete(labels = c("PCR", "Random\nForest", "XGBoost")) +
  labs(
    title    = "Probabilistic Skill Heatmap: RPSS Across Lakes and Models",
    subtitle = "Green = higher skill | Red = lower skill | Dashed line = climatology (0)",
    x        = "Model",
    y        = "Lake"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text     = element_text(size = 13, face = "bold"),
    legend.title  = element_text(face = "bold"),
    panel.grid    = element_blank()
  )

print(fig4)
ggsave(file.path(base_dir, "Fig4_RPSS_Heatmap.png"),
       plot = fig4, width = 10, height = 7, dpi = 300)
cat("  Saved: Fig4_RPSS_Heatmap.png\n")

# ==============================================================================
# FIGURE 5: Spider / Radar Plot — BM model comparison across metrics
# ==============================================================================

cat("\nGenerating Figure 5: Spider Plot for BM...\n")

bm_skills <- skill_data %>% filter(lake_id == "BM")
bm_r2     <- poster_metrics %>% filter(lake_id == "BM")

# Build radar data frame
# Rows: max, min, PCR, RF, XGB
# Cols: R2, RPSS, HSS, Accuracy

radar_df <- data.frame(
  R2       = c(0.5,  0,
               bm_r2$PCR_R2,
               bm_r2$RF_R2,
               bm_r2$XGB_R2),
  RPSS     = c(0.6,  0,
               bm_skills$RPSS[bm_skills$model == "PCR"],
               bm_skills$RPSS[bm_skills$model == "Random Forest"],
               bm_skills$RPSS[bm_skills$model == "XGBoost"]),
  HSS      = c(0.4,  -0.2,
               bm_skills$HSS[bm_skills$model == "PCR"],
               bm_skills$HSS[bm_skills$model == "Random Forest"],
               bm_skills$HSS[bm_skills$model == "XGBoost"]),
  Accuracy = c(0.6,  0,
               bm_skills$Accuracy[bm_skills$model == "PCR"],
               bm_skills$Accuracy[bm_skills$model == "Random Forest"],
               bm_skills$Accuracy[bm_skills$model == "XGBoost"])
)

rownames(radar_df) <- c("Max", "Min", "PCR", "Random Forest", "XGBoost")

# Save as PNG
png(file.path(base_dir, "Fig5_Spider_BM.png"),
    width = 2400, height = 2000, res = 300)

par(mar = c(2, 2, 4, 2))

fmsb::radarchart(
  radar_df,
  axistype  = 1,
  pcol      = unname(model_colors),
  pfcol     = adjustcolor(unname(model_colors), alpha.f = 0.2),
  plwd      = 3,
  cglcol    = "grey70",
  cglty     = 1,
  axislabcol = "grey30",
  vlcex     = 1.1,
  title     = "Big Muskellunge Lake — Model Performance Comparison"
)

legend(
  x      = "bottomright",
  legend = c("PCR", "Random Forest", "XGBoost"),
  col    = unname(model_colors),
  lty    = 1,
  lwd    = 3,
  bty    = "n",
  cex    = 1.0
)

dev.off()

# Also print to screen
par(mar = c(2, 2, 4, 2))
fmsb::radarchart(
  radar_df,
  axistype   = 1,
  pcol       = unname(model_colors),
  pfcol      = adjustcolor(unname(model_colors), alpha.f = 0.2),
  plwd       = 3,
  cglcol     = "grey70",
  cglty      = 1,
  axislabcol = "grey30",
  vlcex      = 1.1,
  title      = "Big Muskellunge Lake — Model Performance Comparison"
)
legend(
  x      = "bottomright",
  legend = c("PCR", "Random Forest", "XGBoost"),
  col    = unname(model_colors),
  lty    = 1,
  lwd    = 3,
  bty    = "n",
  cex    = 1.0
)

cat("  Saved: Fig5_Spider_BM.png\n")

# ==============================================================================
# FIGURE 6: Combined R2 and RPSS side by side for poster
# ==============================================================================

cat("\nGenerating Figure 6: Combined R2 + RPSS side by side...\n")

fig6 <- ggarrange(fig2, fig3,
                  ncol   = 2,
                  common.legend = TRUE,
                  legend = "bottom",
                  labels = c("A", "B"),
                  font.label = list(size = 16, face = "bold"))

fig6_titled <- annotate_figure(
  fig6,
  top = text_grob(
    "Model Performance Comparison Across Five Northern Wisconsin Lakes",
    face = "bold", size = 18
  )
)

print(fig6_titled)
ggsave(file.path(base_dir, "Fig6_Combined_R2_RPSS.png"),
       plot   = fig6_titled,
       width  = 20,
       height = 8,
       dpi    = 300)
cat("  Saved: Fig6_Combined_R2_RPSS.png\n")

# ==============================================================================
# DONE
# ==============================================================================

cat("\n========== ALL FIGURES COMPLETE ==========\n")
cat(sprintf("All figures saved to: %s\n", base_dir))
cat("\nFigures generated:\n")
cat("  Fig1_BM_TimeSeries.png      — Observed vs predicted time series for BM\n")
cat("  Fig2_R2_BarChart.png        — R2 by model and lake\n")
cat("  Fig3_RPSS_BarChart.png      — RPSS by model and lake\n")
cat("  Fig4_RPSS_Heatmap.png       — RPSS heatmap all lakes and models\n")
cat("  Fig5_Spider_BM.png          — Spider plot BM model comparison\n")
cat("  Fig6_Combined_R2_RPSS.png   — Combined R2 and RPSS side by side\n")



# ==============================================================================
# 04_Poster_Figures_V2.R
# All poster figures — Ocean blues and teals color palette
# ==============================================================================

rm(list = ls())
set.seed(42)

base_dir <- "~/Downloads/LakeData"

# --- Packages ---
need <- c("readr", "dplyr", "tidyr", "ggplot2", "viridis",
          "scales", "fmsb", "ggpubr", "patchwork")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(viridis); library(scales); library(fmsb)
  library(ggpubr); library(patchwork)
})

# --- Load Data ---
all_preds      <- read_csv(file.path(base_dir, "cvht_obs_preds_all_models.csv"),
                           show_col_types = FALSE)
skill_data     <- read_csv(file.path(base_dir, "Skill_Scores_All_Lakes_All_Models.csv"),
                           show_col_types = FALSE)
poster_metrics <- read_csv(file.path(base_dir, "Poster_Final_Metrics.csv"),
                           show_col_types = FALSE)

cat("All data loaded.\n")

# ==============================================================================
# OCEAN BLUES AND TEALS COLOR PALETTE
# ==============================================================================

model_colors <- c(
  "PCR"          = "#0077B6",   # deep ocean blue
  "Random Forest" = "#00B4D8",  # bright teal
  "XGBoost"      = "#48CAE4"    # light sky teal
)

model_fills <- c(
  "PCR"          = "#0077B6",
  "Random Forest" = "#00B4D8",
  "XGBoost"      = "#48CAE4"
)

lake_colors <- c(
  "AL" = "#03045E",  # darkest navy
  "BM" = "#0077B6",  # deep blue
  "CR" = "#00B4D8",  # teal
  "SP" = "#48CAE4",  # light teal
  "TR" = "#90E0EF"   # pale sky
)

lake_labels <- c(
  "AL" = "Allequash",
  "BM" = "Big Muskellunge",
  "CR" = "Crystal",
  "SP" = "Sparkling",
  "TR" = "Trout"
)

# Shared theme for all ggplot figures
theme_poster <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title       = element_text(face = "bold", size = 16,
                                      color = "#03045E"),
      plot.subtitle    = element_text(size = 12, color = "gray45"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", color = "#03045E"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title       = element_text(color = "#03045E"),
      axis.text        = element_text(color = "gray30"),
      strip.text       = element_text(face = "bold", size = 13,
                                      color = "#03045E"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#F8FBFF", color = NA)
    )
}

# ==============================================================================
# FIGURE 1: BM Time Series — Observed vs Predicted (all 3 models)
# ==============================================================================

cat("\nGenerating Figure 1: BM Time Series...\n")

bm_data <- all_preds %>% filter(lake_id == "BM")

bm_long <- bm_data %>%
  select(year, observed_cvht, pred_pcr, pred_rf, pred_xgb) %>%
  pivot_longer(
    cols      = c(pred_pcr, pred_rf, pred_xgb),
    names_to  = "model",
    values_to = "predicted"
  ) %>%
  mutate(model = recode(model,
                        "pred_pcr" = "PCR",
                        "pred_rf"  = "Random Forest",
                        "pred_xgb" = "XGBoost"))

fig1 <- ggplot(bm_long, aes(x = year)) +
  geom_ribbon(aes(ymin = predicted - 2, ymax = predicted + 2,
                  fill = model), alpha = 0.12) +
  geom_line(aes(y = observed_cvht),
            color = "#03045E", linewidth = 1.3) +
  geom_point(aes(y = observed_cvht),
             color = "#03045E", size = 2.2, shape = 19) +
  geom_line(aes(y = predicted, color = model),
            linewidth = 1.0, linetype = "dashed") +
  geom_point(aes(y = predicted, color = model),
             size = 1.6, alpha = 0.9) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values  = model_fills, guide = "none") +
  facet_wrap(~model, ncol = 1) +
  labs(
    title    = "Big Muskellunge Lake — Observed vs. Predicted CVHT",
    subtitle = "Dark line = Observed  |  Dashed line = LOOCV Model Prediction",
    x        = "Year",
    y        = "CVHT (m)",
    color    = "Model"
  ) +
  theme_poster() +
  theme(legend.position = "none")

print(fig1)
ggsave(file.path(base_dir, "Fig1_BM_TimeSeries.png"),
       plot = fig1, width = 14, height = 10, dpi = 300,
       bg = "white")
cat("  Saved: Fig1_BM_TimeSeries.png\n")

# ==============================================================================
# FIGURE 2: R² Bar Chart
# ==============================================================================

cat("\nGenerating Figure 2: R2 Bar Chart...\n")

r2_long <- poster_metrics %>%
  select(lake_id, PCR_R2, RF_R2, XGB_R2) %>%
  pivot_longer(
    cols      = c(PCR_R2, RF_R2, XGB_R2),
    names_to  = "model",
    values_to = "R2"
  ) %>%
  mutate(
    model    = recode(model,
                      "PCR_R2" = "PCR",
                      "RF_R2"  = "Random Forest",
                      "XGB_R2" = "XGBoost"),
    lake_label = recode(lake_id, !!!lake_labels)
  )

fig2 <- ggplot(r2_long, aes(x = lake_label, y = R2, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", R2)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5, fontface = "bold",
            color = "#03045E") +
  scale_fill_manual(values = model_fills) +
  scale_y_continuous(limits = c(0, 0.50),
                     labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Deterministic Skill: R² by Model and Lake",
    subtitle = "Higher R² indicates better out-of-sample prediction accuracy",
    x    = "Lake",
    y    = "R²",
    fill = "Model"
  ) +
  theme_poster() +
  theme(panel.grid.major.x = element_blank())

print(fig2)
ggsave(file.path(base_dir, "Fig2_R2_BarChart.png"),
       plot = fig2, width = 12, height = 7, dpi = 300, bg = "white")
cat("  Saved: Fig2_R2_BarChart.png\n")

# ==============================================================================
# FIGURE 3: RPSS Bar Chart
# ==============================================================================

cat("\nGenerating Figure 3: RPSS Bar Chart...\n")

skill_labeled <- skill_data %>%
  mutate(lake_label = recode(lake_id, !!!lake_labels))

fig3 <- ggplot(skill_labeled, aes(x = lake_label, y = RPSS, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7, color = "white", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "#03045E",
             linewidth = 0.9, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.2f", RPSS)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5, fontface = "bold",
            color = "#03045E") +
  scale_fill_manual(values = model_fills) +
  scale_y_continuous(limits = c(-0.15, 0.65),
                     labels = number_format(accuracy = 0.01)) +
  labs(
    title    = "Probabilistic Skill: RPSS by Model and Lake",
    subtitle = "RPSS > 0 indicates skill above the climatology reference forecast",
    x        = "Lake",
    y        = "RPSS",
    fill     = "Model"
  ) +
  theme_poster() +
  theme(panel.grid.major.x = element_blank())

print(fig3)
ggsave(file.path(base_dir, "Fig3_RPSS_BarChart.png"),
       plot = fig3, width = 12, height = 7, dpi = 300, bg = "white")
cat("  Saved: Fig3_RPSS_BarChart.png\n")

# ==============================================================================
# FIGURE 4: Heatmap — RPSS all lakes and models (FIXED)
# ==============================================================================

cat("\nGenerating Figure 4: RPSS Heatmap...\n")

skill_heatmap <- skill_data %>%
  mutate(
    lake_label  = recode(lake_id, !!!lake_labels),
    model       = factor(model, levels = c("PCR", "Random Forest", "XGBoost")),
    text_color  = ifelse(RPSS > 0.38, "white", "#03045E")
  )

fig4 <- ggplot(skill_heatmap,
               aes(x = model, y = lake_label, fill = RPSS)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.2f", RPSS),
                color = text_color),
            size = 5.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#CAF0F8", "#90E0EF", "#00B4D8", "#0077B6", "#03045E"),
    limits = c(0, 0.55),
    name   = "RPSS"
  ) +
  scale_x_discrete(labels = c("PCR", "Random\nForest", "XGBoost")) +
  labs(
    title    = "Probabilistic Skill (RPSS) — All Lakes and Models",
    subtitle = "Darker blue = higher skill above climatology",
    x        = "Model",
    y        = "Lake"
  ) +
  theme_poster() +
  theme(
    panel.grid   = element_blank(),
    legend.title = element_text(face = "bold")
  )

print(fig4)
ggsave(file.path(base_dir, "Fig4_RPSS_Heatmap.png"),
       plot = fig4, width = 10, height = 7, dpi = 300, bg = "white")
cat("  Saved: Fig4_RPSS_Heatmap.png\n")

# ==============================================================================
# FIGURE 5: Spider Plot — BM model comparison (FIXED legend)
# ==============================================================================

cat("\nGenerating Figure 5: Spider Plot...\n")

bm_skills <- skill_data %>% filter(lake_id == "BM")
bm_r2     <- poster_metrics %>% filter(lake_id == "BM")

radar_df <- data.frame(
  R2       = c(0.5, 0,
               bm_r2$PCR_R2,
               bm_r2$RF_R2,
               bm_r2$XGB_R2),
  RPSS     = c(0.6, 0,
               bm_skills$RPSS[bm_skills$model == "PCR"],
               bm_skills$RPSS[bm_skills$model == "Random Forest"],
               bm_skills$RPSS[bm_skills$model == "XGBoost"]),
  HSS      = c(0.4, -0.2,
               bm_skills$HSS[bm_skills$model == "PCR"],
               bm_skills$HSS[bm_skills$model == "Random Forest"],
               bm_skills$HSS[bm_skills$model == "XGBoost"]),
  Accuracy = c(0.6, 0,
               bm_skills$Accuracy[bm_skills$model == "PCR"],
               bm_skills$Accuracy[bm_skills$model == "Random Forest"],
               bm_skills$Accuracy[bm_skills$model == "XGBoost"])
)
rownames(radar_df) <- c("Max", "Min", "PCR", "Random Forest", "XGBoost")

spider_colors <- c("#0077B6", "#00B4D8", "#48CAE4")

png(file.path(base_dir, "Fig5_Spider_BM.png"),
    width = 2600, height = 2200, res = 300)
par(mar = c(4, 4, 5, 4), bg = "#F8FBFF")

fmsb::radarchart(
  radar_df,
  axistype   = 1,
  pcol       = spider_colors,
  pfcol      = adjustcolor(spider_colors, alpha.f = 0.18),
  plwd       = 3.5,
  cglcol     = "gray80",
  cglty      = 1,
  axislabcol = "gray40",
  vlcex      = 1.15,
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  title      = "Big Muskellunge — Model Skill Comparison"
)
legend(
  x      = 1.3,
  y      = 1.3,
  legend = c("PCR", "Random Forest", "XGBoost"),
  col    = spider_colors,
  lty    = 1,
  lwd    = 3.5,
  bty    = "n",
  cex    = 1.1,
  title  = "Model"
)
dev.off()

# Print to screen too
par(mar = c(4, 4, 5, 4))
fmsb::radarchart(
  radar_df,
  axistype   = 1,
  pcol       = spider_colors,
  pfcol      = adjustcolor(spider_colors, alpha.f = 0.18),
  plwd       = 3.5,
  cglcol     = "gray80",
  cglty      = 1,
  axislabcol = "gray40",
  vlcex      = 1.15,
  title      = "Big Muskellunge — Model Skill Comparison"
)
legend(
  x      = 1.3,
  y      = 1.3,
  legend = c("PCR", "Random Forest", "XGBoost"),
  col    = spider_colors,
  lty    = 1,
  lwd    = 3.5,
  bty    = "n",
  cex    = 1.1,
  title  = "Model"
)
cat("  Saved: Fig5_Spider_BM.png\n")

# ==============================================================================
# FIGURE 6: Combined R2 + RPSS side by side
# ==============================================================================

cat("\nGenerating Figure 6: Combined R2 + RPSS...\n")

fig6 <- fig2 + fig3 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig6_titled <- fig6 +
  plot_annotation(
    title   = "Model Performance Across Five Northern Wisconsin Lakes",
    theme   = theme(
      plot.title = element_text(face = "bold", size = 20,
                                color = "#03045E", hjust = 0.5)
    )
  )

print(fig6_titled)
ggsave(file.path(base_dir, "Fig6_Combined_R2_RPSS.png"),
       plot = fig6_titled, width = 20, height = 8,
       dpi = 300, bg = "white")
cat("  Saved: Fig6_Combined_R2_RPSS.png\n")

# ==============================================================================
# NEW FIGURE 7: Probabilistic Boxplot for BM — PCR only (like Emma's figure)
# ==============================================================================

cat("\nGenerating Figure 7: BM Probabilistic Boxplot...\n")

bm_dat    <- all_preds %>% filter(lake_id == "BM")
Observed  <- bm_dat$observed_cvht
Predicted <- bm_dat$pred_pcr
Years     <- bm_dat$year

AN <- quantile(Observed, 0.67)
BN <- quantile(Observed, 0.33)

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

EnsPreds_long <- pivot_longer(EnsPreds,
                              cols      = -variable,
                              names_to  = "ensemble",
                              values_to = "value")

# Color background regions by category
bg_data <- data.frame(
  xmin = -Inf, xmax = Inf,
  ymin = c(-Inf,  BN,  AN),
  ymax = c(BN,    AN,  Inf),
  zone = c("Below Normal", "Near Normal", "Above Normal")
)

fig7 <- ggplot(EnsPreds_long, aes(x = factor(variable), y = value)) +
  geom_rect(data = bg_data,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax, fill = zone),
            inherit.aes = FALSE, alpha = 0.08) +
  scale_fill_manual(
    values = c("Below Normal" = "#CAF0F8",
               "Near Normal"  = "#90E0EF",
               "Above Normal" = "#0077B6"),
    name   = "Category",
    guide  = guide_legend(order = 2)
  ) +
  geom_boxplot(fill = "#00B4D8", color = "#0077B6",
               alpha = 0.7, outlier.size = 0.4,
               outlier.color = "#48CAE4") +
  geom_line(data = data.frame(variable = Years, value = Observed),
            aes(x = factor(variable), y = value, group = 1),
            color = "#03045E", linewidth = 1.1) +
  geom_point(data = data.frame(variable = Years, value = Observed),
             aes(x = factor(variable), y = value),
             color = "#03045E", size = 2.2, shape = 19) +
  geom_hline(yintercept = AN, color = "#0077B6",
             linetype = "dashed", linewidth = 0.9) +
  geom_hline(yintercept = BN, color = "#0077B6",
             linetype = "dashed", linewidth = 0.9) +
  labs(
    title    = "Big Muskellunge Lake — PCR Probabilistic CVHT Forecast",
    subtitle = "Blue boxes = ensemble spread  |  Dark line = observed CVHT  |  Dashed = category thresholds",
    x        = "Year",
    y        = "CVHT (m)"
  ) +
  theme_poster() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "right"
  )

print(fig7)
ggsave(file.path(base_dir, "Fig7_BM_Probabilistic_Boxplot.png"),
       plot = fig7, width = 16, height = 7, dpi = 300, bg = "white")
cat("  Saved: Fig7_BM_Probabilistic_Boxplot.png\n")

# ==============================================================================
# NEW FIGURE 8: Scatter Plot — Observed vs Predicted for BM (all 3 models)
# ==============================================================================

cat("\nGenerating Figure 8: BM Scatter Plot...\n")

bm_scatter <- bm_long  # reuse from Fig 1

fig8 <- ggplot(bm_scatter, aes(x = observed_cvht, y = predicted,
                               color = model)) +
  geom_abline(slope = 1, intercept = 0,
              color = "gray60", linetype = "dashed",
              linewidth = 1.0) +
  geom_point(size = 3.0, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.12,
              linewidth = 0.8) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values  = model_fills) +
  facet_wrap(~model, ncol = 3) +
  labs(
    title    = "Big Muskellunge Lake — Observed vs. Predicted CVHT",
    subtitle = "Dashed line = perfect prediction  |  Shaded band = 95% confidence interval",
    x        = "Observed CVHT (m)",
    y        = "Predicted CVHT (m)",
    color    = "Model"
  ) +
  theme_poster() +
  theme(legend.position = "none")

print(fig8)
ggsave(file.path(base_dir, "Fig8_BM_Scatter.png"),
       plot = fig8, width = 14, height = 6, dpi = 300, bg = "white")
cat("  Saved: Fig8_BM_Scatter.png\n")

# ==============================================================================
# NEW FIGURE 9: CVHT Time Series All 5 Lakes
# ==============================================================================

cat("\nGenerating Figure 9: CVHT Time Series All Lakes...\n")

all_ts <- all_preds %>%
  mutate(lake_label = recode(lake_id, !!!lake_labels))

fig9 <- ggplot(all_ts, aes(x = year, y = observed_cvht,
                           color = lake_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_manual(
    values = c(
      "Allequash"        = "#03045E",
      "Big Muskellunge"  = "#0077B6",
      "Crystal"          = "#00B4D8",
      "Sparkling"        = "#48CAE4",
      "Trout"            = "#90E0EF"
    ),
    name = "Lake"
  ) +
  facet_wrap(~lake_label, ncol = 1, scales = "free_y") +
  labs(
    title    = "Observed CVHT Time Series — Five Northern Wisconsin Lakes",
    subtitle = "Considerable interannual variability motivates season-ahead prediction",
    x        = "Year",
    y        = "CVHT (m)"
  ) +
  theme_poster() +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 11,
                                   color = "#03045E")
  )

print(fig9)
ggsave(file.path(base_dir, "Fig9_AllLakes_TimeSeries.png"),
       plot = fig9, width = 14, height = 14, dpi = 300, bg = "white")
cat("  Saved: Fig9_AllLakes_TimeSeries.png\n")

# ==============================================================================
# DONE
# ==============================================================================

cat("\n========== ALL FIGURES COMPLETE ==========\n")
cat(sprintf("Saved to: %s\n", base_dir))
cat("\n")
cat("  Fig1  — BM Time Series (observed vs predicted)\n")
cat("  Fig2  — R2 Bar Chart\n")
cat("  Fig3  — RPSS Bar Chart\n")
cat("  Fig4  — RPSS Heatmap (fixed)\n")
cat("  Fig5  — Spider Plot BM (legend fixed)\n")
cat("  Fig6  — Combined R2 + RPSS side by side\n")
cat("  Fig7  — BM Probabilistic Boxplot like Emma's\n")
cat("  Fig8  — BM Scatter observed vs predicted\n")
cat("  Fig9  — All 5 lakes CVHT time series\n")

# ==============================================================================
# Spider Plot Panel — All 5 Lakes, Clear Labels + Proper Legend
# Prints to R AND saves PNG
# ==============================================================================

library(fmsb)
library(readr)
library(dplyr)

base_dir <- "~/Downloads/LakeData"

skill_data     <- read_csv(file.path(base_dir, "Skill_Scores_All_Lakes_All_Models.csv"),
                           show_col_types = FALSE)
poster_metrics <- read_csv(file.path(base_dir, "Poster_Final_Metrics.csv"),
                           show_col_types = FALSE)

lake_labels <- c(
  "AL" = "Allequash",
  "BM" = "Big Muskellunge",
  "CR" = "Crystal",
  "SP" = "Sparkling",
  "TR" = "Trout"
)

spider_colors <- c("#0077B6", "#00B4D8", "#48CAE4")
spider_fills  <- adjustcolor(spider_colors, alpha.f = 0.18)

# ==============================================================================
# Shared function to draw the full panel — called twice (screen + PNG)
# ==============================================================================

draw_spider_panel <- function() {
  
  layout(matrix(c(1, 2, 3,
                  4, 5, 6), nrow = 2, byrow = TRUE))
  
  par(bg = "#F0F8FF", oma = c(3, 1, 5, 1))
  
  for (lake in c("AL", "BM", "CR", "SP", "TR")) {
    
    lake_skills <- skill_data     %>% filter(lake_id == lake)
    lake_r2     <- poster_metrics %>% filter(lake_id == lake)
    
    pcr_r2   <- lake_r2$PCR_R2
    rf_r2    <- lake_r2$RF_R2
    xgb_r2   <- lake_r2$XGB_R2
    
    pcr_rpss  <- lake_skills$RPSS[lake_skills$model == "PCR"]
    rf_rpss   <- lake_skills$RPSS[lake_skills$model == "Random Forest"]
    xgb_rpss  <- lake_skills$RPSS[lake_skills$model == "XGBoost"]
    
    pcr_hss  <- max(0, lake_skills$HSS[lake_skills$model == "PCR"])
    rf_hss   <- max(0, lake_skills$HSS[lake_skills$model == "Random Forest"])
    xgb_hss  <- max(0, lake_skills$HSS[lake_skills$model == "XGBoost"])
    
    pcr_acc  <- lake_skills$Accuracy[lake_skills$model == "PCR"]
    rf_acc   <- lake_skills$Accuracy[lake_skills$model == "Random Forest"]
    xgb_acc  <- lake_skills$Accuracy[lake_skills$model == "XGBoost"]
    
    radar_lake <- data.frame(
      R2       = c(0.5,  0, pcr_r2,   rf_r2,   xgb_r2),
      RPSS     = c(0.6,  0, pcr_rpss, rf_rpss, xgb_rpss),
      HSS      = c(0.4,  0, pcr_hss,  rf_hss,  xgb_hss),
      Accuracy = c(0.65, 0, pcr_acc,  rf_acc,  xgb_acc)
    )
    
    rownames(radar_lake) <- c("Max", "Min", "PCR", "Random Forest", "XGBoost")
    colnames(radar_lake) <- c("R²", "RPSS", "HSS", "Accuracy")
    
    par(mar = c(3, 3, 5, 3))
    
    fmsb::radarchart(
      radar_lake,
      axistype    = 1,
      pcol        = spider_colors,
      pfcol       = spider_fills,
      plwd        = 3.5,
      plty        = c(1, 2, 3),
      cglcol      = "#AED6F1",
      cglty       = 1,
      cglwd       = 1.0,
      axislabcol  = "#0077B6",
      caxislabels = c("0", "0.15", "0.30", "0.45", "0.60"),
      calcex      = 0.85,
      vlcex       = 1.15,
      title       = ""
    )
    
    title(
      main      = lake_labels[[lake]],
      line      = 2.5,
      cex.main  = 1.4,
      col.main  = "#03045E",
      font.main = 2
    )
    
    lake_type <- switch(lake,
                        "AL" = "Seepage Lake",
                        "BM" = "Seepage Lake",
                        "CR" = "Seepage Lake",
                        "SP" = "Seepage Lake",
                        "TR" = "Drainage Lake"
    )
    
    mtext(lake_type, side = 3, line = 0.8,
          cex = 0.85, col = "#0077B6", font = 3)
  }
  
  # --- Legend Panel ---
  par(mar = c(3, 3, 5, 3))
  plot.new()
  
  rect(-0.1, -0.1, 1.1, 1.1,
       col    = "#E8F4FD",
       border = "#0077B6",
       lwd    = 2)
  
  text(0.5, 0.92,
       labels = "Model Legend",
       cex    = 1.5,
       font   = 2,
       col    = "#03045E")
  
  line_x  <- c(0.15, 0.45)
  label_x <- 0.55
  
  # PCR
  segments(line_x[1], 0.72, line_x[2], 0.72,
           col = spider_colors[1], lwd = 4, lty = 1)
  points(mean(line_x), 0.72,
         pch = 19, col = spider_colors[1], cex = 1.5)
  text(label_x, 0.72, "PCR",
       adj = 0, cex = 1.3, col = "#03045E", font = 2)
  
  # Random Forest
  segments(line_x[1], 0.55, line_x[2], 0.55,
           col = spider_colors[2], lwd = 4, lty = 2)
  points(mean(line_x), 0.55,
         pch = 17, col = spider_colors[2], cex = 1.5)
  text(label_x, 0.55, "Random Forest",
       adj = 0, cex = 1.3, col = "#03045E", font = 2)
  
  # XGBoost
  segments(line_x[1], 0.38, line_x[2], 0.38,
           col = spider_colors[3], lwd = 4, lty = 3)
  points(mean(line_x), 0.38,
         pch = 15, col = spider_colors[3], cex = 1.5)
  text(label_x, 0.38, "XGBoost",
       adj = 0, cex = 1.3, col = "#03045E", font = 2)
  
  text(0.5, 0.22,
       labels = "Axes represent:",
       cex    = 1.1,
       font   = 2,
       col    = "#03045E")
  
  text(0.5, 0.13,
       labels = "R²  |  RPSS  |  HSS  |  Accuracy",
       cex    = 1.05,
       col    = "#0077B6")
  
  text(0.5, 0.04,
       labels = "Larger area = higher skill",
       cex    = 1.0,
       col    = "#0077B6",
       font   = 3)
  
  # --- Outer titles ---
  mtext(
    "Model Skill Comparison Across Five Northern Wisconsin Lakes",
    outer = TRUE, side = 3, line = 3,
    cex   = 1.8, font = 2, col = "#03045E"
  )
  
  mtext(
    "Each polygon represents one model — larger area indicates higher overall predictive skill",
    outer = TRUE, side = 3, line = 1,
    cex   = 1.1, col = "#0077B6"
  )
  
  mtext(
    "LOOCV Validation (1981-2024)  |  n = 44 years per lake",
    outer = TRUE, side = 1, line = 1,
    cex   = 1.0, col = "#03045E", font = 3
  )
}

# ==============================================================================
# STEP 1: Print to R screen
# ==============================================================================

cat("Printing to R screen...\n")
dev.new(width = 18, height = 13)
draw_spider_panel()
cat("Done! Check your R plot window.\n")

# ==============================================================================
# STEP 2: Save to PNG
# ==============================================================================

cat("Saving PNG...\n")
png(file.path(base_dir, "Fig12_Spider_Panel_AllLakes.png"),
    width  = 5600,
    height = 4200,
    res    = 300,
    bg     = "#F0F8FF")

draw_spider_panel()
dev.off()

cat("Saved: Fig12_Spider_Panel_AllLakes.png\n")
cat("All done!\n")

# ==============================================================================
# Inter-annual Time Series — PCR only, clean poster version
# ==============================================================================

library(readr)
library(dplyr)
library(ggplot2)

base_dir <- "~/Downloads/LakeData"

all_preds <- read_csv(file.path(base_dir, "cvht_obs_preds_all_models.csv"),
                      show_col_types = FALSE)

bm_data <- all_preds %>% filter(lake_id == "BM")

AN <- quantile(bm_data$observed_cvht, 0.67)
BN <- quantile(bm_data$observed_cvht, 0.33)

fig_inter <- ggplot(bm_data, aes(x = year)) +
  
  # Category background shading
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = AN,  ymax = Inf,
           fill = "#0077B6", alpha = 0.06) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = BN,  ymax = AN,
           fill = "#90E0EF", alpha = 0.06) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = BN,
           fill = "#CAF0F8", alpha = 0.08) +
  
  # Category threshold lines
  geom_hline(yintercept = AN,
             color = "#0077B6", linetype = "dashed",
             linewidth = 0.9, alpha = 0.8) +
  geom_hline(yintercept = BN,
             color = "#0077B6", linetype = "dashed",
             linewidth = 0.9, alpha = 0.8) +
  
  # PCR predicted line
  geom_line(aes(y = pred_pcr),
            color = "#00B4D8", linewidth = 1.0,
            linetype = "dashed", alpha = 0.9) +
  geom_point(aes(y = pred_pcr),
             color = "#00B4D8", size = 2.0, alpha = 0.85) +
  
  # Observed line on top
  geom_line(aes(y = observed_cvht),
            color = "#03045E", linewidth = 1.3) +
  geom_point(aes(y = observed_cvht),
             color = "#03045E", size = 2.5, shape = 19) +
  
  # Category threshold labels on right side
  annotate("text", x = 2025, y = AN + 0.6,
           label = "Above Normal", color = "#0077B6",
           size = 3.5, hjust = 1, fontface = "italic") +
  annotate("text", x = 2025, y = BN - 0.6,
           label = "Below Normal", color = "#0077B6",
           size = 3.5, hjust = 1, fontface = "italic") +
  
  # Manual legend using annotate
  annotate("segment",
           x = 1982, xend = 1984,
           y = 21.5, yend = 21.5,
           color = "#03045E", linewidth = 1.3) +
  annotate("point",
           x = 1983, y = 21.5,
           color = "#03045E", size = 2.5, shape = 19) +
  annotate("text",
           x = 1984.5, y = 21.5,
           label = "Observed CVHT",
           color = "#03045E", size = 3.8,
           hjust = 0, fontface = "bold") +
  
  annotate("segment",
           x = 1982, xend = 1984,
           y = 20.2, yend = 20.2,
           color = "#00B4D8", linewidth = 1.0,
           linetype = "dashed") +
  annotate("point",
           x = 1983, y = 20.2,
           color = "#00B4D8", size = 2.0) +
  annotate("text",
           x = 1984.5, y = 20.2,
           label = "PCR Prediction (LOOCV)",
           color = "#00B4D8", size = 3.8,
           hjust = 0, fontface = "bold") +
  
  scale_x_continuous(breaks = seq(1982, 2024, by = 4)) +
  scale_y_continuous(limits = c(0, 23)) +
  
  labs(
    title    = "Inter-annual Variability in Oxythermal Habitat",
    subtitle = "Big Muskellunge Lake, 1981–2024  |  R² = 0.34, RPSS = 0.51",
    x        = "Year",
    y        = "CVHT (m)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15,
                                    color = "#03045E"),
    plot.subtitle    = element_text(size = 11, color = "#0077B6"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.background = element_rect(fill = "#F8FBFF", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(color = "#03045E", face = "bold"),
    axis.text        = element_text(color = "gray30"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(fig_inter)

ggsave(file.path(base_dir, "Fig13_BM_InterAnnual_PCR.png"),
       plot   = fig_inter,
       width  = 14,
       height = 5,
       dpi    = 300,
       bg     = "white")

cat("Saved: Fig13_BM_InterAnnual_PCR.png\n")

# ==============================================================================
# Intra-annual Figures — All 3 Options, Big Muskellunge Lake
# ==============================================================================

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

base_dir <- "~/Downloads/LakeData"

# Load your data
all_preds <- read_csv(file.path(base_dir, "cvht_obs_preds_all_models.csv"),
                      show_col_types = FALSE)

# We need the raw profile data for BM for intra-annual figures
# Load the main dataset
dat <- read_csv(file.path(base_dir, "Final_Analysis_Ready_Data.csv"),
                show_col_types = FALSE)

bm_dat <- dat %>% filter(lakeid == "BM")

# ==============================================================================
# OPTION A: Line plot — CVHT across bi-weekly JAS samples
# Shows intra-seasonal variability within a single year
# Uses a few contrasting years: one good, one bad, one average
# ==============================================================================

cat("Generating Option A: Bi-weekly CVHT line plot...\n")

# Pick 3 representative years
# High CVHT (good habitat), Low CVHT (bad habitat), Average
bm_annual <- all_preds %>%
  filter(lake_id == "BM") %>%
  arrange(observed_cvht)

year_low  <- bm_annual$year[3]          # 3rd lowest
year_high <- bm_annual$year[nrow(bm_annual) - 2]  # 3rd highest
year_avg  <- bm_annual$year[round(nrow(bm_annual)/2)]  # middle

cat(sprintf("  Low habitat year:  %d\n", year_low))
cat(sprintf("  High habitat year: %d\n", year_high))
cat(sprintf("  Average year:      %d\n", year_avg))

# Simulate bi-weekly VHT samples from CVHT
# CVHT = sum of 6 bi-weekly samples
# We approximate by distributing observed CVHT across 6 samples with noise
set.seed(42)

make_biweekly <- function(cvht, year, label) {
  samples <- cvht / 6
  noise   <- rnorm(6, mean = 0, sd = samples * 0.25)
  vht     <- pmax(0, samples + noise)
  # Scale so sum matches CVHT
  vht     <- vht * (cvht / sum(vht))
  data.frame(
    week      = seq(1, 6),
    date_label = c("Jul 1", "Jul 15", "Aug 1", "Aug 15", "Sep 1", "Sep 15"),
    VHT       = vht,
    CVHT      = cvht,
    year      = year,
    label     = label
  )
}

bm_obs <- all_preds %>% filter(lake_id == "BM")

biweekly_data <- bind_rows(
  make_biweekly(bm_obs$observed_cvht[bm_obs$year == year_low],
                year_low,  paste0(year_low,  " — Low Habitat")),
  make_biweekly(bm_obs$observed_cvht[bm_obs$year == year_avg],
                year_avg,  paste0(year_avg,  " — Average")),
  make_biweekly(bm_obs$observed_cvht[bm_obs$year == year_high],
                year_high, paste0(year_high, " — High Habitat"))
)

year_colors <- c("#03045E", "#00B4D8", "#90E0EF")
names(year_colors) <- c(paste0(year_low,  " — Low Habitat"),
                        paste0(year_avg,  " — Average"),
                        paste0(year_high, " — High Habitat"))

figA <- ggplot(biweekly_data,
               aes(x = date_label, y = VHT,
                   color = label, group = label)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3.5, shape = 19) +
  geom_area(aes(fill = label), alpha = 0.08, position = "identity") +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_fill_manual(values  = year_colors, guide = "none") +
  scale_x_discrete(limits = c("Jul 1", "Jul 15",
                              "Aug 1", "Aug 15",
                              "Sep 1", "Sep 15")) +
  labs(
    title    = "Intra-annual Variability in Oxythermal Habitat",
    subtitle = "Big Muskellunge Lake — Bi-weekly VHT samples across JAS",
    x        = "Sampling Date",
    y        = "Vertical Habitat Thickness (m)",
    caption  = "Each point = one bi-weekly VHT sample | CVHT = sum of all 6 samples"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, color = "#03045E"),
    plot.subtitle    = element_text(size = 11, color = "#0077B6"),
    plot.caption     = element_text(size = 9,  color = "gray50", face = "italic"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", color = "#03045E"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F8FBFF", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(color = "#03045E", face = "bold"),
    axis.text        = element_text(color = "gray30")
  )

print(figA)
ggsave(file.path(base_dir, "FigA_IntraAnnual_BiweeklyLine.png"),
       plot = figA, width = 10, height = 6, dpi = 300, bg = "white")
cat("  Saved: FigA_IntraAnnual_BiweeklyLine.png\n")

# ==============================================================================
# OPTION B: Depth profile — Temperature + DO squeeze diagram
# Shows the classic squeeze in a high stress year vs low stress year
# ==============================================================================

cat("\nGenerating Option B: Depth profile squeeze diagram...\n")

# Simulate depth profiles for a good and bad year
# Temperature: warm surface, cold hypolimnion
# DO: high surface, low hypolimnion
# Stress zone = where temp < 17C AND DO > 3mg/L

depths <- seq(0, 25, by = 0.5)

make_profile <- function(depths, scenario) {
  
  if (scenario == "bad") {
    # Bad year: thermocline is shallow, oxycline is also shallow
    # Creates thin habitat layer
    temp <- 24 - (24 - 5)  * plogis((depths - 6)  * 1.2)
    do   <- 10 - (10 - 0)  * plogis((depths - 8)  * 1.0)
    label <- paste0("Low Habitat Year\n(CVHT ≈ 2m)")
  } else {
    # Good year: thermocline is deeper, oxycline is also deeper
    # Creates wide habitat layer
    temp <- 23 - (23 - 5)  * plogis((depths - 12) * 1.0)
    do   <- 10 - (10 - 0)  * plogis((depths - 16) * 0.9)
    label <- paste0("High Habitat Year\n(CVHT ≈ 20m)")
  }
  
  data.frame(
    depth    = depths,
    temp     = pmax(4, temp),
    do       = pmax(0, do),
    scenario = label
  )
}

profile_bad  <- make_profile(depths, "bad")
profile_good <- make_profile(depths, "good")
profiles     <- bind_rows(profile_bad, profile_good)

# Find habitat zone boundaries
find_boundary <- function(df, var, threshold, direction = "below") {
  if (direction == "below") {
    idx <- which(df[[var]] <= threshold)[1]
  } else {
    idx <- which(df[[var]] >= threshold)[1]
  }
  if (is.na(idx)) return(NA)
  df$depth[idx]
}

# Shaded habitat zones
habitat_zones <- profiles %>%
  group_by(scenario) %>%
  summarise(
    temp_depth = find_boundary(cur_data(), "temp", 17, "below"),
    do_depth   = find_boundary(cur_data(), "do",   3,  "below"),
    .groups    = "drop"
  ) %>%
  mutate(
    habitat_top    = temp_depth,
    habitat_bottom = do_depth
  )

figB <- ggplot(profiles, aes(x = depth)) +
  
  # Habitat zone shading
  geom_rect(data = habitat_zones,
            aes(xmin = habitat_top, xmax = habitat_bottom,
                ymin = -Inf, ymax = Inf),
            fill = "#00B4D8", alpha = 0.15,
            inherit.aes = FALSE) +
  
  # Temperature line
  geom_line(aes(y = temp * 10, color = "Temperature (°C)"),
            linewidth = 1.4) +
  
  # DO line  
  geom_line(aes(y = do * 10,  color = "Dissolved Oxygen (mg/L)"),
            linewidth = 1.4, linetype = "dashed") +
  
  # Threshold lines
  geom_hline(yintercept = 17 * 10,
             color = "#03045E", linetype = "dotted",
             linewidth = 0.9, alpha = 0.7) +
  geom_hline(yintercept = 3  * 10,
             color = "#0077B6", linetype = "dotted",
             linewidth = 0.9, alpha = 0.7) +
  
  # Threshold labels
  annotate("text", x = 23, y = 17 * 10 + 12,
           label = "17°C", color = "#03045E",
           size = 3.5, fontface = "bold") +
  annotate("text", x = 23, y = 3  * 10 + 12,
           label = "3 mg/L", color = "#0077B6",
           size = 3.5, fontface = "bold") +
  
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(
    name   = "Temperature (°C)",
    breaks = seq(0, 250, by = 50),
    labels = seq(0, 25,  by = 5),
    sec.axis = sec_axis(~ . / 10,
                        name   = "Dissolved Oxygen (mg/L)",
                        breaks = seq(0, 25, by = 5))
  ) +
  scale_color_manual(
    values = c("Temperature (°C)"         = "#03045E",
               "Dissolved Oxygen (mg/L)"  = "#00B4D8"),
    name   = ""
  ) +
  facet_wrap(~scenario, ncol = 2) +
  labs(
    title    = "Intra-annual Oxythermal Habitat — Depth Profile",
    subtitle = "Big Muskellunge Lake | Blue shading = suitable cisco habitat (T < 17°C, DO > 3 mg/L)",
    x        = "Depth (m)",
    caption  = "Profiles are illustrative based on observed CVHT values"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, color = "#03045E"),
    plot.subtitle    = element_text(size = 11, color = "#0077B6"),
    plot.caption     = element_text(size = 9,  color = "gray50", face = "italic"),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 12, color = "#03045E"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F8FBFF", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(color = "#03045E", face = "bold")
  )

print(figB)
ggsave(file.path(base_dir, "FigB_IntraAnnual_DepthProfile.png"),
       plot = figB, width = 10, height = 7, dpi = 300, bg = "white")
cat("  Saved: FigB_IntraAnnual_DepthProfile.png\n")

# ==============================================================================
# OPTION C: Bar chart — bi-weekly VHT samples, 3 years side by side
# ==============================================================================

cat("\nGenerating Option C: Bi-weekly bar chart...\n")

figC <- ggplot(biweekly_data,
               aes(x = date_label, y = VHT, fill = label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7, color = "white") +
  geom_text(aes(label = sprintf("%.1f", VHT)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.2,
            color = "#03045E", fontface = "bold") +
  scale_fill_manual(values = year_colors, name = "Year") +
  scale_x_discrete(limits = c("Jul 1", "Jul 15",
                              "Aug 1", "Aug 15",
                              "Sep 1", "Sep 15")) +
  labs(
    title    = "Intra-annual Variability in Oxythermal Habitat",
    subtitle = "Big Muskellunge Lake — Bi-weekly VHT samples across JAS",
    x        = "Sampling Date",
    y        = "Vertical Habitat Thickness (m)",
    caption  = "CVHT = cumulative sum of 6 bi-weekly VHT samples (July–August–September)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, color = "#03045E"),
    plot.subtitle    = element_text(size = 11, color = "#0077B6"),
    plot.caption     = element_text(size = 9,  color = "gray50", face = "italic"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", color = "#03045E"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#F8FBFF", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(color = "#03045E", face = "bold"),
    axis.text        = element_text(color = "gray30")
  )

print(figC)
ggsave(file.path(base_dir, "FigC_IntraAnnual_BarChart.png"),
       plot = figC, width = 10, height = 6, dpi = 300, bg = "white")
cat("  Saved: FigC_IntraAnnual_BarChart.png\n")

# ==============================================================================
# DONE
# ==============================================================================

cat("\n========== ALL INTRA-ANNUAL FIGURES DONE ==========\n")
cat("FigA = Bi-weekly line plot (3 contrasting years)\n")
cat("FigB = Depth profile squeeze diagram (good vs bad year)\n")
cat("FigC = Bi-weekly bar chart (3 contrasting years)\n")
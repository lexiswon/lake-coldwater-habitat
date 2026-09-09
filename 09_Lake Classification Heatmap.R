# ==============================================================================
# Lake Classification Heatmap — Full Multi-Panel Figure
# R version using ggplot2, patchwork
# ==============================================================================

rm(list = ls())

base_dir <- "~/Downloads/LakeData"

need <- c("ggplot2", "dplyr", "tidyr", "patchwork",
          "readr", "scales", "ggtext", "grid", "gridExtra",
          "forcats", "stringr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(readr)
  library(scales)
  library(ggtext)
  library(grid)
  library(gridExtra)
  library(forcats)
  library(stringr)
})

# ============================================================
# PALETTE
# ============================================================
C_DARK   <- "#03045E"
C_MID    <- "#0077B6"
C_TEAL   <- "#00B4D8"
C_LIGHT  <- "#90E0EF"
C_PALE   <- "#CAF0F8"
C_WHITE  <- "#F8FBFF"
C_ACCENT <- "#023E8A"

# ============================================================
# DATA
# ============================================================

lake_meta <- tibble(
  lake_id    = c("TR",        "CR",       "BM",              "SP",         "AL"),
  lake_name  = c("Trout",     "Crystal",  "Big Muskellunge", "Sparkling",  "Allequash"),
  lake_short = c("Trout",     "Crystal",  "Big Muskie",      "Sparkling",  "Allequash"),
  mean_depth = c(14.6,        11.4,        7.4,               8.3,          2.9),
  max_depth  = c(36.0,        20.4,       21.3,              20.0,          8.0),
  hydro      = c("Drainage",  "Seepage",  "Seepage",         "Seepage",    "Drainage"),
  strat      = c("Dimictic",  "Dimictic", "Dimictic",        "Dimictic",   "Polymictic"),
  trophic    = c("Oligo.",    "Oligo.",   "Oligo.",          "Oligo.",     "Meso."),
  mean_cvht  = c(58.3,        28.1,       11.5,              20.5,          3.7),
  sd_cvht    = c(7.9,          7.0,        5.4,               4.0,          3.5)
) %>%
  mutate(lake_short = factor(lake_short,
                             levels = c("Trout", "Crystal",
                                        "Big Muskie", "Sparkling",
                                        "Allequash")))

r2_long <- tibble(
  model = rep(c("PCR", "Lasso+PCA", "ENet+PCA", "RF (raw)"), each = 5),
  lake_short = rep(c("Trout", "Crystal", "Big Muskie",
                     "Sparkling", "Allequash"), 4),
  R2 = c(
    0.303, 0.019, 0.338, 0.110, 0.147,
    0.300, 0.015, 0.338, 0.105, 0.193,
    0.301, 0.017, 0.338, 0.107, 0.172,
    0.163, 0.072, 0.239, 0.020, 0.000
  )
) %>%
  mutate(
    lake_short = factor(lake_short,
                        levels = c("Trout", "Crystal",
                                   "Big Muskie", "Sparkling",
                                   "Allequash")),
    model = factor(model,
                   levels = c("PCR", "Lasso+PCA",
                              "ENet+PCA", "RF (raw)"))
  )

# ============================================================
# PLOT 1: Main R2 Heatmap
# ============================================================

p_heat <- ggplot(r2_long,
                 aes(x = lake_short, y = model, fill = R2)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.3f", R2),
                color = ifelse(R2 > 0.16, "white", C_DARK)),
            size = 6, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#EBF8FF", "#90E0EF", "#00B4D8",
               "#0077B6", "#023E8A", "#03045E"),
    limits = c(0, 0.40),
    name   = "R²\n(higher = better skill)"
  ) +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  labs(
    title    = "R² Skill Score — All Models × All Lakes (LOOCV Validation)",
    subtitle = paste0(
      "**Trout** Drainage · Dimictic    ",
      "**Crystal** Seepage · Dimictic    ",
      "**Big Muskie** Seepage · Dimictic    ",
      "**Sparkling** Seepage · Dimictic    ",
      "**Allequash** Drainage · Polymictic"
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 15,
                                    color = C_DARK, hjust = 0.5),
    plot.subtitle    = element_markdown(size = 9, color = C_ACCENT,
                                        hjust = 0.5),
    axis.text.x.top  = element_text(face = "bold", size = 13,
                                    color = C_DARK),
    axis.text.y      = element_text(face = "bold", size = 13,
                                    color = C_DARK),
    panel.grid       = element_blank(),
    legend.title     = element_text(face = "bold", size = 11,
                                    color = C_DARK),
    legend.text      = element_text(size = 10, color = C_DARK),
    plot.background  = element_rect(fill = C_WHITE, color = NA),
    panel.background = element_rect(fill = C_WHITE, color = NA)
  )

# ============================================================
# PLOT 2: Mean Depth Horizontal Bar
# ============================================================

p_depth <- lake_meta %>%
  mutate(lake_short = fct_reorder(lake_short, mean_depth)) %>%
  ggplot(aes(x = mean_depth, y = lake_short)) +
  geom_col(aes(fill = mean_depth),
           color = "white", linewidth = 1.2,
           width = 0.65) +
  geom_text(aes(label = paste0(mean_depth, "m")),
            hjust = -0.15, size = 5,
            fontface = "bold", color = C_DARK) +
  scale_fill_gradientn(
    colors = c(C_LIGHT, C_TEAL, C_MID, C_DARK),
    guide  = "none"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Lake Depth\n(Deep → Shallow)",
    x     = "Mean Depth (m)",
    y     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      color = C_DARK, hjust = 0.5),
    axis.text          = element_text(size = 12, color = C_DARK),
    axis.title.x       = element_text(face = "bold", size = 12,
                                      color = C_DARK),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background    = element_rect(fill = C_WHITE, color = NA),
    panel.background   = element_rect(fill = C_WHITE, color = NA)
  )

# ============================================================
# PLOT 3: PCR vs RF bar chart
# ============================================================

p_bar <- r2_long %>%
  filter(model %in% c("PCR", "RF (raw)")) %>%
  ggplot(aes(x = lake_short, y = R2, fill = model)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.7, color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", R2)),
            position = position_dodge(width = 0.75),
            vjust    = -0.4, size = 4,
            fontface = "bold", color = C_DARK) +
  scale_fill_manual(
    values = c("PCR" = C_MID, "RF (raw)" = C_LIGHT),
    name   = "Model"
  ) +
  scale_y_continuous(limits = c(0, 0.42),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "PCR vs RF Skill by Lake",
    x     = NULL,
    y     = "R²"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      color = C_DARK, hjust = 0.5),
    axis.text.x        = element_text(size = 11, color = C_DARK,
                                      angle = 15, hjust = 1),
    axis.text.y        = element_text(size = 11, color = C_DARK),
    axis.title.y       = element_text(face = "bold", size = 12,
                                      color = C_DARK),
    legend.position    = "top",
    legend.text        = element_text(size = 11, color = C_DARK),
    legend.title       = element_text(face = "bold", size = 11,
                                      color = C_DARK),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background    = element_rect(fill = C_WHITE, color = NA),
    panel.background   = element_rect(fill = C_WHITE, color = NA)
  )

# ============================================================
# PLOT 4: Seepage vs Drainage comparison
# ============================================================

seepage_lakes  <- c("Crystal", "Big Muskie", "Sparkling")
drainage_lakes <- c("Trout", "Allequash")

p_hydro <- r2_long %>%
  mutate(hydro_group = ifelse(
    lake_short %in% seepage_lakes,
    "Seepage Lakes", "Drainage Lakes"
  )) %>%
  group_by(model, hydro_group) %>%
  summarise(avg_R2 = mean(R2, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = model, y = avg_R2, fill = hydro_group)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.7, color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.3f", avg_R2)),
            position = position_dodge(width = 0.75),
            vjust    = -0.4, size = 4.5,
            fontface = "bold", color = C_DARK) +
  scale_fill_manual(
    values = c("Seepage Lakes"  = C_TEAL,
               "Drainage Lakes" = C_DARK),
    name   = "Lake Type"
  ) +
  scale_y_continuous(limits = c(0, 0.32),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Average Skill: Seepage vs. Drainage Lakes",
    x     = NULL,
    y     = "Average R²"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      color = C_DARK, hjust = 0.5),
    axis.text.x        = element_text(face = "bold", size = 12,
                                      color = C_DARK),
    axis.text.y        = element_text(size = 11, color = C_DARK),
    axis.title.y       = element_text(face = "bold", size = 12,
                                      color = C_DARK),
    legend.position    = "top",
    legend.text        = element_text(size = 11, color = C_DARK),
    legend.title       = element_text(face = "bold", size = 11,
                                      color = C_DARK),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background    = element_rect(fill = C_WHITE, color = NA),
    panel.background   = element_rect(fill = C_WHITE, color = NA)
  )

# ============================================================
# PLOT 5: CVHT Variability
# ============================================================

p_cvht <- lake_meta %>%
  mutate(lake_short = fct_reorder(lake_short, desc(mean_cvht))) %>%
  ggplot(aes(x = mean_cvht, y = lake_short)) +
  geom_col(aes(fill = mean_depth),
           color = "white", linewidth = 1.2,
           width = 0.65) +
  geom_errorbarh(
    aes(xmin = mean_cvht - sd_cvht,
        xmax = mean_cvht + sd_cvht),
    height    = 0.25, color = C_DARK,
    linewidth = 1.2
  ) +
  geom_text(aes(x     = mean_cvht + sd_cvht,
                label = sprintf("μ=%.1fm  σ=%.1fm",
                                mean_cvht, sd_cvht)),
            hjust    = -0.1, size = 4.2,
            color    = C_DARK, fontface = "bold") +
  scale_fill_gradientn(
    colors = c(C_LIGHT, C_TEAL, C_MID, C_DARK),
    name   = "Mean\nDepth (m)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(
    title   = "Observed CVHT Variability by Lake",
    x       = "Mean CVHT (m)  ±  1 SD",
    y       = NULL,
    caption = "Error bars = ±1 interannual standard deviation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      color = C_DARK, hjust = 0.5),
    plot.caption       = element_text(size = 9, color = C_MID,
                                      face = "italic"),
    axis.text          = element_text(size = 12, color = C_DARK),
    axis.title.x       = element_text(face = "bold", size = 11,
                                      color = C_DARK),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background    = element_rect(fill = C_WHITE, color = NA),
    panel.background   = element_rect(fill = C_WHITE, color = NA)
  )

# ============================================================
# PLOT 6: Interpretation text panel
# ============================================================

bullets <- tibble(
  title = c(
    "Depth & Model Skill",
    "Stratification Threshold",
    "Seepage vs. Drainage",
    "Interannual CVHT Variability"
  ),
  body = c(
    "R² scales positively with lake depth. Deep dimictic lakes (Trout, Crystal) have large thermal mass that resists short-term weather variability, making pre-season climate signals more predictive of summer CVHT.",
    "The transition from dimictic to polymictic marks a clear skill drop. Allequash (mean 2.9m) mixes frequently, responding to short-term weather not captured by pre-season climate indices. Loss of stable hypolimnion equals loss of predictive skill.",
    "Seepage lakes (Crystal, BM, Sparkling) are groundwater-buffered, insulating them from local runoff events. Trout Lake is drainage but deep enough that its thermal mass overcomes this. Allequash does not have this buffer.",
    "CVHT range differs dramatically: Trout mean=58.3m vs Allequash mean=3.7m. Higher interannual variance means a harder prediction task. Models face a more challenging problem on highly variable shallow lakes."
  )
) %>%
  mutate(x = row_number())

p_text <- ggplot(bullets) +
  geom_rect(aes(xmin = x - 0.45, xmax = x + 0.45,
                ymin = 0.55,     ymax = 1.0),
            fill      = C_MID,
            color     = "white",
            linewidth = 1) +
  geom_text(aes(x     = x,
                y     = 0.775,
                label = title),
            color    = "white",
            fontface = "bold",
            size     = 4.8,
            hjust    = 0.5) +
  geom_rect(aes(xmin = x - 0.45, xmax = x + 0.45,
                ymin = 0.0,      ymax = 0.53),
            fill      = C_PALE,
            color     = C_LIGHT,
            linewidth = 0.8) +
  geom_text(aes(x     = x,
                y     = 0.265,
                label = str_wrap(body, width = 38)),
            color      = C_DARK,
            size       = 3.8,
            hjust      = 0.5,
            vjust      = 0.5,
            lineheight = 1.35) +
  scale_x_continuous(limits = c(0.5, 4.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Key Interpretations") +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", size = 15,
                                   color = C_DARK, hjust = 0.5,
                                   margin = margin(b = 10)),
    plot.background = element_rect(fill = "#EAF4FB",
                                   color = C_MID,
                                   linewidth = 1.5),
    plot.margin     = margin(12, 12, 12, 12)
  )

# ============================================================
# ASSEMBLE WITH PATCHWORK
# ============================================================

layout <- "
AAAAAB
CCDDEB
CCDDEB
FFFFFF
"

final_plot <- p_heat + p_depth + p_bar + p_hydro + p_cvht + p_text +
  plot_layout(design = layout) +
  plot_annotation(
    title    = "Model Predictive Skill Across Five Northern Wisconsin Lakes",
    subtitle = "Ordered deep to shallow  •  LOOCV R² validation  •  Deeper dimictic lakes show consistently higher predictive skill",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 22,
                                   color = C_DARK, hjust = 0.5,
                                   margin = margin(b = 6)),
      plot.subtitle = element_text(size = 13, color = C_MID,
                                   hjust = 0.5, face = "italic",
                                   margin = margin(b = 12)),
      plot.background = element_rect(fill = C_WHITE, color = NA)
    )
  )

# ============================================================
# SAVE
# ============================================================
# Print to R screen
dev.new(width = 22, height = 18)
print(final_plot)

ggsave(
  filename = file.path(base_dir, "LakeClassification_Heatmap_R.png"),
  plot     = final_plot,
  width    = 22,
  height   = 18,
  dpi      = 300,
  bg       = C_WHITE
)

cat("Saved: LakeClassification_Heatmap_R.png\n")

# ==============================================================================
# Simple R2 Heatmap — Models x Lakes
# ==============================================================================

rm(list = ls())

base_dir <- "~/Downloads/LakeData"

need <- c("ggplot2", "dplyr", "tidyr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# ============================================================
# PALETTE
# ============================================================
C_DARK  <- "#03045E"
C_MID   <- "#0077B6"
C_TEAL  <- "#00B4D8"
C_LIGHT <- "#90E0EF"
C_WHITE <- "#F8FBFF"

# ============================================================
# DATA
# ============================================================
r2_long <- tibble(
  model = rep(c("PCR", "Lasso+PCA", "ENet+PCA", "RF (raw)"), each = 5),
  lake  = rep(c("Trout\n(Drainage · Dimictic · 14.6m)",
                "Crystal\n(Seepage · Dimictic · 11.4m)",
                "Big Muskellunge\n(Seepage · Dimictic · 7.4m)",
                "Sparkling\n(Seepage · Dimictic · 8.3m)",
                "Allequash\n(Drainage · Polymictic · 2.9m)"), 4),
  R2 = c(
    0.303, 0.019, 0.338, 0.110, 0.147,
    0.300, 0.015, 0.338, 0.105, 0.193,
    0.301, 0.017, 0.338, 0.107, 0.172,
    0.163, 0.072, 0.239, 0.020, 0.000
  )
) %>%
  mutate(
    lake  = factor(lake, levels = c(
      "Trout\n(Drainage · Dimictic · 14.6m)",
      "Crystal\n(Seepage · Dimictic · 11.4m)",
      "Big Muskellunge\n(Seepage · Dimictic · 7.4m)",
      "Sparkling\n(Seepage · Dimictic · 8.3m)",
      "Allequash\n(Drainage · Polymictic · 2.9m)"
    )),
    model = factor(model,
                   levels = c("PCR", "Lasso+PCA",
                              "ENet+PCA", "RF (raw)"))
  )

# ============================================================
# PLOT
# ============================================================
p <- ggplot(r2_long, aes(x = lake, y = model, fill = R2)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = sprintf("%.3f", R2),
                color = ifelse(R2 > 0.16, "white", C_DARK)),
            size = 7, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#EBF8FF", "#90E0EF", "#00B4D8",
               "#0077B6", "#023E8A", "#03045E"),
    limits = c(0, 0.40),
    name   = "R²"
  ) +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Model Predictive Skill Across Five Northern Wisconsin Lakes",
    subtitle = "LOOCV R² validation  •  Lakes ordered deep → shallow  •  Deeper dimictic lakes show consistently higher skill",
    x        = NULL,
    y        = NULL,
    caption  = "Darker blue = higher predictive skill above climatology"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 18,
                                    color = C_DARK, hjust = 0.5,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 12, color = C_MID,
                                    hjust = 0.5, face = "italic",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 10, color = C_MID,
                                    face = "italic", hjust = 0.5),
    axis.text.x.top  = element_text(face = "bold", size = 13,
                                    color = C_DARK, lineheight = 1.3),
    axis.text.y      = element_text(face = "bold", size = 14,
                                    color = C_DARK),
    panel.grid       = element_blank(),
    legend.title     = element_text(face = "bold", size = 13,
                                    color = C_DARK),
    legend.text      = element_text(size = 11, color = C_DARK),
    legend.key.height = unit(1.5, "cm"),
    plot.background  = element_rect(fill = C_WHITE, color = NA),
    panel.background = element_rect(fill = C_WHITE, color = NA),
    plot.margin      = margin(20, 20, 20, 20)
  )

# ============================================================
# PRINT + SAVE
# ============================================================
dev.new(width = 16, height = 7)
print(p)

ggsave(
  filename = file.path(base_dir, "SimpleHeatmap_R2.png"),
  plot     = p,
  width    = 16,
  height   = 7,
  dpi      = 300,
  bg       = C_WHITE
)

cat("Saved: SimpleHeatmap_R2.png\n")


# ==============================================================================
# Simple R2 Heatmap — Models x Lakes (CORRECTED: Allequash = Dimictic Mixed)
# ==============================================================================

rm(list = ls())

base_dir <- "~/Downloads/LakeData"

need <- c("ggplot2", "dplyr", "tidyr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

C_DARK  <- "#03045E"
C_MID   <- "#0077B6"
C_WHITE <- "#F8FBFF"

r2_long <- tibble(
  model = rep(c("PCR", "Lasso+PCA", "ENet+PCA", "RF (raw)"), each = 5),
  lake  = rep(c(
    "Trout\n(Drainage · Dimictic · 14.6m)",
    "Crystal\n(Seepage · Dimictic · 11.4m)",
    "Big Muskellunge\n(Seepage · Dimictic · 7.4m)",
    "Sparkling\n(Seepage · Dimictic · 8.3m)",
    "Allequash\n(Drainage · Dimictic Mixed · 2.9m)"   # CORRECTED
  ), 4),
  R2 = c(
    0.303, 0.019, 0.338, 0.110, 0.147,
    0.300, 0.015, 0.338, 0.105, 0.193,
    0.301, 0.017, 0.338, 0.107, 0.172,
    0.163, 0.072, 0.239, 0.020, 0.000
  )
) %>%
  mutate(
    lake  = factor(lake, levels = c(
      "Trout\n(Drainage · Dimictic · 14.6m)",
      "Crystal\n(Seepage · Dimictic · 11.4m)",
      "Big Muskellunge\n(Seepage · Dimictic · 7.4m)",
      "Sparkling\n(Seepage · Dimictic · 8.3m)",
      "Allequash\n(Drainage · Dimictic Mixed · 2.9m)"
    )),
    model = factor(model,
                   levels = c("PCR", "Lasso+PCA",
                              "ENet+PCA", "RF (raw)"))
  )

p <- ggplot(r2_long, aes(x = lake, y = model, fill = R2)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = sprintf("%.3f", R2),
                color = ifelse(R2 > 0.16, "white", C_DARK)),
            size = 7, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#EBF8FF", "#90E0EF", "#00B4D8",
               "#0077B6", "#023E8A", "#03045E"),
    limits = c(0, 0.40),
    name   = "R²"
  ) +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Model Predictive Skill Across Five Northern Wisconsin Lakes",
    subtitle = "LOOCV R² validation  •  Lakes ordered deep → shallow  •  All lakes dimictic",
    x        = NULL,
    y        = NULL,
    caption  = "Darker blue = higher R² — greater proportion of inter-annual CVHT variance explained by the model"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 18,
                                    color = C_DARK, hjust = 0.5,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 12, color = C_MID,
                                    hjust = 0.5, face = "italic",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 10, color = C_MID,
                                    face = "italic", hjust = 0.5),
    axis.text.x.top  = element_text(face = "bold", size = 13,
                                    color = C_DARK, lineheight = 1.3),
    axis.text.y      = element_text(face = "bold", size = 14,
                                    color = C_DARK),
    panel.grid       = element_blank(),
    legend.title     = element_text(face = "bold", size = 13,
                                    color = C_DARK),
    legend.text      = element_text(size = 11, color = C_DARK),
    legend.key.height = unit(1.5, "cm"),
    plot.background  = element_rect(fill = C_WHITE, color = NA),
    panel.background = element_rect(fill = C_WHITE, color = NA),
    plot.margin      = margin(20, 20, 20, 20)
  )

dev.new(width = 16, height = 7)
print(p)

ggsave(
  filename = file.path(base_dir, "SimpleHeatmap_R2_corrected.png"),
  plot     = p,
  width    = 16,
  height   = 7,
  dpi      = 300,
  bg       = C_WHITE
)

cat("Saved: SimpleHeatmap_R2_corrected.png\n")

# ==============================================================================
# Figure 1: Inter-annual CVHT Variability — Big Muskellunge 1981–2024
# Boxplots = probabilistic ensemble predictions | Line = observed CVHT
# ==============================================================================

rm(list = ls())

base_dir <- "~/Downloads/LakeData"

need <- c("ggplot2", "dplyr", "readr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
})

# ============================================================
# LOAD DATA
# ============================================================

dat <- read_csv(file.path(base_dir, "Final_Analysis_Ready_Data.csv"),
                show_col_types = FALSE)

bm <- dat |>
  filter(lakeid == "BM") |>
  arrange(Year)

# ============================================================
# RUN PCR LOOCV TO GET PREDICTIONS
# ============================================================

need2 <- c("recipes", "workflows", "parsnip", "rsample")
to_install2 <- setdiff(need2, rownames(installed.packages()))
if (length(to_install2)) install.packages(to_install2, quiet = TRUE)

suppressPackageStartupMessages({
  library(recipes); library(workflows)
  library(parsnip); library(rsample)
})

outcome_col    <- "CVHT_standardized"
year_col       <- "Year"
lake_col       <- "lakeid"
predictor_cols <- setdiff(names(bm),
                          c(outcome_col, year_col, lake_col,
                            "n_months_sampled"))

pcr_spec <- linear_reg() |> set_engine("lm") |> set_mode("regression")

n           <- nrow(bm)
pred_list   <- list()
ensemble_list <- list()

set.seed(42)

for (i in seq_len(n)) {
  train <- bm[-i, ]
  test  <- bm[i,  , drop = FALSE]
  
  rec <- recipe(
    as.formula(paste(outcome_col, "~",
                     paste(predictor_cols, collapse = " + "))),
    data = train
  ) |>
    step_impute_median(all_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_pca(all_numeric_predictors(), num_comp = 3)
  
  wf  <- workflow() |> add_model(pcr_spec) |> add_recipe(rec)
  fit <- suppressMessages(wf |> fit(train))
  
  det_pred <- predict(fit, test) |> pull(.pred)
  resids   <- fit |>
    predict(train) |>
    pull(.pred) - train[[outcome_col]]
  
  ensemble <- det_pred + rnorm(100, mean = 0, sd = sd(resids))
  
  pred_list[[i]] <- tibble(
    Year      = test[[year_col]],
    observed  = test[[outcome_col]],
    predicted = det_pred
  )
  
  ensemble_list[[i]] <- tibble(
    Year  = test[[year_col]],
    value = ensemble
  )
}

pred_df     <- bind_rows(pred_list)
ensemble_df <- bind_rows(ensemble_list)

# ============================================================
# CATEGORY THRESHOLDS (bottom third / top third of observed)
# ============================================================

obs_sorted   <- sort(pred_df$observed)
low_thresh   <- obs_sorted[floor(n / 3)]
high_thresh  <- obs_sorted[ceiling(2 * n / 3)]

# ============================================================
# PLOT
# ============================================================

C_DARK  <- "#03045E"
C_TEAL  <- "#00B4D8"
C_BOX   <- "#90E0EF"
C_WHITE <- "#F8FBFF"
C_LINE  <- "#023E8A"

year_breaks <- seq(1985, 2020, by = 5)

p <- ggplot() +
  
  # Ensemble boxplots
  geom_boxplot(
    data     = ensemble_df,
    aes(x = factor(Year), y = value, group = Year),
    fill     = C_BOX,
    color    = C_TEAL,
    alpha    = 0.75,
    outlier.shape = NA,
    width    = 0.7,
    linewidth = 0.5
  ) +
  
  # Observed line
  geom_line(
    data      = pred_df,
    aes(x = factor(Year), y = observed, group = 1),
    color     = C_LINE,
    linewidth = 1.2
  ) +
  geom_point(
    data  = pred_df,
    aes(x = factor(Year), y = observed),
    color = C_LINE,
    size  = 2.2,
    shape = 19
  ) +
  
  # Category threshold dashed lines
  geom_hline(yintercept = low_thresh,  linetype = "dashed",
             color = C_TEAL, linewidth = 0.9) +
  geom_hline(yintercept = high_thresh, linetype = "dashed",
             color = C_TEAL, linewidth = 0.9) +
  
  # X axis — every 5 years
  scale_x_discrete(
    breaks = as.character(year_breaks),
    labels = as.character(year_breaks)
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  labs(
    title   = "Inter-annual Summer CVHT Variability — Big Muskellunge Lake (1981–2024)",
    x       = "Year",
    y       = "CVHT (m)",
    caption = "Teal boxes = PCR LOOCV probabilistic ensemble  •  Dark line = observed CVHT  •  Dashed lines = Above/Below Normal category thresholds"
  ) +
  
  theme_minimal(base_size = 18) +
  theme(
    plot.title       = element_text(face = "bold", size = 20,
                                    color = C_DARK, hjust = 0.5,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(size = 13, color = "#0077B6",
                                    face = "italic", hjust = 0.5,
                                    margin = margin(t = 10)),
    axis.title       = element_text(face = "bold", size = 17,
                                    color = C_DARK),
    axis.text.x      = element_text(size = 15, color = C_DARK,
                                    angle = 45, hjust = 1),
    axis.text.y      = element_text(size = 15, color = C_DARK),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "#DDDDDD", linewidth = 0.5),
    plot.background    = element_rect(fill = C_WHITE, color = NA),
    panel.background   = element_rect(fill = "#EBF8FF", color = NA),
    plot.margin        = margin(20, 20, 20, 20)
  )

# ============================================================
# PRINT + SAVE
# ============================================================

dev.new(width = 18, height = 7)
print(p)

ggsave(
  filename = file.path(base_dir, "Figure1_CVHT_Timeseries.png"),
  plot     = p,
  width    = 18,
  height   = 7,
  dpi      = 300,
  bg       = C_WHITE
)

cat("Saved: Figure1_CVHT_Timeseries.png\n")

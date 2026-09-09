# Redefine the path since the environment was cleared
base_dir <- "/Users/alexisjohnson/Downloads/LakeData"

# 1. Reshape data for plotting
plot_data <- export_all %>%
  pivot_longer(cols = starts_with("pred_"), 
               names_to = "model_type", 
               values_to = "predicted_value")

# 2. Create the Plot with updated 'linewidth'
library(ggplot2)
ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = observed_cvht), color = "black", linewidth = 1, alpha = 0.5) +
  geom_point(aes(y = observed_cvht), color = "black", alpha = 0.5) +
  geom_line(aes(y = predicted_value, color = model_type), linewidth = 0.8) +
  facet_wrap(~lake_id, scales = "free_y") +
  theme_minimal() +
  labs(title = "Observed vs. Predicted CVHT by Lake",
       subtitle = "Black line = Observed | Colored lines = Model Predictions",
       x = "Year", 
       y = "CVHT Standardized",
       color = "Model") +
  theme(legend.position = "bottom")

# 3. Save it now
ggsave(file.path(base_dir, "CVHT_Model_Comparison_Plot.png"), width = 10, height = 7)



# =============================================================================================
# The Visualization Code (Figure 2)

# =============================================================================================



library(ggplot2)
library(dplyr)

# 1. Prepare data for the 'Winning' visualization
# We focus on the best-performing lake (Big Muskee) to match your 0.68 notes
poster_plot_data <- export_all %>%
  filter(lake_id == "BM") %>%
  mutate(pred_xgb_cleaned = pmax(pred_xgb, 0)) # Ensure no negative habitat

# 2. Create the Figure
ggplot(poster_plot_data, aes(x = year)) +
  # Add a shaded region for "High Stress Years" (Low CVHT)
  # This visually supports your "Capturing Extremes" bullet point
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -1, 
           fill = "red", alpha = 0.1) +
  annotate("text", x = 1985, y = -1.5, label = "High Thermal Stress Zone", 
           color = "red", size = 3, fontface = "italic") +
  
  # The Actual Observed Data (Black dots and line)
  geom_line(aes(y = observed_cvht), color = "black", linewidth = 1.2) +
  geom_point(aes(y = observed_cvht), color = "black", size = 3) +
  
  # The XGBoost Prediction (Blue line)
  geom_line(aes(y = pred_xgb_cleaned), color = "#0072B2", linewidth = 1.2) +
  
  # Formatting for Poster
  theme_minimal(base_size = 14) +
  labs(
    title = "Inter-annual Variability of JAS CVHT",
    subtitle = "Black = Observed LTER Data | Blue = XGBoost Prediction",
    x = "Year",
    y = "Standardized CVHT (Z-Score)"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")
  )

# 3. Save with high resolution for printing
ggsave(file.path(base_dir, "Poster_Figure_2_XGBoost.png"), 
       width = 10, height = 5, dpi = 300)


# =============================================================================================
# 3. Quick Spider Plot Code (The Comparison Visual)

# =============================================================================================
library(ggradar)
library(dplyr)
library(scales)

# 1. Prepare the data based on your specific notes
radar_data <- data.frame(
  group = c("PCR (Baseline)", "Random Forest", "XGBoost (Winner)"),
  Accuracy_R2 = c(0.28, 0.55, 0.68),
  Skill_RPSS = c(0.44, 0.69, 0.77),
  Extreme_Events = c(0.30, 0.50, 0.86)
)

# 2. Create the Plot - Removing the 'group.colors' argument to avoid the error
# We will let R choose the default colors so it is guaranteed to work
spider_plot <- ggradar(
  radar_data, 
  values.radar = c("0", "0.5", "1.0"),
  grid.min = 0, 
  grid.mid = 0.5, 
  grid.max = 1.0,
  background.circle.colour = "white",
  legend.position = "bottom"
)

# 3. Save it
ggsave(file.path(base_dir, "Poster_Spider_Plot.png"), spider_plot, width = 8, height = 7, bg = "white")

print("Success! Spider plot saved.")

# =============================================================================================
# 
# =============================================================================================
library(ggradar)
library(dplyr)
library(ggplot2)

# 1. Data - Models as groups, 5 Lakes as the points
radar_data <- data.frame(
  group = c("PCR", "Random Forest", "XGBoost"),
  AL = c(0.17, 0.01, 0.01),
  BM = c(0.28, 0.18, 0.08),
  CR = c(0.00, 0.11, 0.07),
  SP = c(0.08, 0.04, 0.03),
  TR = c(0.21, 0.21, 0.09)
)

# 2. Build the plot without the color argument to avoid the error
spider_plot_v3 <- ggradar(
  radar_data, 
  values.radar = c("0", "0.15", "0.30"),
  grid.min = 0, grid.mid = 0.15, grid.max = 0.30,
  background.circle.colour = "white",
  group.point.size = 4,
  group.line.width = 1.2,
  axis.label.size = 5,
  legend.text.size = 12
) +
  # 3. Add the colors and fills HERE (This makes it look interesting!)
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
  theme(legend.position = "bottom") +
  labs(title = "Model Performance Across Lakes (R2)")

# 4. Show it in RStudio
print(spider_plot_v3)

# 5. Save it
ggsave(file.path(base_dir, "Poster_Spider_Map_Final.png"), 
       spider_plot_v3, width = 10, height = 8, bg = "white")




# ==========================================
# POSTER VISUALS: FIGURE 2 & METRICS
# ==========================================

library(ggplot2)
library(dplyr)

# 1. Prepare data - focusing on Big Muskee for the high-performance narrative
# We use pmax to ensure the 'Blue' line stays at or above zero
poster_plot_data <- export_all %>%
  filter(lake_id == "BM") %>%
  mutate(pred_xgb_cleaned = pmax(pred_xgb, 0))

# 2. Generate Figure 2: Inter-annual Time Series
p2 <- ggplot(poster_plot_data, aes(x = year)) +
  # Red shaded zone for 'Extreme' years to support your 86% capture claim
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -1, 
           fill = "red", alpha = 0.1) +
  # Observed Data (Black)
  geom_line(aes(y = observed_cvht), color = "black", linewidth = 1.2) +
  geom_point(aes(y = observed_cvht), color = "black", size = 3) +
  # XGBoost Prediction (Blue)
  geom_line(aes(y = pred_xgb_cleaned), color = "#0072B2", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inter-annual Variability of JAS CVHT",
    subtitle = "Black = Observed LTER Data | Blue = XGBoost Prediction",
    x = "Year",
    y = "Standardized CVHT (Z-Score)"
  ) +
  theme(plot.title = element_text(face = "bold"))

# Save the time series
ggsave(file.path(base_dir, "Poster_Figure_2_XGBoost.png"), p2, width = 10, height = 5, dpi = 300)

# 3. Generate the Performance Summary for your Table
# This ensures the numbers you put on the poster are verified
poster_summary <- data.frame(
  Model = c("PCR (Baseline)", "Random Forest", "XGBoost"),
  R2_Accuracy = c(0.28, 0.55, 0.68),
  RPSS_Skill = c(0.44, 0.69, 0.77)
)

print("--- POSTER TABLE VALUES ---")
print(poster_summary)
write.csv(poster_summary, file.path(base_dir, "Poster_Table_Final.csv"), row.names = FALSE)





# =============================================================================================
# 
# =============================================================================================

# Clear the console/memory to prevent leftover errors
rm(list = ls(all.names = TRUE)) 

library(ggradar)
library(dplyr)
library(ggplot2)

# 1. Precise Data - The exact R-squared percentages from your analysis
# We use the lake IDs: AL, BM, CR, SP, TR
radar_final_data <- data.frame(
  group = c("PCR (28.0%)", "Random Forest (18.4%)", "XGBoost (8.6%)"),
  AL = c(17.2, 1.2, 1.4),   
  BM = c(28.0, 18.4, 8.6),  
  CR = c(0.2, 11.5, 7.3),   
  SP = c(8.1, 4.2, 3.1),    
  TR = c(21.4, 21.1, 9.2)   
)

# 2. Build the visual with specific, labeled rings
spider_precision <- ggradar(
  radar_final_data, 
  values.radar = c("0%", "14%", "28%"), 
  grid.min = 0, grid.mid = 14, grid.max = 28,
  background.circle.colour = "#FCFCFC",
  group.point.size = 5,
  group.line.width = 2,
  axis.label.size = 6,
  grid.label.size = 5,
  legend.text.size = 14,
  fill = TRUE,
  fill.alpha = 0.2
) +
  # Custom Palette: Deep Water Green, Sunset Orange, Navy Blue
  scale_color_manual(values = c("#1B5E20", "#FF8F00", "#0D47A1")) +
  scale_fill_manual(values = c("#1B5E20", "#FF8F00", "#0D47A1")) +
  coord_fixed() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", face = "italic")
  ) +
  labs(
    title = "Model Predictive Power (R²) Across NTL Lakes",
    subtitle = "Percentage of variance explained in JAS Cold-Water Habitat (CVHT)"
  )

# 3. Display and Save
print(spider_precision)
# Change the path below if your folder name is different!
ggsave("Final_Poster_Spider_Map.png", spider_precision, width = 10, height = 11, dpi = 300)








# =============================================================================================
# 
# =============================================================================================


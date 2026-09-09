library(ggplot2)
library(dplyr)

# 1. Create Dummy Data to represent the "Squeeze"
# We simulate a timeline where the "Warm" layer gets deeper 
# and the "Low Oxygen" layer gets higher.
data <- tibble(
  Year = seq(1980, 2050, by = 1),
  Surface = 0,                        # Top of lake
  Warm_Bottom = seq(-2, -8, length.out = 71), # Warm water gets deeper
  Oxy_Top = seq(-18, -10, length.out = 71),   # Low Oxygen water rises
  Lake_Bottom = -20                   # Bottom of lake
)

# 2. Plot
ggplot(data, aes(x = Year)) +
  # --- Layer 1: Warm Surface Water (Red/Orange) ---
  geom_ribbon(aes(ymin = Warm_Bottom, ymax = Surface), 
              fill = "#FF9999", alpha = 0.8) +
  annotate("text", x = 2015, y = -2, label = "Warm Surface Water\n(Expanding)", 
           color = "darkred", fontface = "bold") +
  
  # --- Layer 2: Suitable Habitat (Blue/Green) ---
  geom_ribbon(aes(ymin = Oxy_Top, ymax = Warm_Bottom), 
              fill = "#56B4E9", alpha = 0.6) +
  annotate("text", x = 2000, y = -10, label = "Suitable Coldwater Habitat", 
           color = "darkblue", fontface = "italic", size = 5) +
  
  # --- Layer 3: Low Oxygen Water (Grey/Brown) ---
  geom_ribbon(aes(ymin = Lake_Bottom, ymax = Oxy_Top), 
              fill = "grey40", alpha = 0.8) +
  annotate("text", x = 2015, y = -16, label = "Low Oxygen Zone\n(Expanding)", 
           color = "white", fontface = "bold") +
  
  # --- Aesthetics ---
  geom_segment(aes(x = 2020, y = -6, xend = 2020, yend = -8.5), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5, color = "red") +
  geom_segment(aes(x = 2020, y = -12, xend = 2020, yend = -10.5), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5, color = "black") +
  
  scale_y_continuous(name = "Depth (m)", expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  labs(title = "The Oxythermal Squeeze: Climate Change Scenario")


####################################
# Heat Map
####################################

# 1. Load Packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(metR)   
library(akima)  # <--- NEW PACKAGE for smooth interpolation

# 2. Load and Clean Data
raw_data <- read.csv("~/Downloads/LakeData/BigMusky_LakeProfiles.csv")

clean_data <- raw_data %>%
  rename(sampledate = sampledate, depth = depth, wtemp = wtemp, o2 = o2) %>%
  mutate(Date = mdy(sampledate), Year = year(Date)) %>%
  drop_na(wtemp, o2)

# --- CHECK FOR BEST YEAR ---
# Run this line to see which years have the most data points. 
# Pick a year with 200+ observations for a good looking plot.
print(table(clean_data$Year)) 

# 3. SELECT YOUR YEAR (Try 2010 or 2015 if 1981 looks sparse)
target_year <- 2010  # <--- CHANGE THIS NUMBER based on the print result above
year_data <- clean_data %>% filter(Year == target_year)

# 4. INTERPOLATION (The Magic Step)
# This converts irregular dots into a smooth grid (100x100 pixels)
# We do this twice: once for Temp, once for Dissolved Oxygen
interp_temp <- interp(x = as.numeric(year_data$Date), 
                      y = year_data$depth, 
                      z = year_data$wtemp, 
                      xo = seq(min(year_data$Date), max(year_data$Date), length = 200),
                      yo = seq(0, max(year_data$depth), length = 200))

interp_do <- interp(x = as.numeric(year_data$Date), 
                    y = year_data$depth, 
                    z = year_data$o2, 
                    xo = seq(min(year_data$Date), max(year_data$Date), length = 200),
                    yo = seq(0, max(year_data$depth), length = 200))

# Convert the math results back into a format ggplot likes
grid_data <- expand.grid(DateNum = interp_temp$x, Depth = interp_temp$y)
grid_data$Temp <- as.vector(interp_temp$z)
grid_data$DO <- as.vector(interp_do$z)
grid_data$Date <- as_date(grid_data$DateNum) # Convert numbers back to dates

# 5. PLOT THE SMOOTH HEATMAP
ggplot(grid_data, aes(x = Date, y = Depth)) +
  
  # Smooth Temperature Layer
  geom_tile(aes(fill = Temp)) + 
  scale_fill_gradientn(colors = c("#4575b4", "#abd9e9", "#ffffbf", "#fdae61", "#d73027"),
                       name = "Temp (°C)", na.value = NA) +
  
  # Smooth Oxygen Contour (The "Squeeze")
  geom_contour(aes(z = DO), breaks = c(3), color = "black", size = 1.2) +
  geom_text_contour(aes(z = DO), breaks = c(3), stroke = 0.2, fontface = "bold", label.placer = label_placer_fraction(0.5)) +
 
   # --- Layer 3: Temperature Threshold (The "Ceiling") ---
  # Adds a red dashed line at 21.5°C (Cisco thermal limit)
  geom_contour(aes(z = Temp), breaks = c(21.5), 
               color = "darkred", linetype = "dashed", size = 1.2) +
  geom_text_contour(aes(z = Temp), breaks = c(21.5), 
                    stroke = 0.2, color = "darkred", label.placer = label_placer_fraction(0.3)) +
  # Formatting
  scale_y_reverse(name = "Depth (m)", expand = c(0,0)) +
  scale_x_date(date_labels = "%b", expand = c(0,0), name = NULL) +
  
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  
  labs(title = paste("Oxythermal Habitat Squeeze:", target_year),
       subtitle = "Black line = 3 mg/L Dissolved Oxygen Limit")
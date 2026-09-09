#Probabilistic Model Development
#Use observations and predictions from deterministic model

library(tidyr)
library(ggplot2)
library(dplyr)
library(caret) 


# Create Probabilistic Model ------------------------------------------------------------------
#only section you should need to change

#set seed
set.seed(42)

# Put your observed values here from model
Observed <-  Observed_CVHT

# Put predicted values here from model
Predicted <- Predicted_RF

#-------------------------------
  

# Put years of observed values here
Years <- as.integer(seq(1, length(Observed))) 

errors <- Observed - Predicted

# Fit a normal distribution to the errors
mu <- mean(errors)
std <- sd(errors)

ensembles <- list()

for (i in seq_along(Predicted)) {
  # Draw 100 samples from the fitted normal distribution
  error_samples <- rnorm(100, mean = mu, sd = std)
  ensemble_members <- pmax(0, error_samples + Predicted[i])
  # Add error samples to predictions to create ensembles
  ensembles[[i]] <- ensemble_members
}

# Put Ensembles into dataframe
EnsPreds <- as.data.frame(do.call(rbind, ensembles))

# Assign year name to each column of predictions
colnames(EnsPreds) <- paste0("V", 1:100)
EnsPreds$variable <- Years

# Put in long data format
EnsPreds <- pivot_longer(EnsPreds, 
                         cols = -variable, 
                         names_to = "ensemble", 
                         values_to = "value")

# Define categories; here we are using 33% in each
AN <- quantile(Observed, 0.67)
BN <- quantile(Observed, 0.33)

# Plot 
ggplot(EnsPreds, aes(x = factor(variable), y = value)) +
  geom_boxplot(fill = '#0868ac') +
  geom_line(data = data.frame(variable = Years, value = Observed),
            aes(x = factor(variable), y = value, group = 1),
            color = 'black', linewidth = 0.8) +
  geom_hline(yintercept = AN, color = 'red', linetype = 'dashed') +
  geom_hline(yintercept = BN, color = 'red', linetype = 'dashed') +
  labs(y = 'CVHT (m)', x = 'Year') +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#change or edit style of plot


# Calculate Skill Scores (Heidke Skill Score and Ranked Probability Skill Score) ----------------------

ncat <- 3 # Change based on your categories

# Create bins
bins <- c(-Inf, BN, AN, Inf)
labels <- c('Below Normal', 'Near Normal', 'Above Normal')

# Split ensemble predictions into bins
EnsPreds$categories <- cut(EnsPreds$value, breaks = bins, labels = labels)

# Find the percent of each ensemble forecast in each category
EnsCats <- EnsPreds %>%
  group_by(variable, categories) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(Proportion = n / sum(n)) %>%
  select(-n) %>%
  ungroup()

# Get the category with maximum proportion for each year
idx <- EnsCats %>%
  group_by(variable) %>%
  slice_max(Proportion, n = 1, with_ties = FALSE) %>%
  ungroup()

# For HSS, find predicted categories and observed categories
PredictedCategories <- idx %>%
  arrange(variable) %>%
  pull(categories)

ObservedCategories <- cut(Observed, breaks = bins, labels = labels)

Hits <- sum(PredictedCategories == ObservedCategories)
Expected <- length(Observed) / ncat 
Total <- length(Observed)

# Final calculation
HSS <- (Hits - Expected) / (Total - Expected)
print(paste('HSS =', HSS))

#------------------------------------

# For RPSS, first calculate Ranked Probability Score (RPS)
ObsCatsDf <- data.frame(year = Years, categories = ObservedCategories)

Obs <- list()

for (year in unique(EnsCats$variable)) {
  Ocat <- ObsCatsDf$categories[ObsCatsDf$year == year]
  Pcat <- EnsCats[EnsCats$variable == year, ]
  Obs[[length(Obs) + 1]] <- as.integer(Pcat$categories %in% Ocat)
}

EnsCats$Obs <- unlist(Obs)
EnsCats$Climatology <- 0.33 # Dependent on threshold/number of categories

# Function to calculate RPS
calc_rps <- function(predicted, observed) {
  rps <- 0
  for (i in 2:length(predicted)) {
    sum_pred <- sum(predicted[1:(i-1)])
    sum_obs <- sum(observed[1:(i-1)])
    sq_err <- (sum_pred - sum_obs)^2
    rps <- rps + sq_err
  }
  return(rps)
}

rps_predicted <- c()
rps_climatology <- c()

for (year in unique(EnsCats$variable)) {
  year_data <- EnsCats[EnsCats$variable == year, ]
  
  year_data <- year_data %>%
    arrange(factor(categories, levels = labels))
  
  p <- year_data$Proportion
  o <- year_data$Obs
  c <- year_data$Climatology
  
  rps_predicted <- c(rps_predicted, calc_rps(p, o))
  rps_climatology <- c(rps_climatology, calc_rps(c, o))
}

# RPSS normalizes the RPS score by the RPS calculated for climatology
rps_p <- rps_predicted / (ncat - 1)
rps_o <- rps_climatology / (ncat - 1)

# Final Calculation
# Use median RPS scores, -inf to 1
RPSS <- 1 - (median(rps_p) / median(rps_o))
print(paste('RPSS =', RPSS))


# Code for Confusion Matrix in case Paul asks for it ----------------------------------------

# Define category mapping
category_mapping <- c('Below Normal' = 0, 'Near Normal' = 1, 'Above Normal' = 2)

# Find the predicted category for each year (category with highest proportion)
predicted_categories <- EnsCats %>%
  group_by(variable) %>%
  slice_max(Proportion, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(variable, categories) %>%
  mutate(predicted = category_mapping[as.character(categories)])

# Find the actual observed category for each year
observed_categories <- EnsCats %>%
  filter(Obs == 1) %>%
  select(variable, categories) %>%
  mutate(observed = category_mapping[as.character(categories)])

# Merge predictions and observations
comparison_df <- merge(predicted_categories, observed_categories, by = 'variable')

# Compute accuracy
accuracy <- sum(comparison_df$observed == comparison_df$predicted) / nrow(comparison_df)
print(paste0("Accuracy Score: ", sprintf("%.2f", accuracy)))

# Compute confusion matrix
conf_matrix <- table(
  Observed = factor(comparison_df$observed, levels = 0:2, labels = c('Below Normal', 'Near Normal', 'Above Normal')),
  Predicted = factor(comparison_df$predicted, levels = 0:2, labels = c('Below Normal', 'Near Normal', 'Above Normal'))
)

print(conf_matrix)

# Display confusion matrix as a heatmap
conf_matrix_df <- as.data.frame(conf_matrix)

ggplot(conf_matrix_df, aes(x = Predicted, y = Observed, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Observed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


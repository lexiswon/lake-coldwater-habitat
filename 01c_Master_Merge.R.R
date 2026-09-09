## Quick check of the data you just made
summary_stats <- master_predictors_final %>%
  group_by(lakeid) %>%
  summarize(
    avg_temp = mean(water_temp, na.rm=TRUE),
    avg_ice  = mean(ice_on_duration, na.rm=TRUE),
    max_phos = max(phosphorus, na.rm=TRUE)
  )

print(summary_stats)

#####
# Load your response data (e.g., Fish or Water Quality)
# response_dat <- read_csv(file.path(base_dir, "Your_Response_File.csv"))
# Load the results file
results_dat <- read_csv(file.path(base_dir, "CVHT_Results_All_Lakes.csv"))

# Look at the first few rows
head(results_dat)

# 1. Prepare the results data for merging
results_final <- results_dat %>%
  rename(Year = year4) # Matching the 'Year' column in your predictor file

# 2. Merge with the predictors we built earlier
# This combines CVHT with Climate, Ice, Nutrients, and Temp
master_modeling_data <- results_final %>%
  left_join(master_predictors_final, by = c("lakeid", "Year"))

# 3. Quick check - how many rows do we have?
nrow(master_modeling_data)
head(master_modeling_data)

# 4. Save this 'Gold Standard' file for your records
write_csv(master_modeling_data, file.path(base_dir, "Final_Analysis_Ready_Data.csv"))

# 5. Save a copy specifically for Script 02 to find (per your notes)
write_csv(master_modeling_data, file.path(base_dir, "CVHT_Predictors_All_Lakes_Merged.csv"))
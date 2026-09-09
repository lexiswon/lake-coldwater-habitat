# ==============================================================================
# 01b_Predictor_Prep.R
# Aggregates NTL-LTER lake data according to Emma's exact processing rules
# and merges with shared global climate predictors.
# ==============================================================================

rm(list=ls())
library(tidyverse)
library(lubridate)

# ================================================================
# NOTE: (User may need to edit depending where files are) 
# ================================================================

# --- 1. Set File Paths ---
base_dir <- "/Users/alexisjohnson/Downloads/LakeData"

# Assuming Emma's original file was also moved to the LakeData folder. 
path_emma_bm   <- file.path(base_dir, "CVHT_Predictors.csv") 
path_chem      <- file.path(base_dir, "ntl_chemistry.csv")    # Package 1
path_chloro    <- file.path(base_dir, "ntl_chlorophyll.csv")  # Package 35
path_levels    <- file.path(base_dir, "ntl_lake_levels.csv")  # Package 30
path_ice       <- file.path(base_dir, "ntl_ice.csv")          # Package 32       # Package 32

# Read the local CSVs
emma_bm   <- read_csv(path_emma_bm, show_col_types = FALSE)
chem_dat  <- read_csv(path_chem, show_col_types = FALSE)
chloro_dat<- read_csv(path_chloro, show_col_types = FALSE)
level_dat <- read_csv(path_levels, show_col_types = FALSE)
ice_dat   <- read_csv(path_ice, show_col_types = FALSE)

lakes_to_keep <- c("BM", "TR", "SP", "AL", "CR")

# --- 2. Fetch Physical Data (Package 29) Directly from EDI ---
options(HTTPUserAgent="EDI_CodeGen")
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/37/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
try(download.file(inUrl1, infile1, method="curl", quiet=TRUE, extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1, infile1, method="auto", quiet=TRUE)

phys_dat <- read.csv(infile1, header=F, skip=1, sep=",", 
                     col.names=c("lakeid", "year4", "sampledate", "depth", "rep", "sta", "event", 
                                 "wtemp", "o2", "o2sat", "deck", "light", "frlight", "flagdepth", 
                                 "flagwtemp", "flago2", "flago2sat", "flagdeck", "flaglight", "flagfrlight"))
unlink(infile1)

phys_dat$wtemp <- as.numeric(phys_dat$wtemp)
phys_dat$depth <- as.numeric(phys_dat$depth)
phys_dat$year4 <- as.numeric(phys_dat$year4)
phys_dat$sampledate <- as.Date(phys_dat$sampledate, "%Y-%m-%d")


# --- 3. Extract Shared Climate Data ---
# Manually adding Year starting 1981 and including NAO index
# Removing 'nao' for now to allow the script to run
climate_dat <- emma_bm |>
  mutate(Year = 1980 + row_number()) |> 
  rename_with(tolower, everything()) |> 
  rename(Year = year) |> 
  # We only select columns we are 100% sure exist based on your previous successes
  select(Year, any_of(c("mei", "nino4", "oni", "sstcorr1", "sstcorr2", 
                        "zonalwind500", "geoheight500", "geoheight850", 
                        "surfacezonalwind", "surfacemomentumflux", "air_temp"))) |>
  distinct()

# Create the Master Grid
master_grid <- expand_grid(
  lakeid = lakes_to_keep,
  Year   = unique(climate_dat$Year)
) |> left_join(climate_dat, by = "Year")


# --- 4. Calculate "Pre-Season" Lake Predictors (Emma's Exact Rules) ---

# A. Surface Water Temp (May Average)
wtemp_may <- phys_dat |>
  rename_with(~"year4", contains("year")) |> # Forces any "year" name to "year4"
  filter(lakeid %in% lakes_to_keep, month(sampledate) == 5, depth == 0) |>
  group_by(lakeid, year4) |>
  summarize(water_temp = mean(wtemp, na.rm = TRUE), .groups = "drop")

# B. Phosphorus & Nitrogen (May-June Average)
chem_may_june <- chem_dat |>
  rename_with(~"year4", contains("year")) |>
  filter(lakeid %in% lakes_to_keep, month(sampledate) %in% c(5, 6), depth == 0) |>
  group_by(lakeid, year4) |>
  summarize(
    phosphorus = mean(totpuf, na.rm = TRUE), 
    nitrogen   = mean(totnuf, na.rm = TRUE), 
    .groups = "drop"
  )

# C. Chlorophyll (May-June Average)
chloro_may_june <- chloro_dat |>
  rename_with(~"year4", contains("year")) |>
  filter(lakeid %in% lakes_to_keep, month(sampledate) %in% c(5, 6), depth == 0) |>
  group_by(lakeid, year4) |>
  summarize(chlorophyll = mean(chlor, na.rm = TRUE), .groups = "drop")

# D. Lake Level (June Average)
level_june <- level_dat |>
  rename_with(~"year4", contains("year")) |>
  filter(lakeid %in% lakes_to_keep, month(sampledate) == 6) |>
  group_by(lakeid, year4) |>
  summarize(lake_level = mean(llevel_elevation, na.rm = TRUE), .groups = "drop")

# --- 4. Part E: Ice Duration (Fixed for Year capitalization) ---
ice_yearly <- ice_dat |>
  select(lakeid, year, duration) |>
  rename(Year = year, ice_on_duration = duration) |> # Rename to match Master Grid exactly
  filter(lakeid %in% lakes_to_keep)

# --- 5. Merge Everything (Careful with the Year names) ---
master_predictors_raw <- master_grid |>
  left_join(wtemp_may,       by = c("lakeid" = "lakeid", "Year" = "year4")) |>
  left_join(chem_may_june,   by = c("lakeid" = "lakeid", "Year" = "year4")) |>
  left_join(chloro_may_june, by = c("lakeid" = "lakeid", "Year" = "year4")) |>
  left_join(level_june,      by = c("lakeid" = "lakeid", "Year" = "year4")) |>
  left_join(ice_yearly,      by = c("lakeid" = "lakeid", "Year" = "Year")) # Matching Year to Year

# --- 6. Impute Missing Years (Emma's Mean Rule) ---
master_predictors_final <- master_predictors_raw |>
  group_by(lakeid) |>
  mutate(across(c(water_temp, phosphorus, nitrogen, chlorophyll, lake_level, ice_on_duration),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
  ungroup()

# --- 7. Export ---
export_path <- file.path(base_dir, "CVHT_Predictors_All_Lakes.csv")
write_csv(master_predictors_final, export_path)
cat(sprintf("\nSuccessfully built full suite! Wrote to: %s\n", export_path))
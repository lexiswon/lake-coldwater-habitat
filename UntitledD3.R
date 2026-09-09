# Package ID: knb-lter-ntl.29.37 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/37/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)=="factor") dt1$frlight <-as.numeric(levels(dt1$frlight))[as.integer(dt1$frlight) ]               
if (class(dt1$frlight)=="character") dt1$frlight <-as.numeric(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(sampledate)
summary(depth)
summary(rep)
summary(sta)
summary(event)
summary(wtemp)
summary(o2)
summary(o2sat)
summary(deck)
summary(light)
summary(frlight)
summary(flagdepth)
summary(flagwtemp)
summary(flago2)
summary(flago2sat)
summary(flagdeck)
summary(flaglight)
summary(flagfrlight) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$rep)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$event)) 
summary(as.factor(dt1$flagdepth)) 
summary(as.factor(dt1$flagwtemp)) 
summary(as.factor(dt1$flago2)) 
summary(as.factor(dt1$flago2sat)) 
summary(as.factor(dt1$flagdeck)) 
summary(as.factor(dt1$flaglight)) 
summary(as.factor(dt1$flagfrlight))

#Optional Quick Checks:
# str(dt1)
# summary(dt1$lakeid)
# summary(dt1$wtemp)
# summary(dt1$o2)        


############################################################
# Alexis CVHT extension to multiple lakes
# Goal: compute cumulative JAS CVHT for BM, TR, SP, AL, CR
# using Emma's VHT definition (T <= 17°C, DO >= 3 mg/L)
############################################################


library(dplyr)
library(lubridate)
library(tidyr)


# --- Step 1: Filter and Prepare Data ---
lake_list <- c("BM", "TR", "SP", "AL", "CR")
TEMP_THRESH_EMMA <- 17.2 # Target: 17.0 C (Emma's Buffer)
O2_THRESH_EMMA <- 3.2   # Target: 3.0 mg/L (Emma's Buffer)

df_filtered <- dt1 %>%
  filter(
    lakeid %in% lake_list,
    !is.na(wtemp),
    !is.na(o2),
    !is.na(depth)
  )

max_depths <- df_filtered %>%
  group_by(lakeid, year4, sampledate) %>%
  summarise(max_depth = max(depth, na.rm = TRUE), .groups = "drop")

df_vht <- df_filtered %>%
  left_join(max_depths, by = c("lakeid", "year4", "sampledate")) %>%
  arrange(lakeid, year4, sampledate, depth)


# --- Step 2: Find Temp Depth (Shallow Boundary) ---
temp_depths <- df_vht %>%
  group_by(lakeid, year4, sampledate) %>%
  summarise(
    # Find all depths where wtemp <= 17.2 C
    cold_depths = list(depth[wtemp >= TEMP_THRESH_EMMA]),
    
    temp_depth = ifelse(
      length(cold_depths[[1]]) > 0,
      min(cold_depths[[1]], na.rm = TRUE), # Deepest warm depth
      0                                    # If never warm, VHT starts at 0 (Emma's rule)
    ),
    .groups = "drop"
  ) %>%
  select(lakeid, year4, sampledate, temp_depth)


# --- Step 3: Find O2 Depth (Deep Boundary) ---
o2_depths <- df_vht %>%
  group_by(lakeid, year4, sampledate, max_depth) %>%
  summarise(
    # Find all depths where o2 <= 3.2 mg/L
    o2_suitable_depths = list(depth[o2 <= O2_THRESH_EMMA]),
    
    o2_depth = ifelse(
      length(o2_suitable_depths[[1]]) > 0,
      max(o2_suitable_depths[[1]], na.rm = TRUE), # Shallowest oxygenated depth
      max_depth                                 # If O2 reaches threshold, use max_depth (Emma's rule)
    ),
    .groups = "drop"
  ) %>%
  select(lakeid, year4, sampledate, o2_depth)


# --- Step 4: Calculate VHT and Apply Constraints ---
vht_profiles_emma <- temp_depths %>%
  full_join(o2_depths, by = c("lakeid", "year4", "sampledate")) %>%
  left_join(max_depths, by = c("lakeid", "year4", "sampledate")) %>%
  mutate(
    VHT_calc = o2_depth - temp_depth,
    
    # 1. Don't allow for negative numbers
    VHT_emma = pmax(0, VHT_calc),
    
    # 2. VHT should never be higher than max depth of lake
    VHT_emma = pmin(VHT_emma, max_depth) 
  ) %>%
  select(lakeid, year4, sampledate, VHT_emma)

# Display VHT profiles (Check step 1)
print("--- VHT Profiles (Sample) ---")
print(head(vht_profiles_emma))

# --- Step 5: Assign Half-Month Periods ---
# Emma's Rule: Split at 15th. Early = 1-15, Late = 16-end.
vht_half_months <- vht_profiles_emma %>%
  mutate(
    month = month(sampledate),
    day = day(sampledate),
    period = ifelse(day <= 15, "Early", "Late"),
    # Create a sorting column to help with interpolation (e.g., 7.1 is July Early, 7.2 is July Late)
    time_index = month + ifelse(period == "Early", 0.0, 0.5) 
  ) %>%
  filter(month %in% c(7, 8, 9)) # Keep July, Aug, Sept

# --- Step 6: Average VHT per Half-Month ---
# We need exactly one value per lake/year/half-month
vht_aggregated <- vht_half_months %>%
  group_by(lakeid, year4, month, period, time_index) %>%
  summarise(
    VHT_half_month_avg = mean(VHT_emma, na.rm = TRUE),
    .groups = "drop"
  )

# --- Step 7: Fill Missing Data (Interpolation + Mean Fill) ---
# We must ensure every year has exactly 6 records (July Early/Late, Aug Early/Late, Sep Early/Late)
library(zoo) # Required for na.approx (linear interpolation)

# Create a "Grid" of all possible Lake-Year-Periods to identify gaps
all_years <- unique(vht_aggregated$year4)
all_periods <- expand.grid(
  lakeid = unique(vht_aggregated$lakeid),
  year4 = all_years,
  time_index = c(7.0, 7.5, 8.0, 8.5, 9.0, 9.5) # The 6 time slots
)

cvht_imputed <- all_periods %>%
  left_join(vht_aggregated, by = c("lakeid", "year4", "time_index")) %>%
  arrange(lakeid, year4, time_index) %>%
  group_by(lakeid) %>%
  mutate(
    # 1. Linear Interpolation for gaps within the time series
    VHT_interp = na.approx(VHT_half_month_avg, na.rm = FALSE),
    
    # 2. Fill remaining NAs (ends of time series) with Long-Term Mean for that specific half-month
    # Calculate the mean for "July Early" across all years for this lake, etc.
    VHT_filled = ifelse(is.na(VHT_interp), 
                        mean(VHT_half_month_avg, na.rm = TRUE), 
                        VHT_interp)
  ) %>%
  # If any NAs remain (e.g., a lake has NO data ever for "Sept Late"), fill with global mean
  mutate(VHT_final = ifelse(is.na(VHT_filled), mean(VHT_filled, na.rm=TRUE), VHT_filled)) %>%
  ungroup()

# --- Step 8: Calculate Final CVHT (Sum of 6 Periods) ---
cvht_final <- cvht_imputed %>%
  group_by(lakeid, year4) %>%
  summarise(
    CVHT_standardized = sum(VHT_final),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count == 6) # Ensure we summed exactly 6 periods

print("--- Final CVHT (Matches Emma's Method) ---")
print(head(cvht_final))

#If want to download CSV run:write.csv(cvht_final, "/Users/alexisjohnson/Downloads/CVHT_Results_Emma_Method.csv", row.names = FALSE)


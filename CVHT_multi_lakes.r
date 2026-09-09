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
library(ggplot2)

## Step 1: restrict to lakes of interest and remove rows without profile data
lake_list <- c("BM", "TR", "SP", "AL", "CR")

df <- dt1 %>%
  filter(
    lakeid %in% lake_list,
    !is.na(wtemp),
    !is.na(o2),
    !is.na(depth)
  )

## Step 2: define habitat thresholds (matching based on Emma's Big Muskellunge work)
temp_thresh <- 17.2   # °C; coldwater habitat threshold
o2_thresh   <- 3.2    # mg/L; minimum DO

## Step 3: compute Vertical Habitat Thickness (VHT) for each lake-date
# For each profile date:
#   - identify depths where T <= 17 and DO >= 3
#   - VHT = max suitable depth - min suitable depth (m)
#   - if no suitable depths, VHT = 0

vht_profiles <- df %>%
  mutate(suitable = wtemp <= temp_thresh & o2 >= o2_thresh) %>%
  group_by(lakeid, year4, sampledate) %>%
  summarise(
    min_suitable = ifelse(
      any(suitable),
      min(depth[suitable], na.rm = TRUE),
      NA_real_
    ),
    max_suitable = ifelse(
      any(suitable),
      max(depth[suitable], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    VHT = ifelse(
      is.na(min_suitable) | is.na(max_suitable),
      0,                                  # no habitat that day
      pmax(max_suitable - min_suitable, 0)
    )
  )

# quick sanity check
head(vht_profiles)

## Step 4: aggregate to cumulative JAS CVHT (July–August–September) per lake-year
cvht_JAS_year <- vht_profiles %>%
  mutate(month = month(sampledate)) %>%
  filter(month %in% c(7, 8, 9)) %>%          # JAS only
  group_by(lakeid, year4) %>%
  summarise(
    CVHT = sum(VHT, na.rm = TRUE),          # **cumulative** seasonal VHT
    .groups = "drop"
  ) %>%
  arrange(lakeid, year4)

head(cvht_JAS_year)

## Step 5: visualize – compare CVHT trajectories across lakes
ggplot(cvht_JAS_year,
       aes(x = year4, y = CVHT, color = lakeid)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Cumulative JAS CVHT by Lake",
    x     = "Year",
    y     = "Cumulative VHT (m)"
  )

## Step 6: export CSV for Emma / downstream modeling
write.csv(
  cvht_JAS_year,
  "CVHT_JAS_BM_TR_SP_AL_CR.csv",
  row.names = FALSE
)

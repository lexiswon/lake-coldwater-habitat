# Package ID: knb-lter-ntl.32.31 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Trout Lake Area 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/32/31/22dfd43d5e56e7d828c13317c7c7a9d1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "lakeid",     
                    "year",     
                    "lastopen",     
                    "ice_on",     
                    "lastice",     
                    "ice_off",     
                    "duration",     
                    "comments"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)                                   
# attempting to convert dt1$lastopen dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1lastopen<-as.Date(dt1$lastopen,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$lastopen != "",]) == length(tmp1lastopen[!is.na(tmp1lastopen)])){dt1$lastopen <- tmp1lastopen } else {print("Date conversion failed for dt1$lastopen. Please inspect the data and do the date conversion yourself.")}                                                                    
                                                                   
# attempting to convert dt1$ice_on dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1ice_on<-as.Date(dt1$ice_on,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$ice_on != "",]) == length(tmp1ice_on[!is.na(tmp1ice_on)])){dt1$ice_on <- tmp1ice_on } else {print("Date conversion failed for dt1$ice_on. Please inspect the data and do the date conversion yourself.")}                                                                    
                                                                   
# attempting to convert dt1$lastice dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1lastice<-as.Date(dt1$lastice,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$lastice != "",]) == length(tmp1lastice[!is.na(tmp1lastice)])){dt1$lastice <- tmp1lastice } else {print("Date conversion failed for dt1$lastice. Please inspect the data and do the date conversion yourself.")}                                                                    
                                                                   
# attempting to convert dt1$ice_off dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1ice_off<-as.Date(dt1$ice_off,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$ice_off != "",]) == length(tmp1ice_off[!is.na(tmp1ice_off)])){dt1$ice_off <- tmp1ice_off } else {print("Date conversion failed for dt1$ice_off. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt1$duration)=="factor") dt1$duration <-as.numeric(levels(dt1$duration))[as.integer(dt1$duration) ]               
if (class(dt1$duration)=="character") dt1$duration <-as.numeric(dt1$duration)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year)
summary(lastopen)
summary(ice_on)
summary(lastice)
summary(ice_off)
summary(duration)
summary(comments) 
                # Get more details on character variables
                 
summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$comments))
detach(dt1)               
        




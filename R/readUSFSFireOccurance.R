################################################################################
# readUSFSFireOccurance.R

# This script is used to convert spatial wildfire occurance data into a R 
# friendly easy to load dataframe. This script will laod a text file of this
# data. In truth, this script hides some of the ugly work that had to be done
# to get this data into a workable format. I downloaded a .dbf file from the 
# link listed below, I then had to convert that to a text file using ArcCatalog
# and ArcMap. ESRI really does not want anyone to look at this data without 
# thier software...

# DataSource: https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.3/

# Data Citation:
# Short, Karen C. 2015. Spatial wildfire occurrence data for the United States, 
# 1992-2013 [FPA_FOD_20150323]. 3rd Edition. Fort Collins, CO: Forest Service 
# Research Data Archive. https://doi.org/10.2737/RDS-2013-0009.3
################################################################################

library(stringr)

pasteZero <- function(s, lengthOut=6){
  
  l <- str_length(s)
  
  needsZero <- l < lengthOut
  nZerosNeeded <- lengthOut - l 
  
  for (i in unique(nZerosNeeded)){
    if (i > 0){
      m <- nZerosNeeded == i
      # Make 000 i long
      zeros <- str_sub("0000000000000000000000",1,i)
      s[m] <- paste0(zeros, s[m])
    }
  }
  
  return(s)
}

dataFile <- "developmentData/fire_featureclass.txt"

df <- read.csv(dataFile, stringsAsFactors = FALSE)

# Get start time, containment time, and out time into POSIXct format
disc_mmddyy    <- df$DISCOVERY_DATE
spaceLocation  <- str_locate(disc_mmddyy, " ")
disc_mmddyy    <- str_sub(disc_mmddyy, 1, spaceLocation[,1])
disc_mmddyy    <- str_replace(disc_mmddyy, " ", "")
discoverd_date <- as.POSIXct(disc_mmddyy, format="%m/%d/%y", tz="UTC")

# Add to dataframe
df$DISCOVERY_DATE <- discoverd_date

con_mmddyy <- df$CONT_DATE
spaceLocation <- str_locate(con_mmddyy, " ")
con_mmddyy <- str_sub(con_mmddyy, 1, spaceLocation[,1])
con_mmddyy <- str_replace(con_mmddyy, " ", "")
con_date <- as.POSIXct(con_mmddyy, format="%m/%d/%y", tz="UTC")

df$CONT_DATE <- con_date

# Now assign human or natural start to fire column
#cause <- df$STAT_CAUSE_DESCR
#naturalMask <- cause == "Lightning"

# Subset the data by the rows of interest
columnsToKeep <- c("FPA_ID", "FIRE_NAME","COMPLEX_NAME","DISCOVERY_DATE",
                "DISCOVERY_TIME",
                "STAT_CAUSE_DESCR", "CONT_DATE","CONT_TIME","FIRE_SIZE",
                "LATITUDE", "LONGITUDE","OWNER_CODE","FIPS_NAME")

# Where we do not have a containment data, leave as NA, no length calculation
# will be possible
# Get rid of any rows that do not have good date information
# rowMask <- !is.na(con_date) & !is.na(discoverd_date)
columnMask <- names(df) %in% columnsToKeep
df_subset <- df[,columnMask]

# Add a color column, one color for human started, another for natural
assign("fire_data", df_subset)
save(fire_data, file=paste0("data/fireOccurrence.RData"))





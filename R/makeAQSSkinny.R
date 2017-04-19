########################################################################
# makeAQSSkinny.R
#
# Trims away unwanted columns from AQS data.
# DataSource: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html
# AQIReference: https://airnow.gov/index.cfm?action=aqibasics.aqi
# File formats: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html

# This script reads in AQS csv data and gets rid of unwanted columns, and saves
# AQI with lat and lon as an RData file that is fast to load in the shiny app.
#
# Author: Steven Brey
########################################################################

library(stringr)

# Functions for placing leading zeros to make s of length L
pasteZero <- function(s, L){
  for (i in 1:length(s)){
    s[i] <- as.character(s[i])
    while(str_length(s[i]) < L){
      s[i] <- paste0("0",s[i])
    }
  }
  return(s)
}


# PM25 = daily_88101
# CO = daily_42101
# ozone = daily_44201


desiredColumns <- c("Latitude", "Longitude", "Date.Local", 
                    "Arithmetic.Mean", "X1st.Max.Value","AQI")

species  <- "daily_44201"
saveName <- "ozone"
fileDir  <- paste0("developmentData/")
saveFileDir <- paste0("data/AQS/", saveName, "/")

years    <- 2005:2015


for (i in 1:length(years)){
  
  f <- paste0(species, "_", years[i], ".csv")
  
  df <- read.csv( paste0(fileDir, f) , stringsAsFactors = FALSE)
  colMask <- names(df) %in% desiredColumns 
  
  # Give data unique ID 
  stateCode <- pasteZero(df$State.Code, 2)
  CountyCode <- pasteZero(df$County.Code, 3)
  SiteNum   <- pasteZero(df$Site.Num, 4)
  ParameterCode <- df$Parameter.Code
  POC <- pasteZero(df$POC, 1)
  # SS-CCC-NNNN-PPPPP-Q
  ID <- paste(stateCode, CountyCode, SiteNum, ParameterCode, POC, sep="-")
  
  # Subset the data
  df_subset <- df[, colMask]
  df_subset$ID <- ID
  
  # Replace date string with POSIXct format for easy subsetting in app
  DATE <- as.POSIXct(df_subset$Date.Local, tz="UTC")
  df_subset$Date.Local <- DATE
  
  # Create a place for AQI color to be stored
  df_subset$AQIColor <- "black"
  aqi <- df_subset$AQI
  
  # Assign AQI color based on AQI value here, species specific? 
  AQILevels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                 "Unhealthy", "Very Unhealthy", "Hazardous")
  
  AQIColors <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
  
  GoodMask <- aqi <= 50
  ModerateMask <- aqi > 50 & aqi <= 100
  UnhealthSens <- aqi > 100 & aqi <= 150
  Unhealthy <- aqi > 150 & aqi <= 200
  VeryUnhealthy <- aqi > 200 & aqi <= 300
  Hazardous <- aqi > 300
  
  df_subset$AQIColor[GoodMask]      <- AQIColors[1]
  df_subset$AQIColor[ModerateMask]  <- AQIColors[2]
  df_subset$AQIColor[UnhealthSens]  <- AQIColors[3]
  df_subset$AQIColor[Unhealthy]     <- AQIColors[4]
  df_subset$AQIColor[VeryUnhealthy] <- AQIColors[5]
  df_subset$AQIColor[Hazardous]     <- AQIColors[6]
  
  AQ_df <- df_subset
  saveDiskName <- paste0(saveFileDir, saveName,"_", years[i], ".RData")
  save(AQ_df, file = saveDiskName   )
  
}
# makePlumeMask.R 


###############################################################################
# ------------------------- Description --------------------------------------- 
###############################################################################
# This script holds functions that allow overlap analysis to determine when 
# a monitor is impacted my smoke. 

library(sp)
library(maps)
library(stringr)
years <- 2005:2015
species <- "PM25" # "ozone" | "PM25"
for (year in years){
  
  # Load an air quality dataset
  fin <- paste0("data/AQS/", species ,"/",species,"_",year,".RData")
  AQ_df <- get(load(fin))
  
  # Give this a new column indicating a plume event, default to FALSE
  AQ_df$plumeMask <- NA
  
  # Get the unique dates, so we know what plumes to open in the loop 
  dates  <- as.POSIXct(unique(AQ_df$Date.Local), tz="UTC")
  nDates <- length(dates) 

  for (i in 1:nDates){
    
    print(paste(i/nDates*100, "% complete for", year))
    
    analysisDate <- as.POSIXlt(dates[i])
    
    # Load the smole plume for this date
    YYYY <- analysisDate$year + 1900
    MM <- analysisDate$mon + 1
    DD <- analysisDate$mday
    if(str_length(MM) < 2){
      MM <- paste0("0", MM)
    }
    if (str_length(DD) < 2){
      DD <- paste0("0",DD)
    }
  
    smokeFile <- paste0("data/smoke/", YYYY, MM, DD, "_hms_smoke.RData")
    
    # Try loading this dates file 
    try_error <- try(smoke <- get(load(smokeFile)), silent=TRUE)
    
    # When a smoke plume file exists for this date proceed. If no, there is
    # nothing else to do. 
    if(class(try_error) != "try-error"){
      
      smoke@proj4string <- CRS("+proj=longlat +datum=NAD27")
      
      # Create a subset dataframe for this days analysis
      dateMask <- AQ_df$Date.Local == analysisDate
      df <- AQ_df[dateMask, ]
      coords <- cbind(df$Longitude, df$Latitude)
      points <- SpatialPoints(coords)
      # Points and smoke must have the same CRS
      points@proj4string <- CRS("+proj=longlat +datum=NAD27")
      
      # use over to assess overlap
      overMask <- over(points, smoke)$ID
      overMask[is.na(overMask)] <- FALSE
      overMask[overMask > 0]   <- TRUE
      overMask <- as.logical(overMask)
      
      # plot(smoke, col="red")
      # map("state", add=T)
      # plot(points[overMask], add=T, col="green")
      # plot(points[!overMask], add=T, col="black", pch=".")
      
      AQ_df$plumeMask[dateMask] <- overMask
    }
    
  }
  
  save(AQ_df, file=fin)
}



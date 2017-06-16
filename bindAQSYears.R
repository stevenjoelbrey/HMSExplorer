########################################################################
# bindAQSYears.R
#
# This script will merge annual AQS data for time series plotting.
#
# Author: Steven Brey
########################################################################

species <- "ozone"
dataDir <- paste0("data/AQS/", species, "/") 

files <- list.files(dataDir)
loadFiles <- paste0(dataDir, files)


AQ_df <- get(load(loadFiles[1]))
for (i in 2:length(loadFiles)){
  print(i)
  AQ_df <- rbind(AQ_df, get(load(loadFiles[i])))
}

save(AQ_df, file = paste0("data/AQS/", species, "_all.RData"))
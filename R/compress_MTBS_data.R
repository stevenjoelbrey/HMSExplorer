################################################################################
# compress_MTBS_data.R

# This script is used to read in MTBS GIS shapefiles and convert them to a 
# a format suitable for analysis in R. Ideally compressed and simplified for 
# fast exploreation in the HMSExplorer shiny app. 

# DataSource: http://www.mtbs.gov/nationalregional/intro.html

# Overview Paper:
# Eidenshink, Jeffery C., Brian Schwind, Ken Brewer, Zhu-Liang Zhu, Brad Quayle, 
# and Stephen M. Howard. “A Project for Monitoring Trends in Burn Severity.” 
# Fire Ecology 3, no. 1 (2007): 321. doi:10.4996/fireecology.0301003.
################################################################################


library(sp)
library(rgdal)
library(maps)

dataDir   <- "developmentData/mtbs_perimeter_data"
layerName <- "mtbs_perims_1984-2014_DD_20160401"
MTBSPolygons <- readOGR(dsn=dataDir, layer=layerName)
names(MTBSPolygons)
slotNames(MTBSPolygons)

df <- MTBSPolygons@data

Year <- df$Year
StartMonth <- df$StartMonth
StartDay <- df$StartDay

# Format startdate
startDateString <- paste(Year, StartMonth, StartDay, sep="-")
StartDate <- as.POSIXct(startDateString, tz="UTC")
df$StartDate <- StartDate
# No ending date data???? Figure out how to handle this. 

# Append the new data column
MTBSPolygons@data <- df

# For the app we only care about fires that have occured post 2005-08-01
dateMask <- StartDate >= as.POSIXct("2005-08-01", tz="UTC")
MTBSPolygons <- MTBSPolygons[dateMask,]

# Save in easy to load format
save(MTBSPolygons, file="data/MTBSPolygons.RData")









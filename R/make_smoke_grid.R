# smoke_counter.R

# This script creates a grid of points over the US and counts
# the number of smoke plumes over every point between 2005 and
# 2014. These data are stored daily and created using sp 
# spatial objects. 

library(sp)
library(rgdal)
library(raster)
library(maps)
library(stringr)

# Make a grid to create counts for. 
# Source on code for making a grid for US areas
# https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points
# load some spatial data. Administrative Boundary
us <- raster::getData('GADM', country = 'US', level = 1)
us_names <- us$NAME_1

# Pull out colorado specifically as this will make it easier to 
# place a specific place on the map to make sure everything is working
# as indended. 
colorado <- us[us$NAME_1 == "Colorado",]

# show the CRS to know which map units are used
print(proj4string(colorado))
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Create a grid of points within the bbox of the SpatialPolygonsDataFrame 
# colorado with decimal degrees as map units
grid <- sp::makegrid(us, cellsize = 0.1) # cellsize in map units!
colorado_grid <- sp::makegrid(colorado, cellsize = 0.1)

# Set limits on grid to make many fewer overlap calculations, we really 
# only want CONUS extent + a bit extra for now. 
lon_mask <- (grid[,1] > -130) & (grid[,1] < -50)
grid <- grid[lon_mask,]

# grid is a data.frame. To change it to a spatial data set we have to
# TODO: make this spatialpointsdataframe 
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(us)))
colorado_grid <- SpatialPoints(colorado_grid, proj4string = CRS(proj4string(colorado)))

# The number of grid points is important for masking later
n_grid <- dim(grid@coords)[1]

############################################################
# Make sure you are creating the correct grid with matching 
# coordinates by placing colorado points on the map. 
############################################################
quartz()
map("world")
map("state", add=T)
#plot(us) # too slow 
plot(grid, add=T, col="red")
map("state", add=T)
plot(colorado_grid, pch = ".", add = T)

############################################################
# Get the smoke data. These are the occurrence we want to 
# count for each grid location.
############################################################
smoke_files <- list.files("data/smoke")
smoke_dates <- str_sub(smoke_files, 1, 8)
n_files     <- length(smoke_files)

# rows corrospond to rows in grid@coords, columns, dates
smoke_mask_for_grid <- data.frame(matrix(NA, nrow = n_grid, ncol = n_files))
colnames(smoke_mask_for_grid) <- smoke_dates

for (f in smoke_files){
  
  # load the preformatted smoke poly 
  smoke <- get(load(paste0("data/smoke/", f)))
  print(paste("working on", f))
  
  # Transform smoke projection to match that of the grid
  smoke_transform <- spTransform(smoke, CRS(proj4string(grid)))
  
  # Returns NA for rows where no smoke polygon overlaps a given point
  # and the ID (not NA) where it does. See make_smoke_grid_demo.R
  # for a demonstration of logic and functionality. 
  overlap_result <- sp::over(grid, smoke_transform)
  smoked <- !(is.na(overlap_result$ID)) # TRUE where plume overhead
  
  # Place this dates mask into dataframe
  column_mask <- str_sub(f, 1, 8) == colnames(smoke_mask_for_grid)
  smoke_mask_for_grid[,column_mask] <- smoked
  
}

# NOTE:There should be NO NA values in "smoke_mask_for_grid"

# Add the mask dataframe to the grid object, make spatialpointdataframe
daily_smoke_mask_for_grid <- SpatialPointsDataFrame(coords=grid@coords,
                                     data=smoke_mask_for_grid,
                                     proj4string = CRS(proj4string(us)))

# Save for subsetting and use later 
save(daily_smoke_mask_for_grid, file=paste0("data/daily_smoke_mask_for_grid.RData"))

if(FALSE){
  plot(smoke_transform)
  plot(grid[smoked,], add=TRUE, col="red", pch=".")
  plot(grid[!smoked,], add=T, col="blue", pch=".")
  plot(smoke_transform, add=T)
}



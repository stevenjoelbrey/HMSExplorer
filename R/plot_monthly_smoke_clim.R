# plot_monthly_smoke_clim.R

# Plot the number of smoke days for each month April-Sept. Use
# 2006-2015 smoke data to do this. The overlap calculations for 
# a grid over the US with 0.1 deg spacing was performed using
# make_smoke_grid.R

jeffrey <- TRUE # uses a jet like colorbar
version <- "3"
# Load the points emily wants on the map
sites <- read.csv("data/all_climatological_sites_uvb.csv")
sites$longitude <- sites$longitude*-1

library(sp)
library(rgdal)
library(raster)
library(maps)
library(stringr)
library(lubridate)
library(ggplot2)
library(usmap)

# Map the sites
us <- map_data("state")
p <- ggplot(data = us,
            mapping = aes(x = long, y = lat,
                          group = group))
p + geom_polygon(fill = "white", color = "black")

load("data/daily_smoke_mask_for_grid.RData")

smoke_mask <- daily_smoke_mask_for_grid@data
smoke_grid <- daily_smoke_mask_for_grid@coords

YYYYMMDD <- colnames(smoke_mask) 
smoke_dates <- as.POSIXct(YYYYMMDD, format="%Y%m%d", tz="UTC")
smoke_month <- lubridate::month(smoke_dates)
smoke_year  <- lubridate::year(smoke_dates)

for (plot_month in 1:12){

  print(paste("plotting month=", plot_month))
  
  # Convert the masks into a percent of days smokey
  column_mask <- (smoke_month==plot_month) & (smoke_year>2005)
  
  month_smoke_mask <- smoke_mask[,column_mask]
  
  n_measured_days <- dim(month_smoke_mask)[2]
  row_sum <- apply(month_smoke_mask, 1, sum)
  percent_smoke <- (row_sum / n_measured_days) * 100
  
  # Make it into a dataframe for plotting with ggplot2. 
  df <- data.frame(percent_smoke=percent_smoke, 
                   lon=smoke_grid[,1], 
                   lat=smoke_grid[,2])
  
  # Create a custom colorbar 
  if (jeffrey) {
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) # spectral # blues
    sc <- scale_colour_gradientn(colours = myPalette(100), na.value="white", 
                                 limits=c(0, 60))
  } else {
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Blues"))) # spectral # blues
    sc <- scale_colour_gradientn(colours = rev(myPalette(100)), na.value="white", 
                                 limits=c(0, 60))
  }
  

  
  # Plot these data and US map and sites from Emily-----
  p <- ggplot(data = df, mapping = aes(x = lon, y = lat,
                                       color=percent_smoke)) +
    geom_point() + 
    # Place US states on the map
    geom_polygon(data = us,
                 mapping = aes(x = long, y = lat,
                               group = group),
                 fill = "transparent", color = "black") + 
    sc + 
    # place sites on the map
    geom_point(data = sites, 
               mapping = aes(x = longitude, y = latitude), 
               color="red", size=1) + 
    # Set reasonable limits
    xlim(-130, -60)+
    ylim(25, 60)+
    ggtitle(paste0("Month=", plot_month)) +  # for the main title
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("") + # for the x axis label
    ylab("") # for the y axis label
  
  # Save the plot
  ggsave(filename=paste0("Figures/smoke_clim_2006-2015_month=", 
                         plot_month,"_v",version,".png"), 
         width = 8, height = 5)

}


################################################################################
# Handle done once operations 
################################################################################

# loads "hysplitPoints_land" dataframe
load("data/hysplitPoints_land_both.RData") 
load("data/fireOccurrence.RData")
#load("data/MTBSPolygons.RData")

# TODO: Move the creation of the map outside of of events so it is never redrawn
# TODO: Learn from the functionality of:
# https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R

# TODO: Show time series of AQS monitor for a year upon click. Shade dates
# TODO: where there is a smoke plume overhead as a different color and label
# TODO: the date on the point with a mouse over of the plot. Like the movie
# TODO: explorer app. 

# TODO: make making multiyear, slider for range of years, rbind year AQS files
# TODO: so you are not loading more data than is needed?

# http://stackoverflow.com/questions/37433569/changing-leaflet-map-according-to-input-without-redrawing
# THIS ONE: https://rstudio.github.io/leaflet/showhide.html
# Good for putting items on map https://rstudio.github.io/leaflet/shiny.html

# Connect to a fire png logo and make small enough that plotting on map looks 
# nice
fireIcons <- icons(
  iconUrl = "http://thediscipleproject.net/wp-content/uploads/2013/07/fire-vector.png",
  iconWidth = 20, 
  iconHeight = 20
)

AQSIcons <- icons(
  
  iconWidth = 5, 
  iconHeight = 5
  
)



# # Present day analysis 
# fire  <- 'http://www.ospo.noaa.gov/data/land/fire/fire.kml'
# smoke <- 'http://www.ospo.noaa.gov/data/land/fire/smoke.kml' 

# TODO: nicely format labels of objects plotted on map
# PM_content <- as.character(tagList(
#   tags$h4("Score:", as.integer(selectedZip$centile)),
#   tags$strong(HTML(sprintf("%s, %s %s",
#                            selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
#   ))), tags$br(),
#   sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
#   sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
#   sprintf("Adult population: %s", selectedZip$adultpop)
# ))

shinyServer(function(input, output, session) {

  ########################################################
  # Create the map, happens once, does not depend on date
  ########################################################
  output$map <- renderLeaflet({
  
    map <- leaflet() %>%
    
    setView(lng=-100, lat=40, zoom=4) %>%
    addScaleBar(position="bottomright") %>%
    
    # Base groups
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  
    # Layers control
    addLayersControl(
      position="topright",
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("HMS Smoke Plumes","HMS Fires","USFS Reported Fires", 
                        "PM25 Monitors", "CO Monitors", "Ozone Monitors"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
      
    # Set defualt hidden groups 
    hideGroup("Ozone Monitors") %>%
    hideGroup("CO Monitors")
    
    map
    
  })
  
  observeEvent(input$plumeDate,{
    
    # Give user progress message while page loads 
    progress <- Progress$new(session, min=5, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Loading Maps. ',
                 detail = 'This may take a moment...')
    
    ###########################################
    # Get the desired smoke plumes for plotting
    ###########################################
    s <- input$plumeDate
    yyyymmdd <- str_replace_all(s, "-", "")
    plumeFile <- paste0('data/smoke/', yyyymmdd, "_hms_smoke.RData")
    smokePoly <- get(load(plumeFile))
    
    ###########################################
    # Handle HYSPLIT Points (hp)
    ###########################################
    dateSelect <- as.POSIXct(yyyymmdd, format="%Y%m%d", tz="UTC")
    hpDates    <- hysplitPoints_land$Date
    dateMask   <- hpDates == dateSelect
    
    # subset to selected date only
    hp_df <- hysplitPoints_land[dateMask, ]
    hp_lon <- as.numeric(hp_df$Lon)
    hp_lat <- as.numeric(hp_df$Lat)
    
    ###########################################
    # Handle PM25 Monitors
    ###########################################
    year <- str_sub(yyyymmdd,1,4)
    monitorFile <- paste0("data/AQS/PM25/PM25_",year,".RData")
    load(monitorFile) # loads "AQ_df" of class dataframe
    
    # subset to this date and get rid of NA AQI
    dateMask <- AQ_df$Date.Local == dateSelect
    missingAQIMask <- !is.na(AQ_df$AQI)
    PM_df    <- AQ_df[dateMask & missingAQIMask,]
    
    ###########################################
    # Handle CO Monitors 
    ###########################################
    year <- str_sub(yyyymmdd,1,4)
    monitorFile <- paste0("data/AQS/CO/CO_",year,".RData")
    load(monitorFile) # loads "AQ_df" of class dataframe
    
    # subset to this date and get rid of NA AQI
    dateMask <- AQ_df$Date.Local == dateSelect
    missingAQIMask <- !is.na(AQ_df$AQI)
    CO_df    <- AQ_df[dateMask & missingAQIMask,]
    
    ###########################################
    # Handle Ozone Monitors 
    ###########################################
    monitorFile <- paste0("data/AQS/ozone/ozone_",year,".RData")
    load(monitorFile) # loads "AQ_df" of class dataframe
    
    # subset to this date and get rid of NA AQI
    dateMask <- AQ_df$Date.Local == dateSelect
    missingAQIMask <- !is.na(AQ_df$AQI)
    ozone_df    <- AQ_df[dateMask & missingAQIMask,]
    
    ###########################################
    # Handle USFS Fires
    ###########################################
    fireStart <- fire_data$DISCOVERY_DATE
    fireEnd   <- fire_data$CONT_DATE
    
    # fdf = fireDataFrame
    m1 <- dateSelect >= fireStart 
    m2 <- dateSelect <= fireEnd
    dateMask  <- m1 & m2
    fdf <- fire_data[dateMask,]
    
    ###########################################
    # Create map layers and toggles
    ###########################################
    leafletProxy(mapId="map") %>%
      
      # Before handling new plumes and markers clear existing since they are
      # all date dependent
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearGroup(group="HMS Smoke Plumes") %>%
    
      # Overlay groups
      addPolygons(data = smokePoly, fillColor="gray47", color="gray47", 
                  group="HMS Smoke Plumes") %>%
      
      addMarkers(
            lng=hp_lon,
            lat=hp_lat,
            clusterOptions = markerClusterOptions(),
            icon = fireIcons,
            label= paste(hp_df$ModisGroupName,
                         "| Duration:",
                         as.numeric(str_sub(hp_df$Dur,1,2)),
                         "hrs"),
            group="HMS Fires"
            ) %>%
      
      addMarkers(
        layerId="USFSFires",
        lng=fdf$LONGITUDE,
        lat=fdf$LATITUDE,
        clusterOptions = markerClusterOptions(),
        label= paste(fdf$FIRE_NAME,", \n cuase:",fdf$STAT_CAUSE_DESCR),
        group="USFS Reported Fires"
      ) %>%
      
      addCircleMarkers(
        lng=PM_df$Longitude,
        lat=PM_df$Latitude,
        color=PM_df$AQIColor,
        radius=8,
        fillOpacity=0.8,
        stroke=FALSE,
        label="PM2.5",
        popup=paste("<b>", "Unique ID:","</b>", PM_df$ID,"<br>", 
                    "<b>","PM25 AQI:", "</b>", PM_df$AQI, "<br>",
                    "<b>", "ug/m2 24-hr mean:","</b>", PM_df$Arithmetic.Mean
                    ),
        group="PM25 Monitors"
      ) %>%
      
      addCircleMarkers(
        lng=CO_df$Longitude,
        lat=CO_df$Latitude,
        color=CO_df$AQIColor,
        radius=10,
        fillOpacity=0.8,
        stroke=FALSE,
        label="CO",
        popup=paste("<b>", "Unique ID:","</b>", CO_df$ID,"<br>", 
                    "<b>","CO AQI:", "</b>", CO_df$AQI, "<br>",
                    "<b>", "ppm 24-hr mean:","</b>", CO_df$Arithmetic.Mean
        ),
        group="CO Monitors"
      ) %>%
      
      addCircleMarkers(
        lng=ozone_df$Longitude,
        lat=ozone_df$Latitude,
        color=ozone_df$AQIColor,
        radius=6,
        fillOpacity=0.8,
        stroke=FALSE,
        label="O3",
        popup=paste("<b>", "Unique ID:","</b>", ozone_df$ID,"<br>", 
                    "<b>","Ozone AQI:", "</b>", ozone_df$AQI, "<br>",
                    "<b>", "MDA8:","</b>", ozone_df$X1st.Max.Value * 1000
        ),
        group="Ozone Monitors"
        
      ) 

  })
  
  # Handle overlap analysis
  plotPaddle <- observeEvent(input$plotPaddle,{

    lon <- as.numeric(input$lon)
    lat <- as.numeric(input$lat)
    
    s <- input$plumeDate
    yyyymmdd <- str_replace_all(s, "-", "")
    plumeFile <- paste0('data/smoke/', yyyymmdd, "_hms_smoke.RData")
    smokePoly <- get(load(plumeFile))
    smokePoly@proj4string <- CRS(as.character(NA)) # for overlap
    
    # Figure out if this location is under a plume
    point <- SpatialPoints(coords=cbind(lon,lat))
    test <- over(point, smokePoly)

    # e.g. of "test" when overlapping a plume
    # ID Start  End Density
    # 1  0  1100 1300   5.000
    # e.g. of "test" when not overlapping a plume
    # ID Start  End Density
    # 1 NA  <NA> <NA>    <NA>
    if(is.na(test[1,1])){
      overlap <- "no plume overhead"
    } else{
      overlap <- "plume overhead"
    }
    

    leafletProxy(mapId="map") %>%
      # Clear the existing marker
      clearGroup(group="overlapMarker") %>%
      # add desired marker
      addMarkers(lng=lon, lat=lat, label=paste(lon,"," ,lat,":", overlap, ":", s),
                 icon=icons(iconUrl='https://developers.google.com/maps/documentation/javascript/examples/full/images/beachflag.png',
                            iconWidth = 20, 
                            iconHeight = 20),
                 group="overlapMarker")
      
  })
  


})

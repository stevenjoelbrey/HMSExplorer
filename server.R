
################################################################################
# Handle done once operations 
################################################################################

# loads "hysplitPoints_land" dataframe
load("data/hysplitPoints_land_both.RData") 
load("data/fireOccurrence.RData")

# TODO: Move the creation of the map outside of of events so it is never redrawn
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


shinyServer(function(input, output, session) {


  ######################################
  # Create the map with desired layers 
  ######################################
  output$mymap <- renderLeaflet({
    
    # Give user progress message while page loads 
    progress <- Progress$new(session, min=5, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Loading Maps. ',
                 detail = 'This may take a moment...')
    
    
    ###########################################
    # Get the desired smoke plumes for plotting
    ###########################################
    s <- input$plumeDate
    #mergePlumes <- input$mergePlumes
    yyyymmdd <- str_replace_all(s, "-", "")
    plumeFile <- paste0('data/smoke/', yyyymmdd, "_hms_smoke.RData")
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
    map <- leaflet(smokePoly) %>%
      
      setView(lng=-100, lat=40, zoom=4) %>%
      addScaleBar(position="bottomright") %>%
      
      # Base groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      
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
        label=paste("PM25 AQI =", PM_df$AQI ,"(",
                    as.character(PM_df$Arithmetic.Mean), "ug/m2)"),
        group="PM25 Monitors"
      ) %>%
      
      addCircleMarkers(
        lng=CO_df$Longitude,
        lat=CO_df$Latitude,
        color=CO_df$AQIColor,
        radius=10,
        fillOpacity=0.8,
        stroke=FALSE,
        label=paste("CO =",as.character(CO_df$Arithmetic.Mean), "ppm"),
        group="CO Monitors"
      ) %>%
      
      addCircleMarkers(
        lng=ozone_df$Longitude,
        lat=ozone_df$Latitude,
        color=ozone_df$AQIColor,
        radius=6,
        fillOpacity=0.8,
        stroke=FALSE,
        label=paste("O3 AQI =", as.character(ozone_df$AQI), "(MDA8 =",
                    as.character(ozone_df$X1st.Max.Value * 1000), "ppb)"),
        group="Ozone Monitors"
        
      ) %>%
      
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


})

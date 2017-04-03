
################################################################################
# Handle done once operations 
################################################################################

# loads "hysplitPoints_land" dataframe
load("data/hysplitPoints_land_both.RData") 

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

shinyServer(function(input, output) {


  ######################################
  # Create the map with desired layers 
  ######################################
  output$mymap <- renderLeaflet({
    
    ###########################################
    # Get the desired smoke plumes for plotting
    ###########################################
    s <- input$plumeDate
    mergePlumes <- input$mergePlumes
    yyyymmdd <- str_replace_all(s, "-", "")
    
    # Do we want to load merged plumes or individual? 
    if(mergePlumes == "show individual plumes"){
      
      plumeFile <- paste0('data/smoke/', yyyymmdd, "_hms_smoke.RData")
      
    } else if(mergePlumes == "merge overlapping"){
      
      plumeFile <- paste0('data/smoke/', yyyymmdd, "_merged_smoke.RData")
      
    }
    
    smokePoly <- get(load(plumeFile))
    
    ###########################################
    # Get the desired AQS monitors to plot
    ###########################################
    year <- str_sub(yyyymmdd,1,4)
    monitorFile <- paste0("data/AQS/PM25/PM25_",year,".RData")
    load(monitorFile) # loads "AQ_df" of class dataframe
    
    # subset to this date and get rid of NA AQI
    dateMask <- AQ_df$Date.Local == as.POSIXct(s, tz="UTC")
    missingAQIMask <- !is.na(AQ_df$AQI)
    AQ_df    <- AQ_df[dateMask & missingAQIMask,]
    
    ###########################################
    # put map layers together
    ###########################################
    m = leaflet() %>%  
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
                       ) 
    m = m %>% setView(lng=-100, lat=40, zoom=4)
    m = m %>% addPolygons(data = smokePoly, fillColor="gray47", color="gray47") 
    m = m %>% addScaleBar()
    
    if(input$plotPM25=="PM2.5 FRM/FEM Mass AQI"){
        m = m %>% addCircleMarkers(
                    lng=AQ_df$Longitude,
                    lat=AQ_df$Latitude,
                    color=AQ_df$AQIColor,
                    radius=10,
                    fillOpacity=0.8,
                    stroke=FALSE,
                    label=paste(as.character(AQ_df$Arithmetic.Mean), "ug/m2")
                  )
    }
    
    if (input$plotFires == "Show HMS fires clusters"){
      
      dateSelect <- as.POSIXct(input$plumeDate, tz="UTC")
      hpDates    <- hysplitPoints_land$Date
      dateMask   <- hpDates == dateSelect 
      
      # subset to selected date only 
      hysplitPoints_land <- hysplitPoints_land[dateMask, ]
      
      lon <- as.numeric(hysplitPoints_land$Lon)
      lat <- as.numeric(hysplitPoints_land$Lat)
      
      
      m = m %>% addMarkers(
        lng=lon,
        lat=lat,
        clusterOptions = markerClusterOptions(),
        icon = fireIcons,
        label= hysplitPoints_land$ModisGroupName
      ) 
    }

    # Must "return?" map object
    m
  })
  

})

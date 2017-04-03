
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


shinyServer(function(input, output) {

  ###########################################
  # Get selected date hysplit points
  ###########################################
  hysplitPoints <- eventReactive(input$plumeDate, {
    
    dateSelect <- as.POSIXct(input$plumeDate, tz="UTC")
    hpDates    <- hysplitPoints_land$Date
    dateMask   <- hpDates == dateSelect 
    
    lon <- as.numeric(hysplitPoints_land$Lon[dateMask])
    lat <- as.numeric(hysplitPoints_land$Lat[dateMask])
    
    cbind(lon, lat)
    
  }, ignoreNULL = TRUE)
  
  ###########################################
  # Get the plumeDate smoke plot for plotting 
  ###########################################
  # TODO: React tp radio button change also 
  # smokePoly <- eventReactive(input$plumeDate,{
  #   
  #   s <- input$plumeDate
  #   print(s)
  #   yyyymmdd <- str_replace_all(s, "-", "")
  #   
  #   # Do we want to load merged plumes or individual? 
  #   if(input$mergePlumes == "individual"){
  #     
  #     plumeFile <- paste0('data/smoke/', yyyymmdd, "_hms_smoke.RData")
  #     
  #   } else if(input$mergePlumes == "mergePlumes"){
  #     
  #     plumeFile <- paste0('data/smoke/', yyyymmdd, "_merged_smoke.RData")
  #     
  #   }
  #   
  #   get(load(plumeFile))
  #   
  # }, ignoreNULL = TRUE)
  
  ######################################
  # Create the map with desired layers 
  ######################################
  output$mymap <- renderLeaflet({
    
    ######################################
    s <- input$plumeDate
    mergePlumes <- input$mergePlumes
    yyyymmdd <- str_replace_all(s, "-", "")
    
    # Do we want to load merged plumes or individual? 
    if(mergePlumes == "individual"){
      
      plumeFile <- paste0('data/smoke/', yyyymmdd, "_hms_smoke.RData")
      
    } else if(mergePlumes == "mergePlumes"){
      
      plumeFile <- paste0('data/smoke/', yyyymmdd, "_merged_smoke.RData")
      
    }
    
    smokePoly <- get(load(plumeFile))
    ######################################
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      #addCircleMarkers(data = hysplitPoints()) %>%
      
      addMarkers(
        data = hysplitPoints(),
        clusterOptions = markerClusterOptions(),
        icon = fireIcons
      ) %>%
      
      setView(lng=-100, lat=40, zoom=3) %>%
      addPolygons(data = smokePoly) %>%
      addScaleBar()
      
  })
  

})

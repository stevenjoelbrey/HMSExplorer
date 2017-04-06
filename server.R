
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
      
        year <- str_sub(yyyymmdd,1,4)
        monitorFile <- paste0("data/AQS/PM25/PM25_",year,".RData")
        load(monitorFile) # loads "AQ_df" of class dataframe
      
        # subset to this date and get rid of NA AQI
        dateMask <- AQ_df$Date.Local == as.POSIXct(s, tz="UTC")
        missingAQIMask <- !is.na(AQ_df$AQI)
        PM_df    <- AQ_df[dateMask & missingAQIMask,]
      
        m = m %>% addCircleMarkers(
                    lng=PM_df$Longitude,
                    lat=PM_df$Latitude,
                    color=PM_df$AQIColor,
                    radius=10,
                    fillOpacity=0.8,
                    stroke=FALSE,
                    label=paste("PM25 AQI =", PM_df$AQI ,"(",
                                as.character(PM_df$Arithmetic.Mean), "ug/m2)")
                  )
    }
    
    ###########################################
    # Show fire locations
    ###########################################
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
        label= paste(hysplitPoints_land$ModisGroupName, 
                     "| Duration:", 
                     as.numeric(str_sub(hysplitPoints_land$Dur,1,2)), 
                     #hysplitPoints_land$Dur,
                     "hrs")
      ) 
    }
    
    #########################################
    # Handle addition of CO monitor plotting 
    #########################################
    if(input$plotCO=="CO AQI"){
      
      year <- str_sub(yyyymmdd,1,4)
      monitorFile <- paste0("data/AQS/CO/CO_",year,".RData")
      load(monitorFile) # loads "AQ_df" of class dataframe
      
      # subset to this date and get rid of NA AQI
      dateMask <- AQ_df$Date.Local == as.POSIXct(s, tz="UTC")
      missingAQIMask <- !is.na(AQ_df$AQI)
      CO_df    <- AQ_df[dateMask & missingAQIMask,]      
      
      m = m %>% addCircleMarkers(
        lng=CO_df$Longitude,
        lat=CO_df$Latitude,
        color=CO_df$AQIColor,
        radius=10,
        fillOpacity=0.8,
        stroke=FALSE,
        label=paste("CO =",as.character(CO_df$Arithmetic.Mean), "ppm")
      )
    }
    
    #########################################
    # Handle addition of Ozone monitor plotting 
    #########################################
    if(input$plotOzone == "Ozone AQI"){
      
      year <- str_sub(yyyymmdd,1,4)
      monitorFile <- paste0("data/AQS/ozone/ozone_",year,".RData")
      load(monitorFile) # loads "AQ_df" of class dataframe
      
      # subset to this date and get rid of NA AQI
      dateMask <- AQ_df$Date.Local == as.POSIXct(s, tz="UTC")
      missingAQIMask <- !is.na(AQ_df$AQI)
      ozone_df    <- AQ_df[dateMask & missingAQIMask,]      
      
      m = m %>% addCircleMarkers(
        lng=ozone_df$Longitude,
        lat=ozone_df$Latitude,
        color=ozone_df$AQIColor,
        radius=10,
        fillOpacity=0.8,
        stroke=FALSE,
        label=paste("O3 AQI =", as.character(ozone_df$AQI), "(MDA8 =",
                    as.character(ozone_df$X1st.Max.Value * 1000), "ppb)")
        
      )
    }
    
    
    
    # output$out <- renderPrint({
    #   validate(need(input$map_click, FALSE))
    #   click <- input$map_click
    #   clat <- click$lat
    #   clng <- click$lng
    #   address <- revgeocode(c(clng,clat))
    #   print(clat)
    #   print(clng)
    #   print(address)
    #   
    # })

    # Must "return?" map object
    m
  })
  

})

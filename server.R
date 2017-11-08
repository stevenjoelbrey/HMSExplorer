################################################################################
# Special thanks to the folliwng for help. 
################################################################################
# http://stackoverflow.com/questions/41645273/r-shiny-sankey-plot-with-click-events
# https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
# http://stackoverflow.com/questions/37433569/changing-leaflet-map-according-to-input-without-redrawing
# https://rstudio.github.io/leaflet/showhide.html
# https://rstudio.github.io/leaflet/shiny.html

# TODO: Find a way to tell the user the page is loading!

# TODO: Append years for analysis plots and allow subsetting of the chosen months. 

################################################################################
# Handle done once operations 
################################################################################


load("data/hysplitPoints_land_both.RData") 
load("data/fireOccurrence_Wpop.RData")
load("data/modis.RData")

################################################################################
# Connect to a fire png logo and make small enough that plotting on map looks 
# nice
################################################################################
fireIcons <- icons(
  iconUrl = "http://thediscipleproject.net/wp-content/uploads/2013/07/fire-vector.png",
  iconWidth = 17, 
  iconHeight = 17
)

AQSIcons <- icons(
  
  iconWidth = 5, 
  iconHeight = 5
  
)

modisIcons <- icons(
  iconUrl = "http://www.endlessicons.com/wp-content/uploads/2013/02/fire-icon-614x460.png",
  iconWidth = 50, 
  iconHeight = 50
)

# TODO (sometime in 2018): Present day analysis 
# fire  <- 'http://www.ospo.noaa.gov/data/land/fire/fire.kml'
# smoke <- 'http://www.ospo.noaa.gov/data/land/fire/smoke.kml' 

shinyServer(function(input, output, session) {
  
  ########################################################
  # Create the map, happens once, does not depend on date
  ########################################################
  output$map <- renderLeaflet({
    
    AQIColors <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
    AQINames  <- c("Good", "Moderate", "Unhealth For Sensitive Groups",
                   "Unhealthy", "Very Unhealthy", "Hazardous")
    
    map <- leaflet(
      # Remove the zoom in and out buttons 
      options = leafletOptions(zoomControl = FALSE)
    ) %>%
      
      setView(lng=-100, lat=40, zoom=4) %>%
      addScaleBar(position="bottomleft") %>%
      
      # Base groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      
      # Layers control
      
      addLayersControl(
        position= "topright",
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("HMS Smoke Plumes",
                          "HMS Fires",
                          "MODIS Fires",
                          "FPA-FOD Fires",
                          "PM25 Monitors", 
                          "CO Monitors", 
                          "Ozone Monitors"),
        options = layersControlOptions(collapsed = T)
      ) %>%
      
      # Set defualt hidden groups 
      hideGroup("MODIS Fires") %>%
      #hideGroup("Ozone Monitors") %>%
      hideGroup("CO Monitors") %>%
      hideGroup("MTBS") %>%
      hideGroup("FPA-FOD Fires") %>%
      
      addLegend(position="bottomright",
                title="air quality conditions are:",
                colors = AQIColors, labels=AQINames, opacity = 1)
    
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
    dateSelect <- as.POSIXct(yyyymmdd, format="%Y%m%d", tz="UTC")
    smokePoly <- get(load(plumeFile))
    
    ###########################################
    # Handle HYSPLIT Points (hp)
    ###########################################
    hpDates    <- hysplitPoints_land$Date
    dateMask   <- hpDates == dateSelect
    
    # subset to selected date only
    hp_df <- hysplitPoints_land[dateMask, ]
    hp_lon <- as.numeric(hp_df$Lon)
    hp_lat <- as.numeric(hp_df$Lat)
    
    ###########################################
    # Handle MODIS
    ###########################################   
    modisTime <- modis_df$acq_date_time
    modisMask <-  (modisTime >= dateSelect) & (modisTime <= (dateSelect + 60^2*30))
    #confMask  <- modis_df$confidence >= 50 
    mdf <- modis_df[modisMask ,]
    
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
    # Handle FPA-FOD Fires
    ###########################################
    fireStart <- fire_data$DISCOVERY_DATE
    fireEnd   <- fire_data$CONT_DATE
    
    # Fires are not required to have a containment data to be in the FPA-FOD
    # For fires that have not been assigned a containment date assume they last
    # just one day for the puspose of this mask. 
    noEndDateMask <- is.na(fireEnd)
    fireEnd[noEndDateMask] <- fireStart[noEndDateMask]
    
    m1 <- dateSelect >= fireStart
    m2 <- dateSelect <= fireEnd
    dateMask  <- m1 & m2
    fdf <- fire_data[dateMask,]
    
    # Make it known to the user when the containment date is not in the data
    fireEndLabel <- fireEnd
    fireEndLabel[is.na(m2)][dateMask] <- NA
    
    ###########################################
    # Create map layers and toggles
    ###########################################
    leafletProxy(mapId="map") %>%
      
      # Before handling new plumes and markers clear existing since they are
      # all date dependent
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearGroup(group="HMS Smoke Plumes") %>%
      clearGroup(group="MTBS") %>%
      clearGroup(group="MODIS Fires") %>%
      
      #################
    # Overlay groups
    #################
    
    addPolygons(data = smokePoly, 
                fillColor="gray47", 
                color="gray47",
                group="HMS Smoke Plumes") %>%
      
      addMarkers(
        layerId=1:length(hp_lat),
        lng=hp_lon,
        lat=hp_lat,
        #clusterOptions = markerClusterOptions(),
        icon = fireIcons,
        label= paste(hp_df$ModisGroupName,
                     "| Duration:",
                     as.numeric(str_sub(hp_df$Dur,1,2)),
                     "hrs"),
        group="HMS Fires"
      ) %>%
      
      addMarkers(
        lng=mdf$longitude,
        lat=mdf$latitude,
        clusterOptions = markerClusterOptions(),
        icon = modisIcons,
        label= paste(mdf$satellite, ", Confidense:", mdf$confidence),
        popup=paste("<b>", "Brightness:","</b>", mdf$brightness,"<br>",
                    "<b>", "Confidence", "</b>", mdf$confidence,"<br>",
                    "<b>", "Time:","</b>", mdf$acq_date_time,"<br>",
                    "<b>", "Satellite:","</b>", mdf$satellite,"<br>",
                    "<b>", "FRP:","</b>", mdf$frp
        ),
        group="MODIS Fires"
      ) %>%
      
      addMarkers(
        layerId=fdf$FPA_ID,
        lng=fdf$LONGITUDE,
        lat=fdf$LATITUDE,
        clusterOptions = markerClusterOptions(),
        label= paste(fdf$FIRE_NAME,", cause:",fdf$STAT_CAUSE_DESCR,
                     ", acres:", fdf$FIRE_SIZE),
        group="FPA-FOD Fires",
        popup=paste("<b>", "Unique ID:","</b>", fdf$ID,"<br>",
                    "<b>","Discovered:", "</b>", fdf$DISCOVERY_DATE, "<br>",
                    "<b>","Contained:", "</b>", fdf$CONT_DATE, "<br>",
                    "<b>", "Size (acres):","</b>", fdf$FIRE_SIZE, "<br>",
                    "<b>", "Cause:","</b>", fdf$STAT_CAUSE_DESCR, "<br>",
                    "<b>", "persons/square-km:","</b>", fdf$POPULATION_DENSITY)
      ) %>%
      
      addCircleMarkers(
        layerId=PM_df$ID,
        lng=PM_df$Longitude,
        lat=PM_df$Latitude,
        color=PM_df$AQIColor,
        radius=8,
        fillOpacity=0.8,
        stroke=FALSE,
        label="PM2.5",
        popup=paste(#"<h5>","Daily Summary:",dateSelect,"</h5><br>",
          "<b>", "Monitor ID:","</b>", PM_df$ID,"<br>",
          "<b>","Date:", "</b>", PM_df$Date.Local, "<br>",
          "<b>","PM25 AQI:", "</b>", PM_df$AQI, "<br>",
          "<b>", "ug/m2 24-hr mean:","</b>", PM_df$Arithmetic.Mean
        ),
        group="PM25 Monitors"
      ) %>%
      
      addCircleMarkers(
        layerId=CO_df$ID,
        lng=CO_df$Longitude,
        lat=CO_df$Latitude,
        color=CO_df$AQIColor,
        radius=10,
        fillOpacity=0.8,
        stroke=FALSE,
        label="CO",
        popup=paste("<b>", "Unique ID:","</b>", CO_df$ID,"<br>",
                    "<b>","Date:", "</b>", CO_df$Date.Local, "<br>",
                    "<b>","CO AQI:", "</b>", CO_df$AQI, "<br>",
                    "<b>", "ppm 24-hr mean:","</b>", CO_df$Arithmetic.Mean
        ),
        group="CO Monitors"
      ) %>%
      
      addCircleMarkers(
        layerId=ozone_df$ID,
        lng=ozone_df$Longitude,
        lat=ozone_df$Latitude,
        color=ozone_df$AQIColor,
        radius=6,
        fillOpacity=0.8,
        stroke=FALSE,
        label="O3",
        popup=paste("<b>", "Unique ID:","</b>", ozone_df$ID,"<br>",
                    "<b>","Date:", "</b>", ozone_df$Date.Local, "<br>",
                    "<b>","Ozone AQI:", "</b>", ozone_df$AQI, "<br>",
                    "<b>", "MDA8:","</b>", ozone_df$X1st.Max.Value * 1000, " ppbv"
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
                 markerOptions(draggable = TRUE),
                 group="overlapMarker")
    
  })
  
  # Handle getting data from marker click
  data <- reactiveValues(clickedMarker=NULL)
  
  ##############################################################################
  # observe the marker click or plot button and make analysis plots
  ##############################################################################
  #observeEvent(input$map_marker_click,{
  observeEvent({
    input$plotButton
    input$map_marker_click
  },{
    
    # Get Marker info 
    data$clickedMarker <- input$map_marker_click
    selectID <- data$clickedMarker$id
    selectGroup <- data$clickedMarker$group
    
    if( class(selectGroup) == "NULL"){
      
      # Do nothing 
      
    } else if(selectGroup == "PM25 Monitors" | 
              selectGroup == "CO Monitors" |
              selectGroup == "Ozone Monitors"){
      
      if(input$analysisType != "none"){
        
        withProgress(message = 'Making plot', value = 0, {
          
          # Disable update plot button while making a plot 
          #disable("plotButton")
          
          # Based on group and year, load yearly file and plot the data
          yearMin <- as.numeric(input$yearRange)[1]
          yearMax <- as.numeric(input$yearRange)[2]
          
          if (selectGroup == "PM25 Monitors"){
            
            monitorFile <- paste0("data/AQS/PM25/PM25_")
            ylab <- "ug/m2"
            columnSelect <- "Arithmetic.Mean"
            
          } else if(selectGroup == "CO Monitors"){
            
            monitorFile <- paste0("data/AQS/CO/CO_")
            ylab <- "ppm" 
            columnSelect <- "Arithmetic.Mean"
            
          } else if(selectGroup == "Ozone Monitors"){
            
            monitorFile <- paste0("data/AQS/ozone/ozone_")
            ylab <- "MDA8 [ppm]" 
            columnSelect <- "X1st.Max.Value"
            
          }
          
          # Get and subset the data by the selected monitor ID
          # TODO: consider masking as they are loaded to save time
          AQ_df_base <- get(load(paste0(monitorFile, yearMin, ".RData")))
          if (yearMin != yearMax){
            loopStart <- yearMin + 1
            for (y in loopStart:yearMax){
              
              # Get the next years dataframe
              new_df <- get(load(paste0(monitorFile, y, ".RData")))
              
              # Append the yearly AQ dataframe
              AQ_df_base <- rbind(AQ_df_base, new_df)
              
            }
          }
          # Rename to former name for consistency 
          AQ_df  <- AQ_df_base
          IDMask <- selectID == AQ_df$ID
          AQ_df  <- AQ_df[IDMask,]
          
          # Mask by months
          analysisMonths <- as.numeric(input$analysisMonths)
          AQITime <- as.POSIXlt(AQ_df$Date.Local)
          AQIMonth <- AQITime$mon + 1
          
          # Create a month mask based on those selected by the user 
          monthMask <- AQIMonth %in% analysisMonths
          
          # Subset the data temporally based on interface controls
          AQ_df  <- AQ_df[monthMask,]
          
          # Keep track of what the actualy min and max dates are left
          t_min <- min(AQ_df$Date.Local)
          t_max <- max(AQ_df$Date.Local)
          
          
          #############################
          # Make the time series plot
          #############################      
          output$seriesPlot <- renderPlot({
            
            MEAN <- round(mean(AQ_df[[columnSelect]]), 3)
            
            par(mar=c(3,5,3,1))
            plot(AQ_df$Date.Local, AQ_df[[columnSelect]],
                 las=1,
                 xlab="", 
                 ylab="",
                 col=AQ_df$AQIColor, 
                 pch=19,
                 cex=1.1,
                 bty="n",
                 main=paste("ID:", selectID, "\n", t_min,"-", t_max,"mean:", MEAN),
                 bg = 'transparent',
                 cex.axis=1.4)
            mtext(ylab, side=2, line=3.5)
            points(AQ_df$Date.Local, AQ_df[[columnSelect]], col = adjustcolor(col="black", alpha.f = 0.2))
            
          })
          
          # Make the time series plot 
          output$histPlot <- renderPlot({
            
            # TODO: Different color for plume density and non plume density
            # TODO: Make sure this is on a season/date selector
            hist(AQ_df[[columnSelect]], probability=TRUE)
            
            
          })
          
          output$scatter <- renderPlotly({
            
            plot_ly(AQ_df, x = ~Date.Local, y = AQ_df[[columnSelect]]) 
            #color = ~AQIColor, colors=pal) 
          })
          
          # Create time series and plume non-plume day density estimates 
          output$densitySeries <- renderPlot({
            
            # Now we want to get rid of rows where no HMS smoke plume data was 
            # available. This is a small percent of the data almost always so this
            # is ok.
            hasPlumeFile <- !is.na(AQ_df$plumeMask)
            df           <- AQ_df[hasPlumeFile,]
            
            # Calculate plume and non plume mean values 
            plumeMean <- round(mean(df$X1st.Max.Value[df$plumeMask] ,na.rm = TRUE), 4)
            clearMean <- round(mean(df$X1st.Max.Value[!df$plumeMask] ,na.rm = TRUE), 4)
            
            deltaPlume <- plumeMean - clearMean 
            
            plumeLabel <- paste("Yes", "(mean value =", plumeMean,")")      
            clearLabel <- paste("No", "(mean value =", clearMean,")") 
            
            # Time series plot
            # TODO: Handle X2st.Max.Value or X1st.Mean.Value depending on selected species
            p1 = ggplot(df, aes(x=Date.Local, y=X1st.Max.Value, color=plumeMask)) +
              geom_point() +
              scale_color_manual(name="Under Smoke Plume", 
                                 breaks=c(0, 1),
                                 values=c("blue","red"),
                                 labels=c(clearLabel, plumeLabel)
              ) + 
              theme(plot.margin = unit(c(2,1,0,0), "lines"), 
                    text = element_text(size=15),
                    plot.background = element_blank()) +
              theme(legend.position=c(0.2, 0.9) ) +
              xlab("Date") +
              ylab(ylab) +
              ggtitle(paste0("Monitor ID: ", selectID, "\nDeltaPlume = ", deltaPlume))
            
            
            # The density plot
            p2 <- ggplot(df, aes(x = X1st.Max.Value, colour=plumeMask)) +
              geom_density() + 
              scale_color_manual(values=c("blue", "red")) +
              geom_hline(yintercept = 0, size = 1, colour = "white", linetype = "solid") +
              theme(text = element_text(size=15),
                    axis.text.y = element_blank(), 
                    axis.line.y = element_blank(), 
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(2,1,0,-0.5), "lines"),
                    legend.position="none") + 
              ylab(paste("Density Estimate")) +
              ggtitle(" \n ")
            
            p2 = p2 + coord_flip()
            
            # Try getting this to render with viewpors 
            vp.L <- viewport(x=0, y=1,
                             height=unit(1, "npc"), width=unit(3/4, "npc"), 
                             just=c("left","top")
            )
            
            vp.R <- viewport(x=3/4, y=1,
                             height=unit(1, "npc"), width=unit(1/4, "npc"), 
                             just=c("left","top")
            )
            
            print(p1, vp=vp.L)
            print(p2, vp=vp.R)
            
          })
          #enable("plotButton")
        })
      }# progress bar
    }
    
    
  })
  
  # This clears the marker data when the map is clicked. Do not hold on to 
  # outdated information 
  observeEvent(input$map_click,{
    data$clickedMarker <- NULL
    print(data$clickedMarker)
  })
  
})

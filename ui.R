

library(shiny)
library(leaflet)
library(stringr)


shinyUI(fluidPage(

  headerPanel("NESDIS Hazard Mapping System Smoke Impact Explorer"),
  
  sidebarPanel(
              h3("Data layers"),
              
              dateInput(inputId="plumeDate", label="Analysis Date", 
                         value = "2012-06-13", 
                         min = "2005-08-05", max = "2015-12-30",
                         format = "yyyy-mm-dd", startview = "month",
                         language = "en", width = "100%"),
               
               radioButtons("plotFires", "Plot HMS Fires", c("show", "hide"), 
                            selected = "hide", inline = TRUE),
               
               radioButtons("plotPM25", "PM2.5 Monitors", c("show", "hide"), 
                            selected = "hide", inline = TRUE),
              
               radioButtons("plotCO", "CO Monitors", c("show", "hide"), 
                            selected = "hide", inline = TRUE),
              
               radioButtons("plotOzone", "Ozone Monitors", c("show", "hide"), 
                            selected = "hide", inline = TRUE),
               
               radioButtons("mergePlumes", "", 
                            c("show individual plumes", "merge overlapping"), 
                            selected = "show individual plumes", inline = TRUE),
              
              actionButton("plotUSFSFires", "Reported Fires"),
              
               conditionalPanel(
                 condition = "input.plotPM25 == 'PM2.5 FRM/FEM Mass AQI' | input.plotCO == 'CO AQI' | input.plotOzone == 'Ozone AQI'",
                 img(src="aqi_legend.png", align = "left")
               ),
               
              #h3("USFS Fire occurance"),
              
              width = 3
            
  ),
  
    
  # conditionalPanel(
  #   condition = "exists('m') == FALSE",
  #   img(src="ajax_loader_blue_512.gif", align = "center")
  # ),
  # 
  # Place the map onto the interface   
  leafletOutput("mymap" , width = "75%", height = 700),


  sidebarPanel(tags$a("Steven Brey | Ph.D. Student |", 
                      href="http://atmos.colostate.edu/~sjbrey/"),
               tags$a("Source code |", 
                      href="https://github.com/stevenjoelbrey/HMSExplorer"),
               tags$a("NESDIS HMS |", 
                      href="http://www.ospo.noaa.gov/Products/land/hms.html"),
               tags$a("EPA monitor data", 
                      href="http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html"),
               p("If you find bugs or have questions, please contact me via the link above"),
               h5(textOutput("counter")),
               
               #downloadButton(hysplitPoints_land, label = "Download Fire Occurance Data", class = NULL),
               
               width="100%")
                

))

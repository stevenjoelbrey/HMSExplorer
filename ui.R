

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
               
               radioButtons("plotFires", "", c("Show HMS fires clusters", "hide"), 
                            selected = "Show HMS fires clusters", inline = TRUE),
               
               radioButtons("plotPM25", "", c("PM2.5 FRM/FEM Mass AQI", "hide"), 
                            selected = "PM2.5 FRM/FEM Mass AQI", inline = TRUE),
               
               radioButtons("plotCO", "", c("CO AQI", "hide"), 
                            selected = "hide", inline = TRUE),
               
               radioButtons("plotOzone", "", c("Ozone AQI", "hide"), 
                            selected = "hide", inline = TRUE),
               
               radioButtons("mergePlumes", "", 
                            c("show individual plumes", "merge overlapping"), 
                            selected = "show individual plumes", inline = FALSE),
              
               conditionalPanel(
                 condition = "input.plotPM25 == 'PM2.5 FRM/FEM Mass AQI' | input.plotCO == 'CO AQI' | input.plotOzone == 'Ozone AQI'",
                 img(src="aqi_legend.png", align = "left")
               ),
               
              width = 3
            
  ),
  
    
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
               
               
               
               width="100%")
                

))



library(shiny)
library(leaflet)
library(RColorBrewer)
library(stringr)


shinyUI(fluidPage(

  headerPanel("NOAA Hazard Mapping System Smoke Impact Explorer"),
  
  sidebarPanel(dateInput(inputId="plumeDate", label="Analysis Date", 
                         value = "2012-06-13", 
                         min = "2005-08-05", max = "2015-12-30",
                         format = "yyyy-mm-dd", startview = "month",
                         language = "en", width = "100%"),
            
              radioButtons("plotFires", "", c("show HMS fire clusters", "hide"), 
                           selected = "hide", inline = TRUE),
              
              radioButtons("plotPM25", "", c("show PM2.5 monitors AQI", "hide"), 
                           selected = "hide", inline = TRUE),
              
              radioButtons("mergePlumes", "", c("mergePlumes", "individual"), 
                           selected = "individual", inline = TRUE),
              
              width = 3
            
  ),
  
    
  # Place the map onto the interface   
  leafletOutput("mymap" , width = "70%", height = 700),


  sidebarPanel(h5("Created by:"),
               tags$a("Steven Brey | Ph.D. Student |", 
                      href="http://atmos.colostate.edu/~sjbrey/"),
               tags$a("Source code |", 
                      href="https://github.com/stevenjoelbrey/HMSExplorer"),
               tags$a("NESDIS HMS |", 
                      href="http://www.ospo.noaa.gov/Products/land/hms.html"),
               #tags$a("EPA monitor data", 
               #       href="http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html"),
               #p("If you find bugs or have questions, please contact me via the link above"),
               h5(textOutput("counter")),
               
               
               
               width="100%")
                

))

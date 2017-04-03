

library(shiny)
library(leaflet)
library(RColorBrewer)
library(stringr)


shinyUI(fluidPage(

  headerPanel("NOAA Hazard Mapping System Smoke Impact Explorer"),
  
  sidebarPanel(dateInput(inputId="plumeDate", label="Analysis Date", 
                         value = "2012-07-04", 
                         min = "2005-08-05", max = "2015-12-30",
                         format = "yyyy-mm-dd", startview = "month",
                         language = "en", width = "100%"),
            
              radioButtons("mergePlumes", "", c("mergePlumes", "individual"), 
                           selected = "individual", inline = TRUE),
              
              width = 3
            
  ),
  
    
  # Place the map onto the interface   
  leafletOutput("mymap" , width = "70%", height = 700),


  sidebarPanel(h5("Created by:"),
               tags$a("Steven Brey | Ph.D. Student |", 
                      href="http://atmos.colostate.edu/~sjbrey/"),
               tags$a("Source code", 
                      href="https://github.com/stevenjoelbrey/HMSExplorer"),
               #p("If you find bugs or have questions, please contact me via the link above"),
               h5(textOutput("counter")),
               
               width="100%")
                

))

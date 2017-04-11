

library(shiny)
library(leaflet)
library(stringr)

loading <- "True"

shinyUI(fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  headerPanel("Smoke Impact Explorer"),
  
  # conditionalPanel(condition = "loading== 'True'",
  #                  img="ajax_loader_blue_512.gif"),
  
  leafletOutput("mymap" , width = "100%", height = 710),
  absolutePanel(top = 70, left = 70,
                
                dateInput(inputId="plumeDate", label="Analysis Date", 
                          value = "2012-06-13", 
                          min = "2005-08-05", max = "2015-12-30",
                          format = "yyyy-mm-dd", startview = "month",
                          language = "en", 
                          width=110)
  ),

  absolutePanel(bottom=100,
      img(src="aqi_legend.png", align = "left", width=150)
  ),

  absolutePanel(bottom=0, left=20,
                
                tags$a("Steven Brey | Ph.D. Student |", 
                      href="http://atmos.colostate.edu/~sjbrey/"),
               tags$a("Source code |", 
                      href="https://github.com/stevenjoelbrey/HMSExplorer"),
               tags$a("NESDIS HMS |", 
                      href="http://www.ospo.noaa.gov/Products/land/hms.html"),
               tags$a("EPA monitor data |", 
                      href="http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html"),
               tags$a("Reported Fires Data", 
                      href="https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.3/"),
               
               p("If you find bugs or have questions, please contact me via the link above"),
               h5(textOutput("counter")),
               
               #downloadButton(hysplitPoints_land, label = "Download Fire Occurance Data", class = NULL),
               
               width="100%")
                

))

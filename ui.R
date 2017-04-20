

library(shiny)
library(leaflet)
library(stringr)
library(sp)
library(plotly)

loading <- "True"

shinyUI(fluidPage(

  headerPanel("Smoke Impact Explorer (beta)"),
 
  leafletOutput("map" , width = "100%", height = 700),
  
  absolutePanel(top = 70, left = 70, width="400px",
                
                dateInput(inputId="plumeDate", label="Mapped Date",
                          value = "2012-06-13",
                          min = "2005-08-05", max = "2015-12-30",
                          format = "yyyy-mm-dd", startview = "month",
                          language = "en",
                          width=110),
                
                selectInput(inputId="analysisType", label="Analysis Type",
                            choices= c("none", "overlap", "AQI series", 
                                       "html scatter"),
                            selected="none",
                            width=110
                            ),

                # Show certain overlap tools 
                conditionalPanel(
                  
                  condition = "input.analysisType == 'overlap'",
                  
                  bootstrapPage(
                    div(style="display:inline-block",textInput(inputId="lat", label="lat", value = 41.87898, width=80)),
                    div(style="display:inline-block",textInput(inputId="lon", label="lon", value = -87.635556, width=80)),
                    div(style="display:inline-block", actionButton(inputId="plotPaddle", label="show location", width=110))
                  )
                
                ),
                
                # Show time series plot when selected 
                conditionalPanel(
                  
                  condition = "input.analysisType == 'AQI series'",
                  plotOutput("seriesPlot", width="100%") 
             
                ),
                
                conditionalPanel(
                  condition = "input.analysisType == 'html scatter'",
                  plotlyOutput("scatter", width="100%") 
                )
               
                
                
  ),

  absolutePanel(bottom=100, right=20,
      img(src="aqi_legend.png", align = "right", width=150)
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
               tags$a("Reported Fires Data |", 
                      href="https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.3/"),
               tags$a("Current Conditions from AirNow.gov", 
                      href="https://airnow.gov/index.cfm?action=topics.smoke_wildfires"),
               
               p("If you find bugs or have questions, please contact me via the link above"),
               h5(textOutput("counter")),
               
               #downloadButton(hysplitPoints_land, label = "Download Fire Occurance Data", class = NULL),
               
               width="100%")
                

))

# TODO: Add MODIS active fire data as well as current HMS analysis data layers. 
# TODO: This means that you set all layers to hide and instead of choosing a date
# TODO: you say "today". This will be tricky to integrate with the analysis features.

library(shiny)
library(leaflet)
library(stringr)
library(sp)
library(plotly)

loading <- "True"

shinyUI(fluidPage(
  
  headerPanel("Smoke Impact Explorer (beta)"),
  
  tabsetPanel(
    tabPanel(title="Map", 
             
             leafletOutput("map" , width = "100%", height = 700),
             # 120
             absolutePanel(top = 120, left = 70, width="400px",
                           
                           dateInput(inputId="plumeDate", label="Mapped Date",
                                     value = "2012-06-13",
                                     min = "2005-08-05", max = "2015-12-30",
                                     format = "yyyy-mm-dd", startview = "month",
                                     language = "en",
                                     width=110),
                           
                           selectInput(inputId="analysisType", label="Analysis Type",
                                       choices= c("none", "overlap", "AQI scatter", 
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
                             condition = "input.analysisType == 'AQI scatter'",
                             br(),
                             plotOutput("seriesPlot", width="100%") 
                             
                           ),
                           
                           conditionalPanel(
                             condition = "input.analysisType == 'html scatter'",
                             plotlyOutput("scatter", width="100%") 
                           )

                           
             ),
             
             # absolutePanel(bottom=100, right=20,
             #               img(src="aqi_legend.png", align = "right", width=150)
             # ),
             
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
                           
                           h5(textOutput("counter")),
                           
                           #downloadButton(hysplitPoints_land, label = "Download Fire Occurance Data", class = NULL),
                           
                           width="100%")
            
    ), # End of map tab panel 
    
    tabPanel("README", 
             
      h3("Welcome the the Smoke Impact Explorer shiny application!"),
      p("This website plots publically available data created by U.S government agencies. The code that created this app, along with the underlying data is available for anyone to download, modify, and reuse as they please.  Please note that this app is a side project of mine, and I make no guarantees of the accuracy of the information plotted within or analysis done using this app. That being said, the information presented is accurate to my knowledge (sans typos, bugs, which my the way, would be great if you told me about if you find any)."),
             
      h4('How to use this app'),
      p("All of these data layers can be displayed or hidden by checking the box next to them in the panel that appears on the top right corner of the map. The date of the displayed HMS smoke plumes, HMS fire locations, USFS reported fires, and ambient air quality data can be changed with the calendar widget in the upper left hand corner of the map. Everything on the map except smoke plumes can be clicked for more information. Under the date picker there is a drop down menu for analysis options. The first option is 'overlap' which when selected will show fields for entering a latitude and longitude. Clicking the button 'show location' will place a flag at the entered location and perform a point over polygon calculation for the selected date. This calculation determines whether a smoke plume overlapped that location for the selected date. The result of this calculation can be shown by hovering the mouse over the flag. The 'AQI scatter' option will plot monitor values vs. time for the most recently clicked monitor. The values are color coded by the AQI. Values were no AQI was provided are left black. Currently the plot spans the entire year of the selected date or range of available data for that monitor for that year. The 'html scatter' option is the same except instead of coloring the dots by AQI the dots can be clicked for more information."),
      
      h4("HMS fire and smoke products"),
      p("This app displays fire locations and smoke plume locations as analyzed by the National Environmental Satellite, Data, and Information Service (NESDIS) Hazard Mapping System (HMS). These data layers are labelled ‘HMS Fires’ and ‘HMS Smoke Plumes’. These fires and plume locations are generated by human analyst who determine the locations primarily using visible satellite imagery. I estimate what land cover is associated with the location of each individual fire location (fire icons). Currently the span of HMS data available in this app are August 5 2005 to December 31 2015. 2016, 2017, and same day analysis is under development. The HMS office provided the duration estimate, which describes how many hours a given location was observed to be producing smoke. While the HMS data is publically available I would strongly recommend contacted Mark Ruminski at NESDIS before using it since the data does have some key limitations that must be understood before using in scientific research. The HMS is described in peer reviewed literature. 
        "),
    
      h4("Ambient air quality data"),
      p("All air quality data displayed on this site are from the EPA Air quality system database. All values displayed are daily mean, except ozone, which shows the maximum 8-hour average. Additional details, as well as links to download the data are available http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html. The colorbar in the bottom right side of the map shows the EPA legend for the Air Quality Index (AQI), and the monitors are shaded by these values based on the reported AQI value for a given date. When no AQI estimate is available the data is shaded black. More about the EPA AQI can be found here https://airnow.gov/index.cfm?action=aqibasics.aqi"),
    
      h4("USFS fire location data"),
      p("The map data layer labelled “USFS Reported Fires” shows the locations of fires in the United States from 1992 to 2013. The data were generated by the national Fire Program Analysis (FPA) system. The database includes fires reported by various levels of government including federal, state, and local agencies. The data include 1.73 million wildfire records over the 22-year period. For a complete description and link to the raw data go to the following https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.3/.
      ")
    )
    

    
    
  )))

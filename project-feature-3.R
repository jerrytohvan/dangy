#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
packages = c("sp","sf","tidyverse","tmap","jsonlite", "rgdal", "leaflet") 
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p) 
  } 
  library(p,character.only = T) 
}
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Dengue Case Visualisation"),
   navlistPanel(
     "Header",
     tabPanel("Exploratory Data Analysis",
              
              h3("Dengue Spatial Points Map"),
              sidebarPanel(
                sliderInput("yearSlider",
                                        "Year:",
                                        min = 1998,
                                        max = 2018,
                                        value = 1998)
                ),
              leafletOutput("mapPlot", height= 900),
              leafletOutput("dataPoints")
     ),
     tabPanel("Further Analysis",
              h3("This is the second panel")
     ),
     tabPanel("Data Table",
              h3("This is the third panel")
     )
   ),
   # Sidebar with a slider input for number of years
   sidebarLayout(
      sidebarPanel(
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
   
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$mapPlot <- renderLeaflet({
     taiwan_ts_map_sf <- st_read(dsn = "data/TAIWAN_TOWNSHIP", layer = "TOWN_MOI_1071226")
     taiwan_ts_map_st <- st_transform(taiwan_ts_map_sf,crs=3826)
     tmap_mode("plot")
     map <- tm_shape(taiwan_ts_map_st)+
       tm_fill(col="TOWNNAME")+
       tm_borders(col = "grey40",alpha=0.5)
     tmap_leaflet(map)
   })
   
   output$dataPoints <- renderLeaflet({
     df_dengue <- jsonlite::fromJSON("data/dengue_case.json")
     
     # Check for NA values for coordinates
     sum(is.na(df_dengue$Minimum_statistical_area_center_point_X))
     sum(is.na(df_dengue$Minimum_statistical_area_center_point_Y))
     df_dengue<- df_dengue[!is.na(df_dengue$Minimum_statistical_area_center_point_X),]
     df_dengue<- df_dengue[!(df_dengue$Minimum_statistical_area_center_point_X == 'None'),]
     
     # Type conversion
     df_dengue[, c(10,11,19,23,24)] <- sapply(df_dengue[, c(10,11,19,23,24)], as.numeric)
     
     filter_year <- input$yearSlider 
     
     # Filter by year 1998
     df_filtered <- df_dengue %>%
       filter(grepl(filter_year, Onset_day))
     
     # Transform into SF object
     sf_dengue <- st_as_sf(df_filtered, 
                           coords = c("Minimum_statistical_area_center_point_X",
                                      "Minimum_statistical_area_center_point_Y"),
                           crs = 3826)
     
     tmap_mode('view')
     map_dengue <- tm_shape(sf_dengue)+
       tm_bubbles(col = "red",
                  size = 0.1,
                  border.col = "black",
                  border.lwd = 1) 
      # tm_shape(taiwan_ts_map_st)+
      # tm_fill(col="TOWNNAME")
     tmap_leaflet(map_dengue)
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


# Define server logic required to draw a histogram

function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$mapPlot <- renderLeaflet({
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
                          crs =  "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs",na.fail=FALSE)
    
    sf_dengue <- na.omit(sf_dengue)
    sf_dengue <- as(sf_dengue, 'Spatial')
    
    
    map_dengue <- 
      tm_basemap(leaflet::providers$OpenStreetMap)+
      tm_shape(sf_dengue)+
      tm_dots(col = "red",
                 border.col = "black",
                 border.lwd = 1) 
    #tm_shape(taiwan_ts_map_st)+
    #tm_fill(col="TOWNNAME")
    tmap_leaflet(map_dengue)
    
  })
   
}


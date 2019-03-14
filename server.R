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
    tmap_leaflet(map_dengue)
    
  })
  
  # ===========  Feature 3 - Distribution Plot  =============
  
  output$barplot <- renderPlotly({
    filter_year <- input$yearSlider 
    
    # Filter by year 1998
    df_filtered <- df_dengue %>%
      filter(grepl(filter_year, Onset_day))
    
    
    # Rearrange the x axis for age-group
    if (input$filters == "age_group") {
      agg_age <- df_filtered %>% 
        dplyr::mutate(age_group = factor(age_group, 
                                         levels = c("0", "1", "2", "3", "5-9", "10-14", "15-19", "20-24","25-29",
                                                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                    "60-64","65-69", "70+"))) %>%
        group_by(age_group) %>%
        summarise(total_cases = n())
      
      ggplot(agg_age, aes(y = total_cases, x = age_group)) +
        geom_bar(stat= "identity", fill = "#0073C2FF") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Number of cases", x = "Age Group")
    } else if (input$filters == "gender") {
      agg_gender <- df_filtered %>%
        group_by(gender, Living_county) %>%
        summarise(total_cases = n())
      
      ggplot(agg_gender, aes(x = Living_county, y = total_cases, fill=gender)) +
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
        labs(y = "Number of cases", x = "Gender")
    } else {
      agg_county <- df_filtered %>%
        group_by(Living_county) %>%
        summarise(total_cases = n())
      
      ggplot(agg_county, aes(x = Living_county, y = total_cases)) +
        geom_bar(stat="identity", fill="#0073C2FF") +
        #geom_text(size = 3, stat = 'count',aes(label =..count.., vjust = -0.4)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Number of cases", x = "County")
      
    }
  })
  # ===========  Feature 5 - Cases over Time Plot  =============
  output$timeplot <- renderPlotly({
    filter_year <- input$yearSlider 
    
    # Filter by year 1998
    df_filtered <- df_dengue %>%
      filter(grepl(filter_year, Onset_day))
    
    agg_date <- df_filtered %>%
      group_by(Onset_day) %>%
      summarise(total_cases = n())
    
    ggplot(data = agg_date) +
      geom_line(aes(x = Onset_day, y = total_cases), 
                color = "#09557f",
                alpha = 0.6,
                size = 0.6) +
      labs(x = "Date", 
           y = "Cases",
           title = "Case Count over Months") +
      scale_x_date(
        labels = date_format("%Y-%m"),
        breaks = "1 month") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  # ===========  Feature 5 & 6 - Data Table View  =============
  

  # Filter data based on selections
  output$district_cases_table <- DT::renderDataTable(DT::datatable({
    data <- selected_aggregated_temp
    if (input$months != "All") {
      data <- data[data$MONTH == input$months,]
    }
    data
  }))
 
    output$infected_countries <- DT::renderDataTable(DT::datatable({
    data <- infected_countries_aggregate
    if (input$local != "Both") {
      if (input$local == "Taiwan Local") {
        data <- data[data$Infected_country == "Republic of China",]
      }else{
        data <- data[data$Infected_country != "Republic of China",]
      }
    }
    data
  }))
   
}


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
      tm_view(alpha = 1, set.zoom.limits = c(8,21)) +
      tm_shape(sf_dengue)+
      tm_dots(col = "red",
                 border.col = "black",
                 border.lwd = 1) 
    tmap_leaflet(map_dengue)
    
  })
  
  # ===========  Feature 2 - Distribution of Cases over Years Plot  =============
  output$mainplot <- renderPlotly({
    
    agg_date <- df_dengue %>% 
      dplyr::mutate(Onset_Year = format(Onset_day, "%Y")) %>%
      group_by(Onset_Year) %>%
      summarise(total_cases = n())
    
    ggplot(agg_date, aes(y = total_cases, x = Onset_Year)) +
      geom_bar(stat= "identity", fill = "#0073C2FF") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Number of cases", x = "Year")
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
      dplyr::mutate(Onset_Month = format(Onset_day, "%m")) %>%
      group_by(Onset_Month) %>%
      summarise(total_cases = n())
    
    ggplot(agg_date, aes(y = total_cases, x = Onset_Month)) +
      geom_bar(stat= "identity", fill = "#0073C2FF") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Number of cases", x = "Month")
    
    #ggplot(data = agg_date) +
    #  geom_line(aes(x = Onset_day, y = total_cases), 
    #            color = "#09557f",
    #            alpha = 0.6,
    #            size = 0.6) +
    #  labs(x = "Date", 
    #       y = "Cases",
    #       title = "Case Count over Months") +
    #  scale_x_date(
    #    labels = date_format("%Y-%m"),
    #    breaks = "1 month") +
    #  theme_minimal() +
    #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  observeEvent(input$add, {
    shinyjs::addClass(selector = "html", class = "shiny-busy")
  })
  observeEvent(input$remove, {
    shinyjs::removeClass(selector = "html", class = "shiny-busy")
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
  #========= Spatial Temporal =========
    observeEvent(input$sptem_gen_btn, {
      
      # output$imageplot <- renderImage(
      #   {
      #     return(list(
      #       src = "res/load.gif",
      #       contentType = "image/gif",
      #       alt = "Now loading!"
      #     ))
      #   }
      # )
      
      output$my_dump = renderText({ 
        "loading started"
      })
      
      printVerbose("Loading started.",output)
      print("Loading started.")
      
      date_list= list()
      list_i = 1
      
      if (input$analysis_mode=="1 Year"){
        printVerbose("'1 Year' Selected",output)
        print("'1 Year' Selected")
        for(month in c(1:12)){
          start_date = as.Date(paste(input$sptem_yearpick,"-",month,"-",1,sep=""))
          if(month != 12){
            end_date = as.Date(paste(input$sptem_yearpick,"/",month+1,"/",1,sep="")) - 1
          }else{
            end_date = as.Date(paste(start_year,"-",12,"-",31,sep=""))
          }
          date_list[[list_i]] = c(paste(start_date),paste(end_date))
          list_i = list_i+1
        }
      }else if(input$analysis_mode=="12 Weeks"){
        printVerbose("'12 Weeks' Selected",output)
        print("'12 Weeks' Selected")
        end_date = as.Date(input$sptem_datepick)
        for(week in c(1:12)){
          t_start_date = end_date
          end_date = t_start_date + 6
          date_list[[list_i]] = c(paste(t_start_date),paste(end_date))
          
          end_date = end_date+1
          list_i = list_i+1
        }
      }else if(input$analysis_mode=="14 Days"){
        printVerbose("'14 Days' Selected",output)
        print("'14 Days' Selected")
        t_start_date = as.Date(input$sptem_datepick)
        for(day in c(0:13)){
          date_list[[list_i]] = c(paste(t_start_date+day),paste(t_start_date+day))
          list_i = list_i+1
        }
      }else{
        print("Fallen.")
      }
      
      printVerbose("Generating OWIN",output)
      print("Generating OWIN")
      
      if(input$sptem_regionpick == "All"){
        tw_owin <- as(taiwan_ts_map_sp, "owin")
        tw_bb <- bb(taiwan_ts_map_sp)
      }else{
        area_sf = taiwan_ts_map_sf[taiwan_ts_map_sf$TOWNENG==input$sptem_regionpick,]
        area_sp = as(area_sf,"Spatial")
        tw_owin <- as(area_sp, "owin")
        tw_bb <- bb(area_sp)
      }
      
      tw_osm <- read_osm(tw_bb, type="osm")
      
      spatpoint_list= list()
      list_i = 1
      
      printVerbose("Extracting datapoints from dates",output)
      print("Extracting datapoints from dates")
      
      for(date_range in date_list){
        dengue_pt_range = df_dengue2 %>%
          filter(as.Date(Onset_day) >= date_range[1] & as.Date(Onset_day)<= date_range[2])%>%
          st_as_sf(coords = c("x","y"),
                   crs = "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs") %>%
          as('Spatial') 
        spatpoint_list[[list_i]] = dengue_pt_range
        list_i = list_i + 1
      }
      ppp_list= list()
      list_i = 1
      
      printVerbose("Converting Spatpoints to PPP",output)
      print("Converting Spatpoints to PPP")
      
      for(spatpoint in spatpoint_list){
        ppp_range = as(spatpoint_list[[list_i]],"ppp")
        
        ppp_list[[list_i]] = ppp_range
        list_i = list_i + 1
      }
      
      list_i = 1
      
      printVerbose("Matching PPP with OWIN",output)
      print("Matching PPP with OWIN")
      
      for(ppp_range in ppp_list){
        ppp_list[[list_i]] = ppp_list[[list_i]][tw_owin]
        printVerbose(paste(round(list_i/length(ppp_list)*100,2),"%",sep=""),output)
        print(paste(round(list_i/length(ppp_list)*100,2),"%",sep=""))
        list_i = list_i + 1
      }
      
      tmap_mode("plot")
      
      if (dir.exists("plots")) {
        unlink("plots", recursive = TRUE)
      }
      dir.create("plots")
      
      plot_list = list()
      
      list_i = 1
      
      printVerbose("Generating Density Maps",output)
      print("Generating Density Maps")
      
      for(ppp_range in ppp_list){
        t_kde_taiwan_bw <- density(ppp_range, sigma=input$sptem_sigpick, edge=TRUE, kernel=input$sptem_kernelpick)
        plot_list[[list_i]] = t_kde_taiwan_bw
        printVerbose(round(list_i/length(ppp_list)*100,2),output)
        print(round(list_i/length(ppp_list)*100,2))
        list_i = list_i+1
      }
       
      tmap_mode("view") 
      
      min_val= .Machine$integer.max
      max_val=0
      
      for(plot_a in plot_list){
        plot_a$v[is.na(plot_a$v)] <- 0
        if(min(plot_a$v)<min_val){
          min_val=min(plot_a$v)
        }
        if(max(plot_a$v)>max_val){
          max_val=max(plot_a$v)
        }
      }
      
      v_range = ceiling(max_val) - floor(min_val)
      r_interval = ceiling(v_range/input$sptem_binpick)
      bins = seq(0,r_interval*input$sptem_binpick,r_interval)
      
      
      list_i = 1
      
      printVerbose("Output to PNG",output)
      print("Output to PNG")
      
      for(kde_taiwan_bw in plot_list){
        gridded_kde_taiwan_bw<- as.SpatialGridDataFrame.im(kde_taiwan_bw)
        
        kde_taiwan_bw_raster <- raster(gridded_kde_taiwan_bw)
        
        projection(kde_taiwan_bw_raster) =  crs(taiwan_ts_map_sf)
        
        map <- 
          tm_shape(tw_osm)+
          tm_rgb() +
          tm_shape(kde_taiwan_bw_raster)+
          tm_raster("v", alpha = 0.65, style="fixed", breaks=bins )+ 
          tm_layout(paste("Dengue Outbreak Distribution in",date_list[[list_i]][1], "to", date_list[[list_i]][2]), 
                    title.size = 1, 
                    title.position = c("right","top"),
                    legend.title.size = 1,
                    legend.text.size = 0.7,
                    legend.position = c("right","bottom"))
        tmap_save(map, filename=paste("plots/plot",list_i,".png", sep="" ))
        printVerbose(paste("PNG frame saved for plot",list_i),output)
        print(paste("PNG frame saved for plot",list_i))
        list_i = list_i+1
      }
      
      plot_dir_list = list.files(path = "plots",full.names = TRUE, recursive = TRUE)
      plot_dir_list = mixedsort(sort(plot_dir_list))
      
      img_list = list()
      list_i = 1
      
      for(file_n in plot_dir_list){
        img_list = append(img_list,image_read(file_n))
      }
      
      img_list <- image_scale(img_list, "500x500")
      #image_animate(image_scale(img_list, "500x500"), fps = 4, dispose = "previous")
      
      output$imageplot <- renderImage(
        {
          return (NULL)
        }
      )
      
      #====output=====#
      output$my_dump = renderText({ 
        paste(date_list, collapse=",")
      })
      
    })
    
}


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
    shinyjs::disable("sptem_gen_btn")
    output$sptem_gifplot <- renderImage(
      {
        return(NULL)
      }
    )
    
    output$my_dump = renderText({
      "loading started"
    })
    print("Loading started.")
    
    date_list= list()
    list_i = 1
    
    if (input$analysis_mode=="1 Year"){
      print("'1 Year' Selected")
      for(month in c(1:12)){
        start_date = as.Date(paste(input$sptem_yearpick,"-",month,"-",1,sep=""))
        if(month != 12){
          end_date = as.Date(paste(input$sptem_yearpick,"/",month+1,"/",1,sep="")) - 1
        }else{
          end_date = as.Date(paste(input$sptem_yearpick,"-",12,"-",31,sep=""))
        }
        date_list[[list_i]] = c(paste(start_date),paste(end_date))
        list_i = list_i+1
      }
    }else if(input$analysis_mode=="12 Weeks"){
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
      print("'14 Days' Selected")
      t_start_date = as.Date(input$sptem_datepick)
      for(day in c(0:13)){
        date_list[[list_i]] = c(paste(t_start_date+day),paste(t_start_date+day))
        list_i = list_i+1
      }
    }else{
      print("Fallen.")
    }
    
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
    
    print("Extracting datapoints from dates")
    
    
    withProgress(message = 'Extracting Datapoints from Dates', value = 0, {
      for(date_range in date_list){
        dengue_pt_range = df_dengue2 %>%
          filter(as.Date(Onset_day) >= date_range[1] & as.Date(Onset_day)<= date_range[2])%>%
          st_as_sf(coords = c("x","y"),
                   crs = "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs") %>%
          as('Spatial')
        spatpoint_list[[list_i]] = dengue_pt_range
        list_i = list_i + 1
        incProgress(1/length(date_list), detail = paste("Extracting data points for plot ", list_i))
      }
    })
    ppp_list= list()
    list_i = 1
    
    print("Converting Spatpoints to PPP")
    
    withProgress(message = 'Converting Spatpoints to PPP', value = 0, {
      for(spatpoint in spatpoint_list){
        ppp_range = as(spatpoint_list[[list_i]],"ppp")
        
        ppp_list[[list_i]] = ppp_range
        
        list_i = list_i + 1
        incProgress(1/length(spatpoint_list), detail = paste("Converting spatpoints for plot ", list_i))
      }
    })
    
    list_i = 1
    
    print("Matching PPP with OWIN")
    
    withProgress(message = 'Confining PPP with OWIN', value = 0, {
      for(ppp_range in ppp_list){
        ppp_list[[list_i]] = ppp_list[[list_i]][tw_owin]
        print(paste(round(list_i/length(ppp_list)*100,2),"%",sep=""))
        list_i = list_i + 1
        incProgress(1/length(ppp_list), detail = paste("Confining points for plot ", list_i))
      }
    })
    
    tmap_mode("plot")
    
    if (dir.exists("plots")) {
      unlink("plots", recursive = TRUE)
    }
    dir.create("plots")
    
    plot_list = list()
    
    list_i = 1
    
    print("Generating Density Maps")
    withProgress(message = 'Generating Density Maps', value = 0, {
      for(ppp_range in ppp_list){
        t_kde_taiwan_bw <- density(ppp_range, sigma=input$sptem_sigpick, edge=TRUE, kernel=input$sptem_kernelpick)
        plot_list[[list_i]] = t_kde_taiwan_bw
        print(round(list_i/length(ppp_list)*100,2))
        list_i = list_i+1
        incProgress(1/length(ppp_list), detail = paste("Generating KDE map for plot ", list_i))
      }
    })
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
    
    print("Output to PNG")
    
    withProgress(message = 'Output maps to PNG', value = 0, {
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
        print(paste("PNG frame saved for plot",list_i))
        list_i = list_i+1
        incProgress(1/length(plot_list), detail = paste("Generating PNG for plot ", list_i))
      }
    })
    
    plot_dir_list = list.files(path = "plots",full.names = TRUE, recursive = TRUE)
    plot_dir_list = mixedsort(sort(plot_dir_list))
    
    img_list = list()
    list_i = 1
    
    for(file_n in plot_dir_list){
      img_list = append(img_list,image_read(file_n))
    }
    
    img_list <- image_scale(img_list, "900x900")
    exp_gif = image_animate(image_scale(img_list, "900x900"), fps = 2, dispose = "previous")
    image_write(exp_gif, path = "plots/exp_.gif", format = "gif")
    
    output$sptem_gifplot <- renderImage(
      {
        return(list(
          src = "plots/exp_.gif",
          filetype = "image/gif",
          alt = "spatial temporal gif plot"
        ))
      }
    )
    shinyjs::enable("sptem_gen_btn")
  })
  
  
  #========= STTP Temporal =========
  
  observeEvent(input$sttp_gen_btn, {
    shinyjs::disable("sttp_gen_btn")
    output$sttp_plot <- renderPlot(
      {
        return(NULL)
      }
    )
    output$sttp_gifplot<- renderImage(
      {
        return(NULL)
      }
    )
    
    output$my_dump = renderText({
      "loading started"
    })
    print("Loading started...")
    #filter sfdengue here
    daterange_start <- input$daterange3[1]
    daterange_end <- input$daterange3[2]
    # sf_dengue_duplicate <-  sf_dengue[year(sf_dengue@data$Onset_day) == input$sttp_yearpick,]
    sf_dengue_duplicate <-  sf_dengue[as.Date(sf_dengue@data$Onset_day) >= daterange_start & as.Date(sf_dengue@data$Onset_day) < daterange_end ,]
    
    #Validation
    if(nrow(sf_dengue_duplicate@data)==0){
      output$validation_text <- renderText({
        "No dengue cases identified, please select other date."
      })
      return(NULL)
    }
    
    filter_dengue_time <- sf_dengue_duplicate@data %>%
      mutate(DAY_ORDER = yday(as.Date(Onset_day))) 

    
    dengue_3d <- as.3dpoints(sf_dengue_duplicate@coords[,1],sf_dengue_duplicate@coords[,2],filter_dengue_time$DAY_ORDER)
    print("OK")
    
    counter=c()
    max=1
    prev=1
    for(i in 1:length(taiwan.union@polygons[[1]]@Polygons)){
      max=max(c(length(taiwan.union@polygons[[1]]@Polygons[[i]]@coords[,1]),max))
      if(max>prev){
        counter=i
      }
      prev=max
    }

    taiwan_main <- taiwan.union@polygons[[1]]@Polygons[[counter]]@coords
    
    
    print("Rendering Plots...")
    
    
    output$sttp_plot_1 <- renderPlot(
      {
        # xy-locations and cumulative distribution of the times
        plot(dengue_3d,s.region=taiwan_main,col="red",type="projection")
      }
    )
    output$sttp_plot_2 <- renderPlot(
      {
        #space-time 3D scatter
        plot(dengue_3d ,s.region=taiwan_main,col="red",type="scatter" )
      }
    )
    output$sttp_plot_3 <- renderPlot(
      {
        #the time-mark and space-mark.
        plot(dengue_3d,s.region=taiwan_main,type="mark")
      }
    )
    
    
    
    
    print("Preparing Spatial Points Animation...")
    
    taiwan_map <- fortify(taiwan)
    datetime <- as.data.frame(dengue_3d[,3])
    taiwan_plot <- ggplot(data=taiwan_map, aes(x = long, y = lat, group=group))+
      geom_path() + 
      coord_map()
    dengue_points <- as.data.frame(sf_dengue_duplicate) %>%
      dplyr::select(Onset_day, coords.x1,coords.x2)
    
    dengue_points$Onset_day <- as.Date(dengue_points$Onset_day)
    dengue_points <- dengue_points%>%
      mutate(months=as.numeric(format(Onset_day, "%m")))
    date_dengue <- as.data.frame(as.Date(dengue_points$Onset_day))
    date_dengue$coords.x1 <- 120.2900
    date_dengue$coords.x2 <- 22.68133
    colnames(date_dengue) <- c("start_date","coords.x1","coords.x2")
    start_date <- date_dengue[1,]
    
    date_dengue<-date_dengue[!(date_dengue$start_date==start_date$start_date),]
    date_dengue<-unique(date_dengue)
    
    print("Preparing map layers...")
    
    map <-taiwan_plot+
      geom_point(mapping = aes(x = coords.x1, y = coords.x2, frame=Onset_day,cumulative=TRUE),  data = dengue_points,  colour = 'red', alpha = .2, inherit.aes = FALSE)+
      geom_point(mapping = aes(x = coords.x1, y = coords.x2, frame=Onset_day),  data = dengue_points,  colour = 'red', alpha = .5, inherit.aes = FALSE)+
      geom_point(aes(x = coords.x1, y = coords.x2, # this is the init transparent frame
                     frame = start_date,
                     cumulative = TRUE),
                 data = start_date, alpha = 0, inherit.aes = FALSE) +
      geom_point(aes(x = coords.x1, y = coords.x2, # this is the final transparent frames
                     frame = start_date,
                     cumulative = TRUE),
                 data = date_dengue, alpha = 0, inherit.aes = FALSE) +
      # Here comes the gganimate specific bits
      labs(title = 'Day: ') 
    
    print("Creating animation...")
    
    if (dir.exists("plots")) {
      unlink("plots", recursive = TRUE)
    }
    dir.create("plots")
    animated_points <- gganimate(map, interval = .2, "plots/year-datapoints.gif")
    

    print("OK")
    
    
    print("Rendering Dengue Spread Patterns...")
    output$sttp_gifplot <- renderImage(
      {
        return(list(
          src = "plots/year-datapoints.gif",
          filetype = "image/gif",
          alt = "dengue points spread trend gif plot"
        ))
      }
    )
    shinyjs::enable("sttp_gen_btn")
  })
}


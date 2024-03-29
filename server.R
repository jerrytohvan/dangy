# Define server logic required to draw a histogram


function(input, output,session) {
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
  

  sf_dengue_map <-  reactive({
    # Check options for data filters
    date_list= list()
    
    if (input$analysis_mode2=="1 Year" || input$analysis_mode2=="-"){
      print("'1 Year' Selected")
      print(input$sptem_yearpick2)
      start_date = as.Date(paste(input$sptem_yearpick2,"-",1,"-",1,sep=""))
      end_date = as.Date(paste(input$sptem_yearpick2,"/",12,"/",31,sep="")) 
      date_list = c(paste(start_date),paste(end_date))
      
    }else if(input$analysis_mode2=="12 Weeks"){
      print("'12 Weeks' Selected")
      t_start_date = as.Date(input$sptem_datepick2)
      end_date = t_start_date + 83
      date_list = c(paste(t_start_date),paste(end_date))
      
    }else if(input$analysis_mode2=="14 Days"){
      print("'14 Days' Selected")
      t_start_date = as.Date(input$sptem_datepick2)
      date_list = c(paste(t_start_date),paste(t_start_date+13))
      
    }else{
      date_list = c("01/01/1998", "01/01/1999")
    }
    
    # Filter by year 1998
    
    df_filtered <- df_dengue %>%
      filter(as.Date(Onset_day) >= date_list[1] & as.Date(Onset_day)<= date_list[2])
    
    # Transform into SF object
    sf_dengue <- st_as_sf(df_filtered, 
                          coords = c("Minimum_statistical_area_center_point_X",
                                     "Minimum_statistical_area_center_point_Y"),
                          crs =  "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs")
    
    # sf_dengue <- na.omit(sf_dengue)
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
      output$dataPoints <- renderLeaflet({
        sf_dengue_map()
  })
  
  output$dataPoints <- renderLeaflet({
  
    sf_dengue_map()
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
    if (input$analysis_mode2=="1 Year" || input$analysis_mode2=="-"){
      print("'1 Year' Selected")
      print(input$sptem_yearpick2)
      start_date = as.Date(paste(input$sptem_yearpick2,"-",1,"-",1,sep=""))
      end_date = as.Date(paste(input$sptem_yearpick2,"/",12,"/",31,sep="")) 
      date_list = c(paste(start_date),paste(end_date))
      
    }else if(input$analysis_mode2=="12 Weeks"){
      print("'12 Weeks' Selected")
      t_start_date = as.Date(input$sptem_datepick2)
      end_date = t_start_date + 83
      date_list = c(paste(t_start_date),paste(end_date))
 
      
    }else if(input$analysis_mode2=="14 Days"){
      print("'14 Days' Selected")
      t_start_date = as.Date(input$sptem_datepick2)
      date_list = c(paste(t_start_date),paste(t_start_date+13))

    }
    
    df_filtered <- df_dengue %>%
      filter(as.Date(Onset_day) >= date_list[1] & as.Date(Onset_day)<= date_list[2])

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
  output$monthplot <- renderPlotly({
    print("Plotting counts over months")
    print(input$sptem_yearpick2)
    start_date = as.Date(paste(input$sptem_yearpick2,"-",1,"-",1,sep=""))
    end_date = as.Date(paste(input$sptem_yearpick2,"/",12,"/",31,sep="")) 
    date_list = c(paste(start_date),paste(end_date))
    print(date_list)
    
    df_filtered <- df_dengue %>%
      filter(as.Date(Onset_day) >= date_list[1] & as.Date(Onset_day)<= date_list[2])
    
    agg_date <- df_filtered %>% 
      dplyr::mutate(Onset_Month = format(Onset_day, "%m")) %>%
      group_by(Onset_Month) %>%
      summarise(total_cases = n())
    
    ggplot(agg_date, aes(y = total_cases, x = Onset_Month)) +
      geom_bar(stat= "identity", fill = "#0073C2FF") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Number of cases", x = "Month")
    
 
  })
  
  output$weekplot <- renderPlotly({
    
    if(input$analysis_mode2=="12 Weeks"){
      print("'12 Weeks' Selected")
      t_start_date = as.Date(input$sptem_datepick2)
      end_date = t_start_date + 83
      date_list = c(paste(t_start_date),paste(end_date))
      print(date_list)
      
      df_filtered <- df_dengue %>%
        filter(as.Date(Onset_day) >= date_list[1] & as.Date(Onset_day)<= date_list[2])
      
      df_filtered$weeks <- cut(df_filtered[,"Onset_day"], breaks="week")
      
      agg_date <- df_filtered %>% 
        group_by(weeks) %>% 
        summarise(total_cases = n())
      
      ggplot(agg_date, aes(y = total_cases, x = weeks)) +
        geom_bar(stat= "identity", fill = "#0073C2FF") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Number of cases", x = "Weeks")
      
    }else if(input$analysis_mode2=="14 Days"){
      print("'14 Days' Selected")
      t_start_date = as.Date(input$sptem_datepick2)
      date_list = c(paste(t_start_date),paste(t_start_date+13))
      print(date_list)
      
      df_filtered <- df_dengue %>%
        filter(as.Date(Onset_day) >= date_list[1] & as.Date(Onset_day)<= date_list[2])
      
      df_filtered$weeks <- cut(df_filtered[,"Onset_day"], breaks="week")
      
      agg_date <- df_filtered %>% 
        group_by(weeks) %>% 
        summarise(total_cases = n())
      
      ggplot(agg_date, aes(y = total_cases, x = weeks)) +
        geom_bar(stat= "identity", fill = "#0073C2FF") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Number of cases", x = "Weeks")
    }
 
   
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
    withProgress(message = 'Loading has started', value = 0, {
      output$my_dump = renderText({
        "loading started"
      })
      print("Loading started.")
      incProgress(0.5, detail ="Processing parameters ")
      
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
    })
    
    print("Generating OWIN")
    withProgress(message = 'Generating OWIN', value = 0, {
      incProgress(0.5, detail ="Preparing OWIN with parameters")
      if(input$sptem_regionpick == "All"){
       
        tw_owin <- as(taiwan_ts_map_sp, "owin")
        tw_bb <- bb(taiwan_ts_map_sp)
      }else{
       
        area_sf = taiwan_ts_map_sf[taiwan_ts_map_sf$GG_NAME==input$sptem_regionpick,]
        area_sf = na.omit(area_sf)
        area_sp = as(area_sf,"Spatial")
        tw_owin <- as(area_sp, "owin")
        tw_bb <- bb(area_sp)
      }
      
      tw_osm <- read_osm(tw_bb, type="osm")
    })
    
    spatpoint_list= list()
    list_i = 1
    
    print("Extracting datapoints from dates")
    
    
    withProgress(message = 'Extracting Datapoints from Dates', value = 0, {
      for(date_range in date_list){
        dengue_pt_range_fil = df_dengue2 %>%
          filter(as.Date(Onset_day) >= date_range[1] & as.Date(Onset_day)<= date_range[2])
          
        if(nrow(dengue_pt_range_fil)>0){
          
          dengue_pt_range = dengue_pt_range_fil %>% st_as_sf(coords = c("x","y"),
                     crs = "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs") %>%
            as('Spatial')
        }else{
          dengue_pt_range = "No points"
        }
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
        if(!is.character(spatpoint)){
          
          
          ppp_range = as(spatpoint_list[[list_i]],"ppp")
          
          
        }else{
          ppp_range = "No points"
        }
        ppp_list[[list_i]] = ppp_range
        list_i = list_i + 1
        incProgress(1/length(spatpoint_list), detail = paste("Converting spatpoints for plot ", list_i))
      }
    })
    
    list_i = 1
    
    print("Matching PPP with OWIN")
    
    ppp_list2 = list()
    
    withProgress(message = 'Confining PPP with OWIN', value = 0, {
      for(ppp_range in ppp_list){
        if(!is.character(ppp_range)){
          ppp_list2[[list_i]] = ppp_range[tw_owin]
        }else{
          ppp_list2[[list_i]] = "No points"
        }
        print(paste(round(list_i/length(ppp_list)*100,2),"%",sep=""))
        list_i = list_i + 1
        incProgress(1/length(ppp_list), detail = paste("Confining points for plot ", list_i))
      }
    })
    
    tmap_mode("plot")
    
    if (!dir.exists("plots")) {
      dir.create("plots")
    }else{
      if(dir.exists("plots/temp")){
        unlink("plots/temp", recursive = TRUE)
      }
    }
    dir.create("plots/temp")
    
    plot_list = list()
    
    list_i = 1
    
    print("Generating Density Maps")
    withProgress(message = 'Generating Density Maps', value = 0, {
      for(ppp_range in ppp_list2){
        if(is.character(ppp_range)){
          t_kde_taiwan_bw <- "No points"
        }else{
          if(length(ppp_range$x)<1){
            t_kde_taiwan_bw <- "No points"
          }else{
            t_kde_taiwan_bw <- density(ppp_range, sigma=input$sptem_sigpick, edge=TRUE, kernel=input$sptem_kernelpick)
          }
        }
        plot_list[[list_i]] = t_kde_taiwan_bw
        print(round(list_i/length(ppp_list2)*100,2))
        list_i = list_i+1
        incProgress(1/length(ppp_list2), detail = paste("Generating KDE map for plot ", list_i))
      }
    })
    tmap_mode("view")
    
    min_val= .Machine$integer.max
    max_val=0

    plot_list2 = plot_list
    
    for(plot_a in plot_list){
      if(!is.character(plot_a)){
        plot_a$v[is.na(plot_a$v)] <- 0
        if(min(plot_a$v)<min_val){
          min_val=min(plot_a$v)
        }
        if(max(plot_a$v)>max_val){
          max_val=max(plot_a$v)
        }
      }
    }
    
    v_range = ceiling(max_val) - floor(min_val)
    r_interval = ceiling(v_range/input$sptem_binpick)
    bins = seq(0,r_interval*input$sptem_binpick,r_interval)
    
    list_i = 1
    
    print("Output to PNG")
    
    withProgress(message = 'Output maps to PNG', value = 0, {
      for(kde_taiwan_bw in plot_list2){
        if(is.character(kde_taiwan_bw)){
          map <-
            tm_shape(tw_osm)+
            tm_raster() +
            tm_layout(paste("Dengue Outbreak Distribution in",date_list[[list_i]][1], "to", date_list[[list_i]][2]),
                      title.size = 1,
                      title.position = c("right","top"))
        }else{
          
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
        }
        tmap_save(map, filename=paste("plots/temp/plot",list_i,".png", sep="" ))
        print(paste("PNG frame saved for plot",list_i))
        list_i = list_i+1
        incProgress(1/length(plot_list), detail = paste("Generating PNG for plot ", list_i))
      }
    })
    
    plot_dir_list = list.files(path = "plots/temp",full.names = TRUE, recursive = TRUE)
    plot_dir_list = mixedsort(sort(plot_dir_list))
    
    img_list = list()
    list_i = 1
    
    for(file_n in plot_dir_list){
      img_list = append(img_list,image_read(file_n))
    }
    
    img_list <- image_scale(img_list, "700x700")
    exp_gif = image_animate(image_scale(img_list, "700x700"), fps = 2, dispose = "previous")
    image_write(exp_gif, path = "plots/exp_.gif", format = "gif")
    
    output$sptem_gifplot <- renderImage(
      {
        return(list(
          src = "plots/exp_.gif",
          filetype = "image/gif",
          alt = "spatial temporal gif plot"
        ))
      },deleteFile = FALSE
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
      sf_dengue_duplicate <- sp_dengue[as.Date(sp_dengue@data$Onset_day) >= daterange_start & as.Date(sp_dengue@data$Onset_day) < daterange_end ,]
      
      
      #Validation
      if(nrow(sf_dengue_duplicate@data)==0){
        output$validation_text <- renderText({
          "No dengue cases identified, please select other dates."
        })
        shinyjs::enable("sttp_gen_btn")
        return(NULL)
      }else if(nrow(sf_dengue_duplicate@data)==1){
        output$validation_text <- renderText({
          "Dengue spread analysis requires more than one case, you may reconfigure the date range inputs."
        })
        shinyjs::enable("sttp_gen_btn")
        return(NULL)
      }
      
      filter_dengue_time <- sf_dengue_duplicate@data %>%
        mutate(DAY_ORDER = yday(as.Date(Onset_day))) 
      dengue_3d <- as.3dpoints(sf_dengue_duplicate@coords[,1],sf_dengue_duplicate@coords[,2],filter_dengue_time$DAY_ORDER)

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
    
    withProgress(message = 'Rendering plots', value = 0, {
      output$sttp_plot_1 <- renderPlot(
        {
          # xy-locations and cumulative distribution of the times
          plot(dengue_3d,s.region=taiwan_main,col="red",type="projection")
        }
      )
      incProgress(1/3, detail = "Rendering plot 1")
      output$sttp_plot_2 <- renderPlot(
        {
          #space-time 3D scatter
          plot(dengue_3d ,s.region=taiwan_main,col="red",type="scatter" )
        }
      )
      incProgress(1, detail = "Rendering plot 2")
      output$sttp_plot_3 <- renderPlot(
        {
          #the time-mark and space-mark.
          plot(dengue_3d,s.region=taiwan_main,type="mark")
        }
      )
      incProgress(1, detail = "Rendering plot 3")
    })
    
    
    
    print("Preparing Spatial Points Animation...")
    withProgress(message = 'Preparing Spatial Points Animation', value = 0, {
      taiwan_map <- fortify(taiwan)
      
      incProgress(0.2, detail = "Converting data")

      #BUG - Catch error for 1 point result

      if(nrow(filter_dengue_time)==1){
        print("in")
        datetime <- as.data.frame(dengue_3d[3])
      }else{
        print("out")
        datetime <- as.data.frame(dengue_3d[,3])
      }

            taiwan_plot <- ggplot(data=taiwan_map, aes(x = long, y = lat, group=group))+
        geom_path() + 
        coord_map()
      
      incProgress(0.5, detail = "Creating plot")
      
      dengue_points <- as.data.frame(sf_dengue_duplicate) %>%
        dplyr::select(Onset_day, coords.x1,coords.x2)
      
      dengue_points$Onset_day <- as.Date(dengue_points$Onset_day)
      dengue_points <- dengue_points%>%
        mutate(months=as.numeric(format(Onset_day, "%m")))
      print(dengue_points)
      date_dengue <- as.data.frame(as.Date(dengue_points$Onset_day))
      date_dengue$coords.x1 <- 120.2900
      date_dengue$coords.x2 <- 22.68133
      colnames(date_dengue) <- c("start_date","coords.x1","coords.x2")
      start_date <- date_dengue[1,]
      
      date_dengue<-date_dengue[!(date_dengue$start_date==start_date$start_date),]
      date_dengue<-unique(date_dengue)
    })
    
    print("Preparing map layers...")
    withProgress(message = 'Preparing map layers', value = 0, {
      incProgress(0.5, detail = "Preparing...")
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
    })
    
    
    
    print("Creating animation...")
    withProgress(message = 'Creating animation', value = 0, {
      incProgress(0.2, detail = "Clearing cache")
      if(!dir.exists("plots")){
        dir.create("plots")
      }else{
        if (file.exists("plots/year-datapoints.gif")) {
          unlink("plots/year-datapoints.gif", recursive = TRUE)
        }
      }
      
      incProgress(0.5, detail = "Generating animation")
      animated_points <- gganimate(map, interval = .2, "plots/year-datapoints.gif")
      print("OK")
    })
    
    print("Rendering Dengue Spread Patterns...")
    withProgress(message = 'Rendering Dengue Spread Patters', value = 0, {
      
      incProgress(0.5, detail = "Almost done")
      output$sttp_gifplot <- renderImage(
        {
          return(list(
            src = "plots/year-datapoints.gif",
            filetype = "image/gif",
            alt = "dengue points spread trend gif plot"
          ))
        }
      )
    })
    
    output$validation_text <- renderText({
      ""
    })
    shinyjs::enable("sttp_gen_btn")
  })
  # pryr::mem_used()
}


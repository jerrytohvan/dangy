packages = c("DT","sp","sf","tidyverse","tmap","jsonlite", "rgdal", "leaflet","shiny","ggplot2","dplyr") 

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p) 
  } 
  library(p,character.only = T) 
}

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

taiwan_ts_map_sf <- st_read(dsn = "data/TAIWAN_TOWNSHIP", layer = "TOWN_MOI_1071226")
taiwan_ts_map_st <- st_transform(taiwan_ts_map_sf,crs=3826)
taiwan_ts_map_sp <- as(taiwan_ts_map_st,'Spatial')

df_dengue <- jsonlite::fromJSON("data/dengue_case.json")
sum(is.na(df_dengue$Minimum_statistical_area_center_point_X))
sum(is.na(df_dengue$Minimum_statistical_area_center_point_Y))
df_dengue<- df_dengue[!is.na(df_dengue$Minimum_statistical_area_center_point_X),]
df_dengue<- df_dengue[!(df_dengue$Minimum_statistical_area_center_point_X == 'None'),]

# Type conversion
df_dengue[, c(10,11,19,23,24)] <- sapply(df_dengue[, c(10,11,19,23,24)], as.numeric)



# Transform into SF object
sf_dengue <- st_as_sf(df_dengue, 
                      coords = c("Minimum_statistical_area_center_point_X",
                                 "Minimum_statistical_area_center_point_Y"),
                      crs =  "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs",na.fail=FALSE)

sf_dengue <- na.omit(sf_dengue)
sf_dengue <- as(sf_dengue, 'Spatial')

projection(taiwan_ts_map_sp) <- sf_dengue@proj4string
taiwan_ts_map_sp@data$id <- 1:nrow(taiwan_ts_map_sp)

#join poly and points
pts.poly <- point.in.poly(sf_dengue, taiwan_ts_map_sp)

#assign unique id per poly, full info


#descendng order of town with most
#FILTER: Months, Gender
#DISPLAY: COUNTY, GENDER, TOWNID, TOWNNAME, TOWNENG, total_cases, ....
selected_aggregated_temp <-pts.poly@data %>% dplyr::select(id,TOWNID,TOWNNAME, TOWNENG, Onset_day, Case_study_date, Notification_day) %>% 
  mutate(MONTH = months(as.Date(pts.poly@data$Onset_day))) %>%
  group_by(id,TOWNID, TOWNENG,MONTH) %>% summarise(total_cases = n())
selected_aggregated <- left_join(selected_aggregated_temp, taiwan_ts_map_sp@data, by=c("TOWNID"))


infected_countries_aggregate <-pts.poly@data %>% 
  dplyr::select(Onset_day, Infected_country, Infected_counties_and_cities, Infected_village)%>% 
  mutate(MONTH = months(as.Date(pts.poly@data$Onset_day))) %>%
  group_by( MONTH, Infected_country, Infected_counties_and_cities, Infected_village) %>% summarise(total_cases = n())

tmap_mode('view')
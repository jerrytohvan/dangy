packages = c("rsconnect","rjson","DT","sp","sf","tidyverse","tmap","jsonlite","geojsonio", "rgdal", "leaflet","shiny","ggplot2","dplyr", "raster","spatialEco","GISTools", "plotly", "scales", "shinyjs", "shinyBS") 

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p) 
  } 
  library(p,character.only = T) 
}

# rsconnect::setAccountInfo(name='dangy',
#                           token='B9D18F7AF49EB8DB4EFA8EEEC1682918',
#                           secret='vr4aq0OamiPwv5VOslTXsAJyJlpqRdaAT+Df8Mum')
# rsconnect::deployApp(server="shinyapps.io")

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

#taiwan_ts_map_sf <- st_read(dsn = "data/TAIWAN_TOWNSHIP", layer = "TOWN_MOI_1071226")
#taiwan_ts_map_st <- st_transform(taiwan_ts_map_sf,crs=3826)
taiwan_ts_map_sp <- readOGR(dsn = "data/TAIWAN_TOWNSHIP", layer = "TOWN_MOI_1071226", stringsAsFactors=TRUE)


df_dengue <- jsonlite::fromJSON("data/dengue_case.json")
sum(is.na(df_dengue$Minimum_statistical_area_center_point_X))
sum(is.na(df_dengue$Minimum_statistical_area_center_point_Y))
df_dengue<- df_dengue[!is.na(df_dengue$Minimum_statistical_area_center_point_X),]
df_dengue<- df_dengue[!(df_dengue$Minimum_statistical_area_center_point_X == 'None'),]

# Type conversion
df_dengue[, c(10,11,19,23,24)] <- sapply(df_dengue[, c(10,11,19,23,24)], as.numeric)
# Date conversion
df_dengue[, "Onset_day"] <- as.Date(df_dengue$Onset_day, "%Y/%m/%d")
df_dengue[, "Case_study_date"] <- as.Date(df_dengue$Case_study_date, "%Y/%m/%d")
df_dengue[, "Notification_day"] <- as.Date(df_dengue$Notification_day, "%Y/%m/%d")


# Transform into SF object
sf_dengue <- st_as_sf(df_dengue, 
                      coords = c("Minimum_statistical_area_center_point_X",
                                 "Minimum_statistical_area_center_point_Y"),
                      crs =  "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs",na.fail=FALSE)

sf_dengue <- na.omit(sf_dengue)
sf_dengue <- as(sf_dengue, 'Spatial')

tmap_mode('view')

projection(taiwan_ts_map_sp) <- sf_dengue@proj4string
taiwan_ts_map_sp@data$poly.ids <- 1:nrow(taiwan_ts_map_sp)

#join poly and points
pts.poly <- point.in.poly(sf_dengue, taiwan_ts_map_sp)

#assign unique id per poly, full info

#descendng order of town with most
#FILTER: Months, Gender
#DISPLAY: COUNTY, GENDER, TOWNID, TOWNNAME, TOWNENG, total_cases, ....
selected_aggregated_temp <-pts.poly@data %>% 
  dplyr::select(poly.ids, TOWNNAME, TOWNENG, Onset_day, Case_study_date, Notification_day) %>% 
  mutate(MONTH = months(as.Date(pts.poly@data$Onset_day))) %>%
  group_by(TOWNNAME, TOWNENG,MONTH) %>% 
  summarise(total_cases = n())

infected_countries_aggregate <-pts.poly@data %>% 
  dplyr::select(Onset_day, Infected_country, Infected_counties_and_cities, Infected_village)%>% 
  mutate(MONTH = months(as.Date(pts.poly@data$Onset_day))) %>%
  group_by( MONTH, Infected_country, Infected_counties_and_cities, Infected_village) %>% summarise(total_cases = n())


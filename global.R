
library("shiny")
library("sp")
library("sf")
library("tidyverse")
library("GISTools")
library("OpenStreetMap")
library("maps")
library("ggmap")
# library("pryr")

packages = c("DT","jsonlite", "leaflet", "raster",  "shinyjs")

for (p in packages){
  require(p,character.only = T)
}
require("tmap")
require("rgdal")
require("rgeos")
require("openxlsx")
require("spatialEco")
require("lubridate")
require("plotly")
require("stpp")
require("shinyBS")

require("mapproj")
require("maptools")
require("gganimate")
require("gtools")
require("magick")
require("tmaptools")

#https://cran.r-project.org/src/contrib/Archive/rpanel/

# Sys.setlocale("LC_CTYPE", "Chinese")

#for sttp
taiwan <- readOGR(dsn = "data/taiwan_data", layer = "COUNTY_MOI_1070516")
# taiwan <- readShapePoly("data/taiwan_data/COUNTY_MOI_1070516.shp")
taiwan@proj4string<- CRS( "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs")
taiwan.union <- aggregate(taiwan)
#end sttp
taiwan_ts_map_sf = st_read(dsn = "data/TAIWAN_TOWNSHIP", layer = "TOWN_MOI_1071226", stringsAsFactors=TRUE,options = "ENCODING=UTF-8")
county_eng_name <- read.xlsx("data/county_names.xlsx",sheet=1)
taiwan_ts_map_sf = left_join(taiwan_ts_map_sf,county_eng_name[c(1,3)],by=c("COUNTYNAME"="C_NAME"))
taiwan_ts_map_sf$GG_NAME[is.na(taiwan_ts_map_sf$GG_NAME)] <- as.character(taiwan_ts_map_sf$TOWNENG[is.na(taiwan_ts_map_sf$GG_NAME)])
taiwan_ts_map_sf = st_as_sf(taiwan_ts_map_sf,sf_column_name="geometry")

taiwan_ts_map_sp <- as(taiwan_ts_map_sf,"Spatial")


df_dengue.raw <- jsonlite::fromJSON("data/dengue_case.json")
df_dengue.raw$ID <- seq.int(nrow(df_dengue.raw))
sum(is.na(df_dengue.raw$Minimum_statistical_area_center_point_X))
sum(is.na(df_dengue.raw$Minimum_statistical_area_center_point_Y))
# Type conversion
df_dengue.raw[, c(10,11,19,23,24)] <- sapply(df_dengue.raw[, c(10,11,19,23,24)], as.numeric)
# Date conversion
df_dengue.raw[, "Onset_day"] <- as.Date(df_dengue.raw$Onset_day, "%Y/%m/%d")
df_dengue.raw[, "Case_study_date"] <- as.Date(df_dengue.raw$Case_study_date, "%Y/%m/%d")
df_dengue.raw[, "Notification_day"] <- as.Date(df_dengue.raw$Notification_day, "%Y/%m/%d")

df_dengue<- df_dengue.raw[!is.na(df_dengue.raw$Minimum_statistical_area_center_point_X),]
df_dengue<- df_dengue[!(df_dengue$Minimum_statistical_area_center_point_X == 'None'),]
df_dengue2 = df_dengue[,-c(2:9,12:25)]
df_dengue2 = rename(df_dengue, x = Minimum_statistical_area_center_point_X, y = Minimum_statistical_area_center_point_Y)

# Transform into SF object
sf_dengue <- st_as_sf(df_dengue,
                      coords = c("Minimum_statistical_area_center_point_X",
                                 "Minimum_statistical_area_center_point_Y"),
                      crs =  "+init=epsg:3826 +proj=longlat +ellps=WGS84 +no_defs",na.fail=FALSE)

sp_dengue <- as(sf_dengue, 'Spatial')

tmap_mode('view')

projection(taiwan_ts_map_sp) <- sp_dengue@proj4string
taiwan_ts_map_sp@data$poly.ids <- 1:nrow(taiwan_ts_map_sp)

#join poly and points
pts.poly <- point.in.poly(sp_dengue, taiwan_ts_map_sp)

#assign unique id per poly, full info

#descendng order of town with most
#FILTER: Months, Gender
#DISPLAY: COUNTY, GENDER, TOWNID, TOWNNAME, TOWNENG, total_cases, ....
selected_aggregated_temp <-pts.poly@data %>%
  dplyr::select(poly.ids, TOWNNAME, TOWNENG, Onset_day, Case_study_date, Notification_day) %>%
  mutate(MONTH = months(as.Date(pts.poly@data$Onset_day))) %>%
  mutate(YEAR = year(as.Date(pts.poly@data$Onset_day))) %>%
  group_by(TOWNNAME, TOWNENG,MONTH,YEAR) %>%
  summarise(total_cases = n())

infected_countries_aggregate <-pts.poly@data %>%
  dplyr::select(Onset_day, Infected_country, Infected_counties_and_cities, Infected_village)%>%
  mutate(MONTH = months(as.Date(pts.poly@data$Onset_day))) %>%
  mutate(YEAR = year(as.Date(pts.poly@data$Onset_day))) %>%
  group_by( MONTH, YEAR,Infected_country, Infected_counties_and_cities, Infected_village) %>% summarise(total_cases = n())


verbose =c()
printVerbose<-function(x,output){
  verbose = append(verbose,paste("\n",x,sep=""))
  output$my_dump = renderText({
    paste(verbose, collapse=",")
  })
}


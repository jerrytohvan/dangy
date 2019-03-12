packages = c("sp","sf","tidyverse","tmap","jsonlite", "rgdal", "leaflet") 

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p) 
  } 
  library(p,character.only = T) 
}

taiwan_ts_map_sf <- st_read(dsn = "data/TAIWAN_TOWNSHIP", layer = "TOWN_MOI_1071226")
taiwan_ts_map_st <- st_transform(taiwan_ts_map_sf,crs=3826)

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#
# Example R code to install packages
# See http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages for details
#

###########################################################
# Update this line with the R packages to install:

my_packages = c("devtools","colorspace","xlsx","rsconnect","rjson","DT","sp","sf","tidyverse","tmap","jsonlite","geojsonio", "rgdal", "leaflet","shiny","ggplot2","dplyr", "raster","spatialEco","GISTools", "plotly", "scales", "shinyjs", "shinyBS", "OpenStreetMap",'tmaptools', 'magick', 'purrr',"stpp","lubridate","maps","ggmap","gganimate","gtools")

###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    if(p == "gganimate"){
      install.packages("./gganimate-0.1.1.zip", repos = NULL, type="source")
    }else{
      install.packages(p, dependencies = TRUE)
      }
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))



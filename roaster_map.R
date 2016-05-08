#!/usr/bin/env Rscript

### -------------   Leaflet of Coffee Exchange Map   --------------- ###
# function: roaster_map
# @params: path (default: NULL)
# @params: city (default: NULL)

roaster_map <- function(path = NULL, city = NULL){
  # fatal errors
	if(is.null(path)){
	  stop("specify path")
	}
  # required packages
  packages_needed <- c("rleafmap", "sp", "dplyr", "readr", "stringr")
  install_packages <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
  if(length(install_packages) > 0){
    install.packages(install_packages)
  }
  for(p in packages_needed){
    library(p, character.only = TRUE)
  }

	# base layer map
	#stamen.bm <- basemap("stamen.toner")
	bm <- basemap(paste('https://a.tiles.mapbox.com/v3/',
		'eleanor.ipncow29/{z}/{x}/{y}.png', sep = ""))
  
	if(is.null(city)){
	  # Coffee Roasters in US
	  roasters <- read_csv(path) %>% 
	    as.data.frame()
	  roasters <- rename(roasters, lon = long) 
	} else{
	  # Coffee Roasters in region specified
	  roasters <- read_csv(path) %>% 
	    filter(str_detect(City_State_ZIP, city)) %>% 
	    as.data.frame()
	  roasters <- rename(roasters, lon = long)
	}

	# Creating Spatial points of Roasters
	coordinates(roasters) <- ~lon+lat
	roasters_map <- spLayer(roasters, stroke = FALSE, fill.col = 'red', 
	  popup = roasters$Name)

	# writing leaflet
	writeMap(bm, roasters_map, width = 1100, height = 800,
		setView = c(median(roasters_map$lat, na.rm = TRUE), median(roasters_map$long, na.rm = TRUE)), 
		setZoom = 4, directView = "browser")

}

roaster_map(path = '~/Dropbox/Coffee Maps/Coffee_Roaster_Directory_US.txt')

#!/usr/bin/env Rscript

### -------------   Leaflet of Coffee Exchange Map   --------------- ###
# function: roaster_map
# @params: path 

roaster_map <- function(path = NULL){
  # fatal errors
	if(is.null(path)){
	  stop("specify path")
	}
  # required packages
  packages_needed <- c("rleafmap", "sp", "dplyr")
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

	# Coffee Roasters in US
	roasters <- read.csv(path, header = TRUE, stringsAsFactors = FALSE, sep = ",")
	roasters <- rename(roasters, lon = long)

	# Creating Spatial points of Roasters
	coordinates(roasters) <- ~lon+lat
	roasters_map <- spLayer(roasters, stroke = FALSE, fill.col = 'red', 
	  popup = roasters$Name)

	# writing leaflet
	writeMap(bm, roasters_map, width = 1000, height = 700,
		setView = c(39.5, -98.35), setZoom = 4, directView = "browser")

}

roaster_map(path = '~/Dropbox/Coffee Maps/Coffee_Roaster_Directory_US.txt')

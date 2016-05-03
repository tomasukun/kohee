#!/usr/bin/env Rscript

### -------------   Leaflet of Coffee Exchange Map   --------------- ###
# Author: Thomas U. Aguilar
# Date: 10-21-2015
# function: roaster_map
# @params: path 
#	(e.g. "C:/Users/taguilar/Dropbox/Coffee Maps/Coffee_Roaster_Directory_US.txt")

roaster_map <- function(path = NULL){
	if(is.null(path)){
	  stop("specify path to 'Coffee_Roaster_Directory_US.txt'")
	}
	if(!require(rleafmap)) {
		install.packages("rleafmap",
		 repos = 'http://cran.us.r-project.org', dependencies = TRUE)
		suppressPackageStartupMessages(require(rleafmap))
	} else{
		suppressPackageStartupMessages(require(rleafmap))
	}
	if(!require(sp)) {
		install.packages("sp",
		 repos = 'http://cran.us.r-project.org', dependencies = TRUE)
		suppressPackageStartupMessages(require(sp))
	} else{
		suppressPackageStartupMessages(require(sp))
	}
	if(!require(dplyr)) {
		install.packages("dplyr",
		 repos = 'http://cran.us.r-project.org')
		suppressPackageStartupMessages(require(dplyr))
	} else{
		suppressPackageStartupMessages(require(dplyr))
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

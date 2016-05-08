#!/usr/bin/env Rscript

## ----------  Scraping Yelp Search Results  ----------- ##
# Program: yelp_scrape
# @params: 1. url
# @params: 2. out_path
# e.g. yelp_scrape(search_term = "coffee roaster", location = "Oakland, CA", pagestart = 10,
#     out_path = '~/Dropbox/Coffee Maps/CoffeeRoasters_Oakland_Yelp.csv')


yelp_scrape <- function(url = 'http://www.yelp.com/search?find_desc=', 
  search_term = NULL, location = NULL, out_path = NULL, pagestart = 0, ...){

  # - Warnings - #
  if(is.null(search_term)){
    warning('No search terms were used. All businesses included.')
  }
  if(is.null(location)){
    warning('"location" was not specified. Will default to IP address location.')
  }
  # required packages
  packages_needed <- c("rvest", "stringr", "dplyr", "ggmap", "readr")
  install_packages <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
  if(length(install_packages) > 0){
    install.packages(install_packages)
  }
  for(p in packages_needed){
    library(p, character.only = TRUE)
  }
  
	# Allowing script arguments (Allowing script to run in terminal)
	if(is.null(search_term)) {
	  args <- commandArgs(TRUE)
    search_term <- args[1]
  } else{
    search_term <- search_term
  }
  if(is.null(location)) {
    args <- commandArgs(TRUE)
    location <- args[2]
  } else{
    location <- location
  }
  if(is.null(out_path)) {
	  args <- commandArgs(TRUE)
    out_path <- args[3]
  } else{
    out_path <- out_path
  }
	
	# - Parsing HTML page - #
  url_cat <- gsub(",", "%2C", gsub(" ", "+", paste(url, search_term, '&find_loc=', 
    location, '&start=', pagestart, sep = "")))
	url <- read_html(url_cat)
  html_page <- html_nodes(url, paste(".", 'biz-name', sep = ""))
  Name <- html_text(html_page)

  html_page <- html_nodes(url, paste( ".", 'neighborhood-str-list', sep = ""))
  Neighborhood <- html_text(html_page)
  Neighborhood <- str_trim(gsub("\n", "", Neighborhood))

  html_page <- html_nodes(url, paste(".", 'secondary-attributes', sep = ""))
  Address_mess <- html_text(html_page)
  Address_mess <- str_trim(str_replace_all(Address_mess, "\n", ""))
  Address_mess <- gsub("\\([0-9]{3})", "", Address_mess)
  Address_mess <- gsub("Phone number", "", Address_mess)
  phone_patt <- "\\([0-9]{3})([[:blank:]])([0-9]{3})\\-([0-9]{4})"
  Address_mess <- gsub(phone_patt, "", Address_mess)
  phone_patt <- "([0-9]{3})\\-([0-9]{4})"
  Address_mess <- gsub(phone_patt, "", Address_mess)
  
  # final clean address function
  address_clean_f <- Vectorize(function(dirt_addy){
    clean_addy <- dirt_addy %>% 
      str_sub(str_locate(dirt_addy, '\\d')[[1]], nchar(dirt_addy)) %>% 
      str_replace(pattern = location, replacement = str_c(", ", location)) %>% 
      str_trim()
    return(clean_addy)
  }, USE.NAMES = FALSE)
    
  # - Geocoding Addresses - #
  geocodes <- address_clean_f(Address_mess) %>% 
    geocode(output="latlona") %>% 
    mutate(address = as.character(address))
    
  out_file <- geocodes %>% 
    mutate(Name = Name, Address = str_sub(address, 1, (str_locate(address, ',')[,1]-1)), 
           City_State_ZIP = str_sub(address, (str_locate(address, ',')[,1] + 2), str_length(address)), 
           Country = rep('United States', length(Name)), URL = rep('', length(Name)), 
           long = lon, lat = lat) %>% 
    select(Name, Address, City_State_ZIP, Country, URL, long, lat, -address)
    
  # - Outputting Parsed Data - #
  out_file <- data.frame(Name = Name, geocodes)
  out_path_end <- max(str_locate_all(out_path, '\\/')[[1]][,2])
  path_files <- list.files(path = str_sub(out_path, start = 1, (out_path_end-1)), full.names = TRUE)
  if(out_path %in% path_files){
  	write_csv(out_file, out_path, append = TRUE)
  } else{
    write_csv(out_file, out_path, append = FALSE)
  }

} #END



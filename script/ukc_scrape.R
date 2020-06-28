# This code is designed to scrape crag data from UKC 
# 1 - scrape location data
# 2 - scrape crag data
# 3 - iterate over multiple crags

library(tidyverse)
library(glue)
library(rvest)
library(sf)
library(tmap)
library(leaflet)

# extracting data from UKC crag finder page
postcode <- "s71hf"
distance <- 50
ukc <- read_html(glue("https://www.ukclimbing.com/logbook/map/?g=0&loc={postcode}&dist={distance}&km=1&q=&rock=0&dir=0&day=0&rain=0#main"))

top_x_crags <- function(postcode, distance){
  
  # remove spaces from postcodes
  postcode <- gsub(" ","", postcode)
  
  # address of the UKC page
  ukc <- read_html(glue("https://www.ukclimbing.com/logbook/map/?g=0&loc={postcode}&dist={distance}&km=1&q=&rock=0&dir=0&day=0&rain=0#main"))  
  
  # extract coordinates from each crag
  crag_xy <- ukc %>% 
    # pull x and y html element
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel", " " ))]') %>% 
    html_attr("onmouseover") %>%
    # extract coordinates
    regmatches(., gregexpr("(?<=\\().*?(?=\\))", ., perl=T)) %>% 
    # split them into X and Y
    str_split(pattern = ",")
  
  # convert list to a data frame
  crag_xy <- data.frame(matrix(unlist(crag_xy), nrow = length(crag_xy), byrow = TRUE)) %>% 
    rename(lat = X1, lon = X2)
  
  # extract crag ID
  crag_id<- ukc %>% 
    # extract part which has crag id
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel", " " ))]') %>% 
    html_attr("onclick") %>% 
    # extract crag ID
    regmatches(.,gregexpr("(?<=id=).*(?=',)", ., perl = T))
  
  # extract crage name
  crag_name <- ukc %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel-heading", " " ))]') %>% 
    html_text() %>% 
    str_trim()
  
  # extract type of place - we only want crag
  crag_type <-ukc %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "float-right", " " ))]') %>% html_attr("title")
  
  # combine all into one crag data frame
  crags <- bind_cols(name = crag_name, type = crag_type, id = unlist(crag_id), crag_xy)
  
  # only keep crags
  crags <- crags %>% filter(type == "Crag")
  return(crags)
}

crags <- top_x_crags("bn3 3w p", 50)
crags %>% mutate(link = str_to_lower(str_replace_all(name," ","_")))

# convert to sf
crags_sf <- st_as_sf(crags, coords = c("lon","lat"), crs = 4326)

crags_sf %>% filter(type == "Crag") %>% qtm()


crag_access <- ukc %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel-heading", " " ))]') %>% as.character()
restricted <- grep("Restricted Access", crag_access)
banned <- grep("Access Banned", crag_access)





a<-do.call(cbind,crag_access) %>% data.frame()
names(a) <- "xml"

crag_access %>% filter(str_detect("Restricted Access"))

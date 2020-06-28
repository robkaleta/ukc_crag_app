# function to scrape some crag data from UKC. This collects results on 1st page of UKC crag search for a specified postcode and distance

top_x_crags <- function(postcode, distance){
  
  #### remove spaces from postcodes ####
  postcode <- gsub(" ","", postcode)
  
  #### address of the UKC page ####
  ukc <- read_html(glue("https://www.ukclimbing.com/logbook/map/?g=0&loc={postcode}&dist={distance}&km=1&q=&rock=0&dir=0&day=0&rain=0#main"))  
  
  #### extract coordinates from each crag ####
  crag_xy <- ukc %>% 
    # pull x and y html element
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel", " " ))]') %>% 
    html_attr("onmouseover") %>%
    # extract coordinates
    regmatches(., gregexpr("(?<=\\().*?(?=\\))", ., perl=T)) %>% 
    # split them into X and Y
    str_split(pattern = ",")
  
  #### convert list to a data frame ####
  crag_xy <- data.frame(matrix(unlist(crag_xy), nrow = length(crag_xy), byrow = TRUE)) %>% 
    rename(lat = X1, lon = X2)
  
  #### extract crag ID ####
  crag_id<- ukc %>% 
    # extract part which has crag id
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel", " " ))]') %>% 
    html_attr("onclick") %>% 
    # extract crag ID
    regmatches(.,gregexpr("(?<=id=).*(?=',)", ., perl = T))
  
  #### extract crage name ####
  crag_name <- ukc %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel-heading", " " ))]') %>% 
    html_text() %>% 
    str_trim()
  
  #### extract type of place - we only want crag ####
  crag_type <-ukc %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "float-right", " " ))]') %>% html_attr("title")
  
  #### extract any access issues ####
  crag_access <- ukc %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel-heading", " " ))]') %>% as.character()
  # crags with restricted access
  restricted <- grep("Restricted Access", crag_access)
  # crags with access banned
  banned <- grep("Access Banned", crag_access)
  
  # maybe create vector of length == crag_id and populate with NAs and then swap records which are restriced / banned 
  # not the nicest but works - try and make this smoother?
  crag_restrictions <- rep(NA_character_, length(crag_id))
  crag_restrictions[restricted] <- "Restricted Access"
  crag_restrictions[banned] <- "Access Banned"
  
  #### combine all into one crag data frame ####
  crags <- bind_cols(name = crag_name, id = unlist(crag_id), type = crag_type, restrictions = crag_restrictions, crag_xy)
  
  #### only keep type == Crag ####
  crags <- crags %>% filter(type == "Crag")
  return(crags)
}




# check how many pages the results are split over
page_check <- function(postcode, distance){
  #### remove spaces from postcodes ####
  postcode <- gsub(" ","", postcode)
  
  # TO DO - if address fails
  #ukc %>% html_nodes(xpath = '//*[(@id = "results")]//li') %>% html_text
  
  #### address of the UKC page ####
  ukc <- read_html(glue("https://www.ukclimbing.com/logbook/map/?g=0&loc={postcode}&dist={distance}&km=1&q=&rock=0&dir=0&day=0&rain=0&nstart=0#main"))  
  
  # check how many results are there - only 50 can be displayed at a time
  # part of the URL which changes it is - nstart=50
  # kind of messy at the moment...
  n_results <- ukc %>% html_nodes(xpath = '//*[@id="results"]') %>% html_text() %>% 
    regmatches(.,gregexpr("(?<=Search results) .* (?=crags)",., perl = TRUE)) %>% as.integer()
  # if there are more than 50 results the pages get split into multiples of 50 - we need to generate the appropriate number of breaks
  if(n_results > 50){
    # check how many times 50 fits in n_results - round up the result to integer.
    n_pages <- ceiling(n_results/50)
    # -1 from n_pages to get the lower end of the final page range
    n_pages <- n_pages - 1
    
    n_page_start <- seq(from = 0, to = 50*n_pages,by = 50)
    }
  # generate the URLs for each result page
  pages <- glue("https://www.ukclimbing.com/logbook/map/?g=0&loc={postcode}&dist={distance}&km=1&q=&rock=0&dir=0&day=0&rain=0&nstart={n_page_start}#main")
  return(pages)
}

page <- pages[1]
#### function to scrape some crag data from UKC. This collects results on 1st page of UKC crag search for a specified postcode and distance ####
top_x_crags <- function(page){
  #### address of the UKC page ####
  ukc <- read_html(page)  
 
  
  #### extract coordinates from each crag ####
  crag_xy <- ukc %>% 
    # pull x and y html element
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel", " " ))]') %>% 
    html_attr("onmouseover") %>%
    # extract coordinates
    regmatches(., gregexpr("(?<=\\().*?(?=\\))", ., perl=T))
  # not every place has coordinates - set fake coords for now 
  crag_xy[lengths(crag_xy)==0]  <- c("9999,9999")
  
  #split x and y
  crag_xy <- str_split(crag_xy, pattern = ",")
  
  #### convert list to a data frame ####
  crag_xy <- data.frame(matrix(unlist(crag_xy), nrow = length(crag_xy), byrow = TRUE)) %>% 
    rename(lat = X1, lon = X2)
  
  #### extract crag ID ####
  crag_id<- ukc %>% 
    # extract part which has crag id
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel", " " ))]') %>% 
    html_attr("onclick") %>% 
    # extract crag ID
    regmatches(.,gregexpr("(?<=id=).*(?=',)", ., perl = TRUE))
  
  #### extract crage name ####
  crag_name <- ukc %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "panel-heading", " " ))]') %>% 
    html_text() %>% 
    str_trim()
  
  #### extract type of place - we only want crag ####
  crag_type <- ukc %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "float-right", " " ))]') %>% html_attr("title")
  # crags with unkown type are always at the end of the search results
  # a quick workaround the empty types is to add NA which make the length same as length of crag_name
  if(length(crag_type) != length(crag_name)){
    crag_type[(length(crag_type)+1):length(crag_name)] <- NA_character_
  }
  
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


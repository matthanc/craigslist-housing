library(tidyverse)
library(rvest)

#oakland <- read_html("https://sfbay.craigslist.org/search/apa?search_distance=15&postal=94706&min_price=1500&max_price=3000&min_bedrooms=2&availabilityMode=0&sale_date=all+dates")
#oaklandnodes <- html_nodes(oakland, ".hdrlnk")
#oaklandnodes <- html_nodes(oakland, "li.result-row")


# Vishal Chandawarkar Medium Post

#location <- "sfbay"
#area <- "eby" #focus on postings in sfc specifically
#hasPic <- 1 #don't want sketchy postings without pictures
#min_bed <- 1 #guarantees the posts have some data to scrape
#min_bath <- 1 #same as above
#minSqft <- 0 #same as above
#availabilityMode <- 0 
#laundry <- 1 #I really want in-unit laundry, so I included this

# Matthan's query
location <- "sfbay"
searchfield1 <- "-furnished"
searchdistance <- 4
zipcode <- 94703
min_price <- 1750
max_price <- 3200
min_bed <- 2
availabilityMode <- 0
sale_dates <- "all+dates"

#Constructing Matthan's Query
baseurl <- paste0("https://", location, ".craigslist.org/search/apa")
queries <- c("?")
queries <- c(queries, paste0("query=", searchfield1))
queries <- c(queries, paste0("search_distance=", searchdistance))
queries <- c(queries, paste0("postal=", zipcode))
queries <- c(queries, paste0("min_price=", min_price))
queries <- c(queries, paste0("max_price=", max_price))
queries <- c(queries, paste0("min_bedrooms=", min_bed))
queries <- c(queries, paste0("availabilityMode=", availabilityMode))
queries <- c(queries, paste0("sale_date=", sale_dates))
query_url <- paste0(baseurl,queries[1], paste(queries[2:length(queries)], collapse = "&"))


#https://sfbay.craigslist.org/search/apa?search_distance=8&postal=94607&min_price=1500&max_price=3200&min_bedrooms=2&availabilityMode=0&sale_date=all+dates
#https://sfbay.craigslist.org/search/eby/apa?hasPic=1&min_bedrooms=1&min_bathrooms=1&minSqft=0&availabilityMode=0&laundry=1


# Constructing The Query by Concatenating The Features Above
#baseurl <- paste0("https://", location, ".craigslist.org/search/", area, "/apa")
#queries <- c("?")
#queries <- c(queries, paste0("hasPic=", hasPic))
#queries <- c(queries, paste0("min_bedrooms=", min_bed))
#queries <- c(queries, paste0("min_bathrooms=", min_bath))
#queries <- c(queries, paste0("minSqft=", minSqft))
#queries <- c(queries, paste0("availabilityMode=", availabilityMode))
#queries <- c(queries, paste0("laundry=", laundry))
#query_url <- paste0(baseurl,queries[1], paste(queries[2:length(queries)], collapse = "&"))

query_url

# QUERY CRAIGSLIST
raw_query <- xml2::read_html(query_url)
raw_ads <- html_nodes(raw_query, "li.result-row")

# EXTRACT RELEVANT ATTRIBUTES
ids <- raw_ads %>%
  html_attr('data-pid')

titles <- raw_ads %>%
  html_node("a.result-title") %>%
  html_text()

links <- raw_ads %>% 
  html_node(".result-title") %>% 
  html_attr('href')

prices <- raw_ads %>% 
  html_node("span.result-price") %>%
  html_text() %>%
  parse_number()

dates <- raw_ads%>%
  html_node('time') %>%
  html_attr('datetime')

locales <- raw_ads %>%
  html_node(".result-hood") %>%
  html_text()

bedrooms <- raw_ads %>% 
  html_node("span.housing") %>% 
  html_text() %>% 
  parse_number()

sqft <- raw_ads %>% 
  html_node("span.housing") %>% 
  html_text() %>% 
  gsub(".*-\\s([^.]+)[f][t].*","\\1",.) %>% 
  as.numeric()

latlongs <- map_dfr(links, function(x){
  xml2::read_html(x) %>% 
    html_node("#map") %>%
    html_attrs() %>%
    t() %>%
    as_tibble() %>%
    select_at(vars(starts_with("data-"))) %>%
    mutate_all(as.numeric)
}
)

# COMBINE INTO DATA FRAME
craigslist <- data.frame(ids, locales, prices, bedrooms, sqft, dates, titles, latlongs, links) %>% as_tibble()


#Loop for more than 120 results
loopn <- seq(120, 1080, 120)

for(i in loopn){
  Sys.sleep(5) #delays each query by 5 seconds
  queriesloop <- queries
  
  # ADD OFFSET TO URL IN INTERVALS OF 120
  queriesloop <- c(queries, paste0("s=", i))
  query_url <- paste0(baseurl,queriesloop[1], paste(queriesloop[2:length(queriesloop)], collapse = "&"))
  
  # The following loop body is going to be repetitive, but important!
  
  # QUERY CRAIGSLIST
  raw_query <- xml2::read_html(query_url)
  
  raw_ads <- html_nodes(raw_query, "li.result-row")
  
  # EXTRACT ATTRIBUTES
  ids <- raw_ads %>% html_attr('data-pid')
  titles <- raw_ads %>% html_node("a.result-title") %>% html_text()
  links <- raw_ads %>% html_node(".result-title") %>% html_attr('href')
  prices <- raw_ads %>% html_node("span.result-price") %>% html_text() %>% parse_number()
  dates <- raw_ads %>% html_node('time') %>% html_attr('datetime')
  locales <- raw_ads %>% html_node(".result-hood") %>% html_text()
  bedrooms <- raw_ads %>% html_node("span.housing") %>% html_text() %>% parse_number()
  sqft <- raw_ads %>% html_node("span.housing") %>% html_text() %>% gsub(".*-\\s([^.]+)[f][t].*","\\1",.) %>% as.numeric()
  latlongs <- map_dfr(links, function(x){
    xml2::read_html(x) %>% 
      html_node("#map") %>%
      html_attrs() %>%
      t() %>%
      as_tibble() %>%
      select_at(vars(starts_with("data-"))) %>%
      mutate_all(as.numeric)
  }
  )
  
  craigslistloop <- data.frame(ids, locales, prices, bedrooms, sqft, dates, titles, latlongs, links) %>% as_tibble()
  
  # RBIND POSTS IN EACH LOOP TO THE MASTER CRAIGSLIST DATA FRAME
  craigslist <- rbind(craigslist, craigslistloop)
  
}

#Save/download CSV file
write_excel_csv(craigslist,"rentaldata.csv")

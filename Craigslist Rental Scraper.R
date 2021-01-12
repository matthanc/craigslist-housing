pacman::p_load(tidyverse, rvest)


# The guide/post that helped put this together can be found here from vishal Chandawarkar - https://medium.com/swlh/exploring-san-francisco-apartments-on-craigslist-with-r-43e5fa38a77b


# Query items
location <- "sfbay" # searching SFBay CraigsList
searchfield1 <- "-furnished" # many postings were showing up as furnished, this query removes any postings that mention furnished places.
searchdistance <- 4 # limit search to 4 miles around the below zip code
zipcode <- 94703 # zip code / center of search
min_price <- 1750 # min price
max_price <- 3200 # max price
min_bed <- 2 # min beds
availabilityMode <- 0
sale_dates <- "all+dates"

#Constructing  Query by concatenating the search items above
baseurl <- paste0("https://", location, ".craigslist.org/search/apa")
queries <- c("?")
queries <- c(queries, paste0("query=", searchfield1))
queries <- c(queries, paste0("search_distance=", searchdistance))
queries <- c(queries, paste0("postal=", zipcode))
queries <- c(queries, paste0("min_price=", min_price))
queries <- c(queries, paste0("max_price=", max_price))
queries <- c(queries, paste0("min_bedrooms=", min_bed))
queries <- c(queries, paste0("availabilityMode=", availabilityMode))
queries <- c(queries, paste0("laundry=1&laundry=4&laundry=2&laundry=3")) #all but no laundry selections
queries <- c(queries, paste0("parking=1&parking=2&parking=3&parking=4")) #selections for available parking
queries <- c(queries, paste0("sale_date=", sale_dates))
query_url <- paste0(baseurl,queries[1], paste(queries[2:length(queries)], collapse = "&"))

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

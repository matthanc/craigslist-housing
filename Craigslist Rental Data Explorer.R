# https://medium.com/swlh/exploring-san-francisco-apartments-on-craigslist-with-r-43e5fa38a77b

pacman::p_load(tidyverse, DataExplorer, ggplot2, data.table, lubridate, plotly, sf, ggmap, maptools, ggthemes, RColorBrewer, kableExtra)

# READ DATA
craigslist <- read_csv("rentaldata.csv")

# UNDERSTAND YOUR DATA STRUCTURE
str(craigslist)

# REVIEW DATA TAIL IN CASE THERE ARE ANY ISSUES
craigslist %>% 
  as.data.table() %>% 
  tail()

# Note: I review the tail as a data table because the output is much nicer in the console
# Also, the tail could reveal more issues with your data than the head

plot_missing(craigslist)


# REMOVE LISTINGS WITHOUT A LOCATION

craigslist <- craigslist %>% 
  filter(!is.na(data.longitude))

# EXTRACT ONLY DISTINCT ROWS (remove duplicates)
craigslist <- craigslist %>% 
  distinct()

# EXTRACT ONLY DISTINCT TITLES (remove duplicates)
craigslist <- craigslist %>% 
  distinct(titles, .keep_all = TRUE)

craigslist %>% arrange(links) %>% select(links) %>% head()

# REMOVE FIRST PART OF THE URL UP UNTIL "/d/"
craigslist <- craigslist %>% 
  mutate(urlextract = sub("https://sfbay.craigslist.org/eby/apa/d/","",links))

#REMOVE ANY CHARACTERS AFTER "/"
craigslist <- craigslist %>% 
  mutate(urlextract = sub("[///].*","",urlextract))

#SELECT ONLY DISTINCT ROWS
craigslist <- craigslist %>% 
  distinct(urlextract, .keep_all = TRUE)

#REMOVE URL EXTRACT VECTOR, WE DON'T NEED IT ANYMORE
craigslist$urlextract <- NULL

#Removing locations too far from eby
craigslist %>% 
  ggplot(aes(x = data.longitude, y = data.latitude)) +
  geom_point() +
  labs(title = "Scatterplot of GPS Coordinates") +
  theme_minimal()

craigslist <- craigslist %>% 
  filter(data.longitude > -122.39 
         & data.longitude < -121.86
         & data.latitude > 37.77 
         & data.latitude < 37.9)

# CLEAN DATE
craigslist$dates <- ymd_hms(craigslist$dates) %>% floor_date(unit="day")

# FEATURE ENGINEER PRICE PER ROOM AS "roomprice"
craigslist <- craigslist %>% 
  mutate(roomprice = prices/bedrooms)

# FEATURE ENGINEER PRICE SQFT ROOM AS "sqftprice"
craigslist <- craigslist %>%
  filter(sqft > 200 | is.na(sqft)) %>%
  mutate(sqftprice = prices/sqft)

# FACTOR BEDROOMS
craigslist$bedrooms <- as.factor(craigslist$bedrooms)

# BOXPLOT OF DISTRIBUTION
craigslist %>% 
  ggplot(aes(x = bedrooms, y = roomprice)) +
  geom_boxplot() +
  theme_minimal()

#REMOVE SUPER HIGH PRICES
craigslist <- craigslist %>% 
  filter(roomprice < 4500)


#REMOVE >4 BEDROOMS
craigslist <- craigslist %>% 
  filter(as.numeric(bedrooms) < 5)

#CLEAN UP LOCALES

# REMOVE PARENTHESES (FIRST AND LAST CHARACTERS)
craigslist$locales <- gsub('^.|.$', '', craigslist$locales)

# MAKE EVERYTHING LOWERCASE
craigslist$locales <- tolower(craigslist$locales)

# REMOVE NA'S
craigslist <- craigslist %>% 
  filter(is.na(locales) == FALSE)

unique(craigslist$locales)

#FACTOR LOCALES
craigslist$locales <- as.factor(craigslist$locales)

#REVIEW NEIGHBORHOODS
craigslist %>% 
  group_by(locales) %>% 
  select(locales) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#DISTRIBUTION OF PER-ROOM PRICING BY LOCALE

craigslist %>% 
  filter(as.numeric(bedrooms) <= 3) %>% 
  ggplot(aes(x = reorder(locales, roomprice, FUN = median), y=prices)) +
  geom_boxplot(alpha = .8) +
  coord_cartesian(ylim = c(1500,3500)) +
  labs(x = "Neighborhood", y = "Price Per Room", title = "Price Per Room By Neighborhood") +
  theme_wsj() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#DISTRIBUTION OF SQFT PRICING BY LOCALE

craigslist %>% 
  filter(!is.na(sqftprice)) %>%
  ggplot(aes(x = reorder(locales, sqftprice, FUN = median), y=prices)) +
  geom_boxplot(alpha = .8) +
  coord_cartesian(ylim = c(1500,3500)) +
  labs(x = "Neighborhood", y = "Price Per SQFT", title = "Price Per SQFT By Neighborhood") +
  theme_wsj() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

craigslist %>% 
  filter(!is.na(sqftprice)) %>%
  ggplot(aes(x = reorder(locales, sqftprice, FUN = median), y=sqftprice)) +
  geom_boxplot(alpha = .8) +
  coord_cartesian(ylim = c(0,5)) +
  labs(x = "Neighborhood", y = "Price Per SQFT", title = "Price Per SQFT By Neighborhood") +
  theme_wsj() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Price per SQFT Average
mean(craigslist$sqftprice, na.rm = TRUE)
median(craigslist$sqftprice, na.rm = TRUE)
max(craigslist$sqftprice, na.rm = TRUE)
min(craigslist$sqftprice, na.rm = TRUE)

min(craigslist$prices, na.rm = TRUE)


# 1 Bedroom By Price

register_google(key = "AIzaSyC_PtmZsYHmcyKjpJm-TfcITpNfz04qKnI")

eby <- c(lon = -122.272743, lat = 37.817110)
map <- get_map(location = eby, zoom = 12, scale = 2, maptype = 'roadmap')
p <- ggmap(map)
p + geom_point(aes(x=data.longitude, y = data.latitude, color = roomprice), 
               data = craigslist %>% 
                 filter(roomprice < 4000 & 
                          roomprice > 1500 &
                          bedrooms > 2), size=5, alpha = .5) + 
  scale_color_gradient(low="green", high = "red")

#Save/download CSV file
write_excel_csv(craigslist,"rentaldata122220.csv")
write_excel_csv(craigslist, paste0("rentaldata"," (",format(Sys.time(), "%b-%d-%Y"),")", ".csv"))

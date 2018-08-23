require(tidyr)
require(maps)
require(geosphere)
require(dplyr)
require(ggplot2)
require(rworldmap)
require(plyr)
require(data.table)
require(ggthemes)
require(httr)
require(dplyr)
require(plotly)

locations <- read.csv("https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv") %>%
  separate(coordinates, c("lon" ,"lat"), ", ") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  filter(iata_code != "") %>%
  select(c(iata_code, name, lat, lon))

pittsburgh_int <- subset(locations, iata_code == "PIT")

# Function to download WPRDC Data
ckan <- function(id) {
  url <- paste0("https://data.wprdc.org/datastore/dump/", id)
  r <- RETRY("GET", url)
  content(r)
}

getIds <- function(phrase) {
  url <- paste0("http://data.wprdc.org/api/action/resource_search?query=name:", phrase)
  r <- RETRY("GET", url)
  raw <- content(r, "text")
  df <- jsonlite::fromJSON(raw)$result$results
  df$id
}

ids <- getIds("pittsburgh international")

for (i in ids) {
  if (i == ids[1]) {
    trips <- ckan(id)
  } else {
    trips <- rbind(trips, ckan(id))
  }
}

colnames(trips)[6:9] <- c("Flights 2018", "Seats 2018", "Flights 2017", "Seats 2017")

trips <- subset(trips, `Destination Code` != "TOTALS")

table <- aggregate(`Seats 2018`~`Destination Code`, data = trips, FUN = sum)
table$`Flights 2018` <- aggregate(`Flights 2018`~`Destination Code`, data = trips, FUN = sum)$`Flights 2018`

locs <- merge(table, locations, by.x = "Destination Code", by.y = "iata_code", all.x = T) %>%
  filter(`Flights 2018` > 0 & name != "Erase Me 19") %>%
  mutate(org.lat = pittsburgh_int$lat,
         org.lon = pittsburgh_int$lon)
  

# Get World map
worldMap <- getMap()
mapworld_df <- fortify(worldMap)
us <- map_data("state")

select_world <- subset(mapworld_df, id %in% c("Canada", "Mexico", "Cuba", "Puerto Rico", "Iceland"))

ggplot() + 
  geom_polygon(data= select_world, aes(long,lat, group=group), color = "black", fill="#608341", alpha = .8) +
  geom_polygon(data= us, aes(long,lat, group=group), color = "black", fill="#608341", alpha = .8) +
  geom_curve(data = locs, aes(x = org.lon, y = org.lat, xend = lon, yend = lat, color = `Flights 2018`),
             curvature = -0.3, arrow = arrow(length = unit(0.015, "npc")), size = .65) +
  # scale_size_continuous(range = c(.1,2)) +
  scale_colour_distiller(palette = "Reds", direction = 1) +
  geom_text(data = locs, aes(x = lon, y = lat, label = `Destination Code`), size = 2.25, colour = "red", hjust = 1, vjust = 1) +
  coord_equal() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_rect(fill = '#DAFFFF'), panel.grid.major = element_line(colour = "black"), panel.grid.minor = element_line(colour = "gray10")) +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Number of flights out of Pitsburgh International Airport by destination", subtitle = "Jan - Apr 2018", x = NULL, y = NULL, caption = "Sources: Flights - WPRDC.org \n Airport Locations - https://github.com/datasets/airport-codes") +
  coord_cartesian()

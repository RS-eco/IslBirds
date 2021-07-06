rm(list=ls()); gc()
library(geodat); library(tidyverse); library(sf)

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

data(marinerealms)
marinerealms$Realm2 <- cut(marinerealms$Realm, breaks=c(1,2,3,9,10,11,29,30,31), right=F,
                           labels=c("Inner Baltic Sea", "Black Sea", "NE and NW Atlantic and\nMediterranean, Arctic and N Pacific",
                                    "Mid-tropical N Pacific Ocean", "South-east Pacific", 
                                    "Mid South Tropical Pacific and Indian Oceans", "North West Pacific", "Southern Ocean"))
marinerealms <- marinerealms %>% group_by(Realm2) %>% summarise()

marineregions <- st_difference(marinerealms, outline)
save(marineregions, file="data/marineregions.rda", compress="xz")

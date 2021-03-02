rm(list=ls()); gc()
library(sf); library(dplyr)

# IHO Sea Areas, version 1
# downloaded from Marineregions.org
world_seas <- st_read("extdata/World_Seas.shp")
head(world_seas)
length(unique(world_seas$NAME))
length(unique(world_seas$MRGID))
world_seas$ID2 <- as.numeric(sub("[a-zA-Z ]", "", world_seas$ID))
length(unique(world_seas$ID2))

#world_seas$Gazetteer_ <- world_seas$MRGID

world_seas$Ocean <- as.factor(world_seas$NAME)
world_seas$Gazetteer2 <- as.character(substr(world_seas$Gazetteer_, 1, 2))
world_seas$Gazetteer3 <- as.character(substr(world_seas$Gazetteer_, 1, 3))

world_seas$Ocean[world_seas$NAME %in% c("Barents Sea", "Northwestern Passages", "Bering Sea")] <- "Arctic Ocean"
world_seas$Ocean[world_seas$Gazetteer2 %in% c(24, 31, 33)] <- "North Atlantic Ocean"
world_seas$Ocean[world_seas$Gazetteer3 %in% c(424, 425)] <- "Arctic Ocean"
world_seas$Ocean[world_seas$Gazetteer3 %in% c(426, 433)] <- "Indian Ocean"
world_seas$Ocean[world_seas$Gazetteer3 %in% c(237, 238, 428, 429)] <- "North Atlantic Ocean"
world_seas$Ocean[world_seas$Gazetteer3 %in% c(430, 431, 435)] <- "North Pacific Ocean"
world_seas$Ocean[world_seas$Gazetteer3 %in% c(432)] <- "South Atlantic Ocean"
world_seas$Ocean[world_seas$Gazetteer3 %in% c(434, 436)] <- "South Pacific Ocean"
world_seas$Ocean[world_seas$Gazetteer_ %in% c(2353, 2356)] <- "Arctic Ocean"
world_seas$Ocean[world_seas$Gazetteer_ %in% c(2350, 2351, 2357, 2359, 4279)] <- "North Atlantic Ocean"
world_seas$Ocean[world_seas$Gazetteer_ %in% c(4276)] <- "South Pacific Ocean"
world_seas$Ocean[world_seas$Gazetteer_ %in% c(4273,4274, 4275)] <- "Indian Ocean"
world_seas$Ocean[!world_seas$Ocean %in% c("Southern Ocean", "North Atlantic Ocean", "South Atlantic Ocean", 
                                          "South Pacific Ocean", "North Pacific Ocean", "Indian Ocean", 
                                          "Arctic Ocean")] <- NA

library(ggplot2)
ggplot() + geom_sf(data=world_seas, aes(fill=Ocean), colour=NA) + 
  labs(x="", y="") + theme_minimal() + 
  coord_sf(xlim=c(-180,180), ylim=c(-86,90), expand = FALSE, ndiscr=FALSE) + 
  theme(legend.position = "bottom", axis.title.y = element_text(angle=0, hjust=0)) + 
  coord_sf(ndiscr=0)

#world_seas_sub <- world_seas[is.na(world_seas$Ocean),]
#world_seas_sub$Gazetteer2 <- as.character(substr(world_seas_sub$Gazetteer_, 1, 3))
#length(unique(world_seas_sub$Gazetteer_))

#ggplot() + geom_sf(data=world_seas_sub, aes(fill=as.factor(Gazetteer_)), colour=NA) + 
#  labs(x="", y="") + theme_minimal() + 
#  coord_sf(xlim=c(-180,180), ylim=c(-86,90), expand = FALSE, ndiscr=FALSE) + 
#  theme(legend.position = "bottom", axis.title.y = element_text(angle=0, hjust=0)) 

world_seas <- nngeo::st_remove_holes(world_seas)
world_seas <- sf::st_buffer(world_seas, 0)
#world_seas <- as(world_seas, "Spatial")
#world_seas <- rgeos::gPolygonize(rgeos::gNode(as(world_seas, "SpatialLines")))
#world_seas <- rgeos::gUnaryUnion(world_seas, id=world_seas$Ocean)
#world_seas <- sf::st_as_sf(world_seas)

world_seas <- world_seas %>% group_by(Ocean) %>% summarise()
save(world_seas, file="data/world_seas.rda", compress="xz")

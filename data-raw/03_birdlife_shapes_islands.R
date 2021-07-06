#' ---
#' title: "Extract bird shapes for the world's islands"
#' author: "RS-eco"
#' ---

# Clear environment
rm(list=ls()); gc()

# Install and load required libraries
packages <- c("dtplyr", "dplyr", "sf", "raster", "rasterDT", "snowfall")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

# Set working directory
#getwd()
#setwd(sub("Matthias", "", getwd()))
#setwd("/dss/dsshome1/lxc09/ge82nob2/Matthias/")

# Load island data
#remotes::install_github("RS-eco/ggmap2", build_vignettes = TRUE)
library(ggmap2)
data(largeislands, package="ggmap2")
data(smallislands, package="ggmap2")
islands <- bind_rows(largeislands, smallislands)
rm(largeislands, smallislands); gc()

#' We run the st_intersection command step-by-step 
#' for each individual bird species
dir.create("extdata/ind_species_shps_islands", recursive=T)
n <- 10
sfInit(parallel=TRUE, cpus=n)
sfLibrary(sf)
sfExport(list=c("islands"))
sfLapply(1:17463, function(x){
  if(!file.exists(paste0("extdata/ind_species_shps_islands/bird", 
                         x, "_island.shp"))){
    #' Load individual bird shapefile
    #' which was created using ind_species_shapes.R
    botw <- st_read(dsn=paste0("extdata/ind_species_shps/bird", x, ".shp"))
    
    #' Extract intersecting shapes
    island_bird_shp <- st_intersection(islands, botw)
    
    #' Save to file
    st_write(island_bird_shp, 
             paste0("extdata/ind_species_shps_islands/bird", x, "_island.shp"))
  } 
})
sfStop()

# Plot island bird shapes
lapply(1:17463, function(x){
  botw_isl <- sf::st_read(paste0("extdata/ind_species_shps_islands/bird", x, "_island.shp"))
  library(ggplot2)
  ggplot(botw_isl) + geom_sf(aes(fill=Island))
})

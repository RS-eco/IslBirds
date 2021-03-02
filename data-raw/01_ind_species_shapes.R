#' ---
#' title: "Extract individual bird species shapefiles"
#' author: "RS-eco"
#' ---

# Clear environment
rm(list=ls()); gc()

# Install and load required libraries
packages <- c("dtplyr", "dplyr", "sf", "ggplot2")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

# Set working directory
#getwd()
#setwd(sub("Matthias", "", getwd()))
#setwd("/dss/dsshome1/lxc09/ge82nob2/Matthias/")

# Create R-object of birdlife geodatabase
if(!file.exists("extdata/botw.rda")){
  #' List all feature classes in the geodatabase
  sf::st_layers("extdata/BOTW_15022019/BOTW.gdb")
  
  #' Read shapefile with one feature class into R
  botw <- sf::st_read(dsn="extdata/BOTW_15022019/BOTW.gdb", layer="All_Species", type=3)
  save(botw, file="extdata/botw.rda", compress="xz")
  rm(botw); gc()
}

#' Running this takes some time and needs a considerable amount of memory (+10 GB)

#' You can also read only a certain query of the .gdb database
#' This might take up less memory, but takes even longer to load

#botw <- st_read(dsn="extdata/BOTW_15022019/BOTW.gdb", layer="All_Species",
#                query = paste0("SELECT == * FROM \"All_Species\" WHERE SCINAME = ", "'", x, "'"))

load("extdata/botw.rda")

#' Extract data.frame of BOTW.gdb
if(!file.exists("data/botw_df.rda")){
  botw_df <- botw %>% as.data.frame() %>% select(-Shape)
  save(botw_df, file="data/botw_df.rda", compress="xz")
  rm(botw_df); gc()
}

#' Alternatively, we can run the above command step-by-step for each individual bird species
dir.create("extdata/botw_ind_species_shps", recursive=T)
lapply(1:17463, function(x){
  if(!file.exists(paste0("extdata/botw_ind_species_shps/bird", x, ".shp"))){
    #' Subset data
    botw_sub <- botw[x,]
    
    #' Save individual shapefile
    sf::st_write(botw_sub, paste0("extdata/botw_ind_species_shps/bird", x, ".shp"))
  }
})

# Plot Output
load("data/botw_df.rda")
file_numbers <- which(botw_df$SCINAME == "Haliaeetus albicilla")
lapply(file_numbers, function(x){
  botw_sp <- sf::st_read(paste0("extdata/botw_ind_species_shps/bird", x, ".shp"))
  ggplot(botw_sp) + geom_sf()
})

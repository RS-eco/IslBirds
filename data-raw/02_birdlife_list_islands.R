#' ---
#' title: "Create bird species list for the world's islands"
#' author: "RS-eco"
#' ---

# Clear environment
rm(list=ls()); gc()

# Install and load required libraries
packages <- c("dtplyr", "dplyr", "sf", "raster", "rasterDT")
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

#' Extract data.frame of BOTW.gdb
if(!file.exists("data/islands_df.rda")){
  islands_df <- islands %>% as.data.frame() %>% select(-geometry)
  save(islands_df, file="data/islands_df.rda", compress="xz")
  rm(islands_df); gc()
}

#' Identify missing files
dir.create("extdata/botw_isl_ind_species_lists", recursive=T)
file_missing <- sapply(1:17463, function(x){
  if(!file.exists(paste0("extdata/botw_isl_ind_species_lists/bird", x, "_island_df.rds"))){
    return(x)
  }  else{
    return(NULL)
  } 
})
file_missing <- Filter(Negate(is.null), file_missing)
file_missing <- unlist(file_missing)
length(file_missing); head(file_missing)

# Run individual bird shapes in parallel (5 times in individual sessions)
(n <- 5)
file_missing <- split(file_missing, sort(1:length(file_missing)%%n))


r <- raster::raster(nrow=21600, ncol=43200, xmn=-180, xmx=180, ymn=-90, ymx=90,
                    crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
r <- raster::crop(r, islands, snap="out")
r_isl <- rasterDT::fasterizeDT(islands, r, field="ulm_ID")
r <- raster::crop(r, r_isl, snap="out")
no_cells <- rasterDT::freqDT(r_isl)
colnames(no_cells) <- c("ulm_id", "no_cells")
no_cells <- no_cells %>% tidyr::drop_na()
lapply(file_missing[[1]], function(y){
  #' Load individual bird shapefile
  #' which was created using 01_ind_species_shapes.R
  botw <- sf::st_read(dsn=paste0("extdata/botw_ind_species_shps/bird", y, ".shp"))
  id <- unique(botw$SISID)
  r_sub <- raster::crop(r, botw, snap="out")
  botw <- rasterDT::fasterizeDT(botw, r); gc()
  botw <- rasterDT::zonalDT(botw, r_isl, "sum")
  colnames(botw) <- c("ulm_id", "presence")
  botw <- botw %>% mutate(presence = presence/id) %>% 
    filter(presence != 0) %>% left_join(no_cells) %>% group_by(ulm_id) %>%
    mutate(perc_cover=presence/no_cells*100) %>% dplyr::select(ulm_id, perc_cover)
  saveRDS(botw, paste0("extdata/botw_isl_ind_species_lists/bird", y, "_island_df.rds"), 
          compress="xz")
  rm(botw, r_sub); gc()
})

#' Turn individual species lists into one data.frame

if(!file.exists("data/islands_birds.rda")){
  # List files in correct order 1, 2, 3, ...
  files <- list.files("extdata/botw_isl_ind_species_lists", full.names=T)
  
  # Turn individual files into one data.frame
  island_birds <- lapply(1:length(files), function(x){
    dat <- readRDS(files[x])
    dat$id <- as.numeric(sub("_island_df.rds", "", 
                             sub("extdata/botw_isl_ind_species_lists/bird", "", files[x])))
    return(dat)
  }) %>% dplyr::bind_rows() 
  
  # Add individual identifier of bird species
  botw_df <- load("data/botw_df.rda") %>% 
    dplyr::select(SCINAME, PRESENCE, ORIGIN, SEASONAL)
  botw_df$id <- 1:nrow(botw_df)
  island_birds <- left_join(island_birds, botw_df)
  
  #' and save to file
  save(island_birds, file="data/island_birds.rda", compress="xz")
}

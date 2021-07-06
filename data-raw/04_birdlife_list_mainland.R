#' ---
#' title: "Create bird species list for the mainland"
#' author: "RS-eco"
#' ---

# Load libraries
rm(list=ls()); gc()
packages <- c("dtplyr", "dplyr", "sf", "raster", "tidyr", "rasterDT", "snowfall")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)

# Load required packages
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

# Set working directory
#getwd()
#setwd(sub("Matthias", "", getwd()))
#setwd("/dss/dsshome1/lxc09/ge82nob2/Matthias/")

# Set file directory
#filedir <- "/dss/dsshome1/lxc09/ge82nob2/"
filedir <- getwd()
setwd(filedir)

#' Identify missing files
dir.create(paste0(filedir, "extdata/botw_mainland_ind_species_lists"), recursive=T)
file_missing <- sapply(1:17463, function(x){
  if(!file.exists(paste0(filedir, "extdata/botw_mainland_ind_species_lists/bird", x, "_mainland_df.rds"))){
    return(x)
  }  else{
    return(NULL)
  } 
})
file_missing <- Filter(Negate(is.null), file_missing)
file_missing <- unlist(file_missing)
length(file_missing); head(file_missing)

# Run individual bird shapes in parallel (5 times in individual sessions)
#(n <- 5)
#file_missing <- split(file_missing, sort(1:length(file_missing)%%n))

load(file = "data/GADM_mainlands_tiles_simple_100m.rda")
mainland_sf <- st_as_sf(mainland); rm(mainland)
mainland_sf$ml <- 1
r <- raster::raster(nrow=21600, ncol=43200, xmn=-180, xmx=180, ymn=-90, ymx=90,
                    crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
r <- raster::crop(r, mainland_sf, snap="out")
r_mainland <- rasterDT::fasterizeDT(mainland_sf, r, field="ml")
r <- crop(r, r_mainland, snap="out")
no_cells <- rasterDT::freqDT(r_mainland)
colnames(no_cells) <- c("ml", "no_cells")
no_cells <- no_cells %>% tidyr::drop_na()
lapply(file_missing, function(y){
  #' Load individual bird shapefile
  #' which was created using ind_species_shapes.R
  botw <- st_read(dsn=paste0("extdata/botw_ind_species_shps/bird", y, ".shp"))
  id <- unique(botw$SISID)
  botw <- rasterDT::fasterizeDT(botw, raster=r); gc()
  botw <- rasterDT::zonalDT(botw, r_mainland, "sum")
  colnames(botw) <- c("ml", "presence")
  botw <- botw %>% mutate(presence = presence/id) %>% 
    filter(presence != 0) %>% left_join(no_cells) %>% group_by(ml) %>%
    mutate(perc_cover=presence/no_cells*100) %>% dplyr::select(ml, perc_cover)
  saveRDS(botw, paste0("extdata/botw_mainland_ind_species_lists/bird", y, "_mainland_df.rds"), 
          compress="xz")
  rm(botw, r_sub); gc()
})
gc()

files <- list.files(paste0("extdata/botw_mainland_ind_species_lists"), full.names=T)

# Turn individual files into one data.frame
mainland_birds <- lapply(1:length(files), function(x){
  dat <- readRDS(files[x])
  dat$id <- as.numeric(sub("_mainland_df.rds", "", 
                           sub(paste0("extdata/botw_mainland_ind_species_lists/bird"), "", files[x])))
  return(dat)
}) %>% dplyr::bind_rows() 

# Add individual identifier of bird species
botw_df <- load("data/botw_df.rda") %>% 
  dplyr::select(SCINAME, PRESENCE, ORIGIN, SEASONAL)
botw_df$id <- 1:nrow(botw_df)
mainland_birds <- left_join(mainland_birds, botw_df)

#' and save to CSV file
save(mainland_birds, file="data/mainland_birds.rda", compress="xz")

# Check number of mainland species
load("data/mainland_birds.rda")
length(unique(mainland_birds$SCINAME))

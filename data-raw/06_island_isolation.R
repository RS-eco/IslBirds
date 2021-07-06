#' ---
#' title: "Coast to coast distance calculation"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

# Load libraries
packages <- c("dtplyr", "dplyr", "sf", "raster", "tidyr", "rasterDT", "snowfall", "ggplot2", "geosphere", "rgeos")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)

# Load required packages
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

# Global coastline shapefile
load(file = "data/GADM_mainlands_tiles_simple_100m.rda")
crs(mainland) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
mainland <- rgeos::gUnionCascaded(mainland)
plot(mainland)

mainland.coords <- suppressMessages(fortify(mainland))[,c(1:2)]
mainland_sf <- st_as_sf(mainland); rm(mainland)
plot(mainland_sf)
mainland_sf <- st_wrap_dateline(mainland_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
plot(mainland_sf)

# Load island data
#remotes::install_github("RS-eco/ggmap2", build_vignettes = TRUE)
data(largeislands, package="ggmap2")
data(smallislands, package="ggmap2")
islands <- bind_rows(largeislands, smallislands)
islands <- st_wrap_dateline(islands, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
rm(largeislands, smallislands); gc()

# Add islands >= 100000 km2 to mainland
islands_1ht <- islands %>% filter(Area >= 100000)
islands_1ht <- st_wrap_dateline(islands_1ht, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
plot(st_geometry(islands_1ht))

mainland_1ht <- bind_rows(mainland_sf, islands_1ht) %>% st_combine()
mainland_1ht <- st_wrap_dateline(mainland_1ht, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
mainland_1ht.coords <- suppressMessages(fortify(as(mainland_1ht, "Spatial")))[,c(1:2)]
plot(st_geometry(mainland_1ht))

mainland_isl <- bind_rows(mainland_sf, islands)
mainland_isl <- st_wrap_dateline(mainland_isl, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)

dir.create("extdata/island_dist")
files <- sub(".rds", "", list.files("extdata/island_dist"))
head(files)

'%!in%' <- function(x,y)!('%in%'(x,y))
islands <- islands %>% filter(Area >= 1)
#islands <- islands %>% filter(ulm_ID %!in% files)
gc()

# Calculate distances for all islands
#(n <- 8)
#sfInit(parallel=TRUE, cpus=n)
#sfLibrary(sf); sfLibrary(geosphere); sfLibrary(ggplot2); sfLibrary(dplyr)
#sfExport(list=c("islands", "islands_1ht", "mainland_1ht", "mainland_1ht.coords", "mainland_isl", 
#                "mainland_sf", "mainland.coords"))
#print(n)

#isl_miss <- which(islands$ulm_ID %!in% files)
#isl_miss <- split(isl_miss, sort(1:length(isl_miss)%%n))
#isl_miss <- lapply(isl_miss, function(x) rev(x))

# Antipodes island 
#i <- which(islands$ulm_ID == "4946")

# Some islands still have NA values for d_main_1ht
load("data/dist_islands_df.rda")
mis <- dist_islands_df[is.na(dist_islands_df$d_main_1ht),]
isl_miss <- which(islands$ulm_ID %in% mis$ulm_ID)

lapply(isl_miss, function(i){
  print(i)
  islands[i,]
  dist_orig <- round(islands[i,]$Dist, 3)
  
  # Calculate centroid distance
  cen <- centroid(as(islands[i,], "Spatial"))
  dist_centroid <- round(min(suppressWarnings(distGeo(cen, st_coordinates(mainland_sf)[,c(1:2)])))/1000, 3)
  dist_centroid_rough <- round(min(suppressWarnings(distGeo(cen,as.matrix(mainland.coords))))/1000, 3)
  
  # Distance based on points
  dist_coast <- round(min(suppressWarnings(distGeo(st_coordinates(islands[i,])[,c(1:2)],
                                                   st_coordinates(mainland_sf)[,c(1:2)])))/1000, 3)
  
  # Rough distance based on points
  island.coords <- suppressMessages(fortify(as(islands[i,], "Spatial")))[,c(1:2)]
  dist_coast_rough <- round(min(suppressWarnings(distGeo(as.matrix(island.coords),as.matrix(mainland.coords))))/1000, 3)
  
  # Minimum distance to mainland using individual projection
  entity.proj <- st_transform(islands[i,], st_crs(paste("+proj=aeqd +lat_0=",cen[2]," +lon_0=", cen[1],sep="")))
  entity.pnt.proj <- st_transform(st_sfc(st_point(cen), crs=4326), 
                                  st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
  entity.pnt.buff.proj <- st_buffer(entity.pnt.proj, dist = (dist_centroid*1000)+200000)
  entity.pnt.buff <- st_transform(entity.pnt.buff.proj, st_crs(mainland_sf))
  
  # Visual check in WGS84
  plot(entity.pnt.buff)
  plot(mainland_sf, col="red", add=T)
  plot(st_geometry(islands[i,]), add=T, col="blue", pch=10)
  plot(st_geometry(islands_1ht), add=T, col="green")
  
  # Visual check in Azimuthal equidistant
  plot(entity.pnt.buff.proj)
  mainland_sf.proj <- st_transform(mainland_sf, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
  plot(mainland_sf.proj, add=T, col="red")
  plot(entity.pnt.buff.proj, add=T)
  plot(st_geometry(entity.proj), add=T, col="black")
  islands_1ht.proj <- st_transform(islands_1ht, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
  plot(st_geometry(islands_1ht.proj), add=T, col="green")
  
  # Calculate mainland distance
  dist_mainland <- tryCatch({
    if(nrow(st_crop(mainland_sf, entity.pnt.buff))==0){
      mainland.clipped <- st_intersection(mainland_sf, entity.pnt.buff)
    } else{
      mainland.clipped <- st_crop(mainland_sf, entity.pnt.buff)
    }
    mainland.clipped.proj <- st_transform(mainland.clipped, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
    dist <- as.numeric(round(st_distance(entity.proj,mainland.clipped.proj)/1000,3))
    #if(dist > 0){dist} else{NA}
  }, error = function(e){
    mainland.proj <- st_transform(mainland_sf, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
    dist <- as.numeric(round(st_distance(entity.proj,mainland.proj)/1000,3))
    #if(dist > 0){dist} else{NA}
    #st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = TRUE)
    #st_shift_longitude
  }, silent = T)
  
  # Calculate mainland and islands >= 100000 km2 distance
  dist_main_1ht <- tryCatch({
    if(length(st_crop(mainland_1ht, entity.pnt.buff))==0){
      mainland_1ht.clipped <- st_intersection(mainland_1ht, entity.pnt.buff)
    } else{
      mainland_1ht.clipped <- st_crop(mainland_1ht, entity.pnt.buff)
    }
    mainland_1ht.clipped.proj <- st_transform(mainland_1ht.clipped, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
    dist <- as.numeric(round(st_distance(entity.proj,mainland_1ht.clipped.proj)/1000,3))
    #if(dist > 0){dist} else{NA}
  }, error = function(e){
    mainland_1ht.proj <- st_transform(mainland_1ht, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
    #plot(entity.pnt.buff.proj)
    #plot(mainland_1ht.proj, col="green")
    #plot(st_geometry(entity.proj), add=T, col="red")
    #plot(entity.pnt.buff.proj, add=T)
    dist <- as.numeric(round(st_distance(entity.proj,mainland_1ht.proj)/1000,3))
    #if(dist > 0){dist} else{NA}
  }, silent = T)
  
  dist_main_1ht_rough <- round(min(suppressWarnings(distGeo(as.matrix(island.coords),as.matrix(mainland_1ht.coords))))/1000, 3)
  
  # Calculate distance to the nearest larger or equivalent-sized landmass (Db)
  #mainland_bigger <- mainland_isl %>% filter(Area >= islands[i,]$Area) %>% filter(ulm_ID != islands[i,]$ulm_ID) %>%
  #  bind_rows(mainland_sf) %>% st_combine()
  #plot(st_geometry(mainland_bigger))
  #dist_big <- tryCatch({
  #  mainland_bigger.clipped <- st_crop(mainland_bigger, entity.pnt.buff)
  #  mainland_bigger.clipped.proj <- st_transform(mainland_bigger.clipped, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
  #  dist <- as.numeric(round(st_distance(entity.proj,mainland_bigger.clipped.proj)/1000,3))
  #  if(dist > 0){dist} else{NA}
  #}, error = function(e){
  #  mainland_bigger.proj <- st_transform(mainland_bigger, st_crs(paste("+proj=aeqd +lat_0=", cen[2]," +lon_0=", cen[1],sep="")))
  #  dist <- as.numeric(round(st_distance(entity.proj,mainland_bigger.proj)/1000,3))
  #  if(dist > 0){dist} else{NA}
  #}, silent = T)
  
  #mainland_bigger.coords <- suppressMessages(fortify(as(mainland_bigger, "Spatial")))[,c(1:2)]
  #dist_big_rough <- tryCatch({round(min(suppressWarnings(distGeo(as.matrix(island.coords),as.matrix(mainland_bigger.coords))))/1000, 3)
  #}, error = function(e){
  #  dist_big_rough <- NA
  #}, silent=T)
  
  df <- data.frame(ulm_ID=islands$ulm_ID[i], d_orig=dist_orig, 
                   d_centroid=dist_centroid, d_centroid_rough=dist_centroid_rough,
                   d_coast = dist_coast, d_coast_rough = dist_coast_rough,
                   d_main_rough=dist_coast_rough, d_main=dist_mainland,
                   d_main_1ht_rough=dist_main_1ht_rough, d_main_1ht=dist_main_1ht#,
                   #d_bigger_rough=dist_big_rough, d_bigger=dist_big
                   ); gc()
  saveRDS(df, file=paste0("extdata/island_dist/", islands$ulm_ID[i], ".rds"), compress="xz")
  #return(df)
  return(NULL)
}); sfStop()

files <- list.files("extdata/island_dist", full.names=T)
dist_islands_df <- lapply(files, readRDS)
dist_islands_df <- dplyr::bind_rows(dist_islands_df)
head(dist_islands_df)
save(dist_islands_df, file="data/dist_islands_df.rda", compress="xz")
readr::write_csv(dist_islands_df, file="data/dist_islands_df.csv.xz")

####################################

files <- list.files("extdata/island_dist", full.names=T)
dist_islands_new <- lapply(files, readRDS)
dist_islands_new <- dplyr::bind_rows(dist_islands_new)
head(dist_islands_new)

load("data/dist_islands_df.rda")
mis <- dist_islands_df[is.na(dist_islands_df$d_main_1ht),]
dist_islands_orig <- dist_islands_df[!is.na(dist_islands_df$d_main_1ht),]
mis_big <- mis %>% dplyr::select(ulm_ID, d_orig, d_centroid, d_bigger_rough, d_bigger)
dist_islands_new <- inner_join(dist_islands_new, mis_big)
dist_islands_df <- bind_rows(dist_islands_orig, dist_islands_new)
mis2 <- dist_islands_df[is.na(dist_islands_df$d_main_1ht),]
head(dist_islands_df)
save(dist_islands_df, file="data/dist_islands_df.rda", compress="xz")

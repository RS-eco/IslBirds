#' ---
#' title: "Extract eBird Data for the world's islands"
#' author: "RS-eco"
#' ---

# Clear environment
rm(list=ls()); gc()

# Install and load required libraries
packages <- c("sf", "auk", "ggplot2", "snowfall", "dtplyr", "dplyr", "vroom")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

# Set working directory
getwd()
#setwd(sub("Matthias", "", getwd()))
#setwd("/dss/dsshome1/lxc09/ge82nob2/Matthias/")

# Set file directory
filedir <- "D:/Data/eBird/"
#filedir <- "/dss/dsshome1/lxc09/ge82nob2/eBird/"

####################

#' ## Download ebird data

#download.file(url="https://download.ebird.org/ebd/prepackaged/ebd_relJun-2020.tar", 
#              destfile="F:/Data/ebd_relJun-2020.tar")

####################

# Create large islands datafile

# Load island data
#remotes::install_github("RS-eco/ggmap2", build_vignettes = TRUE)
library(ggmap2)
data(largeislands, package="ggmap2")
data(smallislands, package="ggmap2")
islands <- bind_rows(largeislands, smallislands)
rm(largeislands, smallislands); gc()

# Remove very small islands
largeislands <- islands %>% filter(Area >= 1)

####################

#' ## Define data splits
#'

# Define 81 regions
r <- raster::raster()
r <- raster::aggregate(r, fact=c(40, 20), expand=F)
l <-  raster::rasterToPolygons(r)
bbox_isl <- lapply(1:81, function(x) as.numeric(bbox(l[x,])))
#bbox_isl <- list(c(-180, -90, -90, 0), c(-90, -90, 0, 0), c(0, -90, 90, 0), c(90, -90, 180, 0),
#                 c(-180, 0, -135, 45), c(-135, 0, -90, 45), c(-180, 45, -135, 90), c(-135, 45, -90, 90), 
#                 c(-90, 0, -45, 45), c(-90, 45, -45, 90), c(-45, 0, 0, 45), c(-45, 45, 0, 90), 
#                 c(0, 0, 90, 90), c(90, 0, 180, 90))

# Calculate buffered bounding box of islands for each region 
#sapply(1:length(bbox_isl), function(i){
#  largeislands_sub <- sf::st_crop(largeislands, xmin=bbox_isl[[i]][1], ymin=bbox_isl[[i]][2], xmax=bbox_isl[[i]][3],  ymax=bbox_isl[[i]][4])
#  round(sf::st_bbox(sf::st_buffer(largeislands_sub, dist=2)),0)
#})

##
# Manually adjusted bbox values for each region
##
#bbox_isl <- list(c(-180, -82, -90, 0), c(-90, -81, -4, 0), c(0, -72, 89, 0), c(90, -80, 180, 0),
#                 c(-180, 0, -153, 30), c(-126, 0, -90, 43), c(-180, 49, -133, 74), c(-135, 44, -90, 84), 
#                 c(-90, 0, -48, 45), c(-90, 45, -45, 85), c(-33, 5, 0, 45), c(-45, 45, 0, 86), 
#                 c(0, 0, 90, 84), c(90, 0, 180, 83))

####################

# Split eBird data into 81 parts
(n <- 10)
sfInit(parallel=TRUE, cpus=n)
sfLibrary(sf); sfLibrary(auk); sfLibrary(dplyr)
sfExport(list=c("bbox_isl", "filedir"))
sfLapply(1:81, function(i){
  if(!file.exists(paste0(filedir, "ebd_split_", i, ".txt"))){
    print(i)
    
    # Read data for one Polygon and save to file
    x <- auk_ebd(paste0(filedir, "ebd_relMay-2020.txt")) %>% 
      auk_bbox(bbox= bbox_isl[[i]]) %>% 
      auk_complete() %>% auk_filter(file=paste0(filedir, "ebd_split_", i, ".txt"),
                                    keep=c("group identifier", "sampling event identifier", "observer id", 
                                           "locality", "scientific name", "observation count", "longitude", "latitude"))
  }; rm(x); invisible(gc())
})
sfStop(); invisible(gc())


####################

dir.create("extdata/ebird", recursive=T)
lapply(1:81, function(i){
  if(!file.exists(paste0(filedir, "/ebd_isl_split_", i, ".csv.xz"))){
    size1000  <- sum(nchar(readLines(con = paste0(filedir, "/ebd_split_", i, ".txt"), n = 1000)))
    sizetotal <- file.size(paste0(filedir, "/ebd_split_", i, ".txt"))
    total_rows <- ceiling(1000*sizetotal/size1000)
    if(total_rows >= 1e6){
      if(total_rows >= 3e8){n <- 200} else{n <- 20}
      n_rows <- ceiling(total_rows/n)
      
      #sfInit(parallel=TRUE, cpus=10)
      #sfLibrary(raster); sfLibrary(sf); sfLibrary(dplyr); sfLibrary(tidyr); sfLibrary(readr); sfLibrary(vroom)
      #sfExport(list=c("bbox_isl", "islands", "filedir", "i", "n_rows")) 
      ebd_on_isl <- lapply(1:n, function(j){
        if(!file.exists(paste0(filedir, "ebd_isl_split_", i, "_split", j, ".csv"))){
          print(j)
          k <- j-1
          if(k == 0){
            ebd <- read.delim(file=paste0(filedir, "/ebd_split_", i, ".txt"), nrows=n_rows)
          } else{
            ebd <- read.delim(file=paste0(filedir, "/ebd_split_", i, ".txt"), skip=k*n_rows, nrows=n_rows)
            colnames(ebd) <- c("SCIENTIFIC.NAME", "OBSERVATION.COUNT", "LOCALITY", "LATITUDE", "LONGITUDE", 
                               "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER", "GROUP.IDENTIFIER")
          }
          ebd_sub <- ebd %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326); invisible(gc())
          
          # Subset islands according to bbox
          islands_sub <- st_crop(largeislands, xmin=bbox_isl[[i]][1], ymin=bbox_isl[[i]][2], 
                                 xmax=bbox_isl[[i]][3],  ymax=bbox_isl[[i]][4])
          
          # identify points in polygon
          in_poly <- st_within(ebd_sub, islands_sub, sparse=F)
          rm(islands_sub); invisible(gc())
          
          # which points fall inside a polygon?
          in_poly <- apply(in_poly, 1, any)
          
          # subset data frame
          ebd_in_poly <- ebd_sub[in_poly,] %>% as.data.frame()
          rm(ebd_sub, in_poly); invisible(gc())
          
          # Write data.frame to file
          readr::write_csv(ebd_in_poly, paste0(filedir, "ebd_isl_split_", i, "_split", j, ".csv"))
        } else{
          ebd_in_poly <- readr::read_csv(paste0(filedir, "ebd_isl_split_", i, "_split", j, ".csv"))
          colnames(ebd_in_poly) <-  c("SCIENTIFIC.NAME", "OBSERVATION.COUNT", "LOCALITY", "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER",
                                      "GROUP.IDENTIFIER", "geometry")
        }
        return(ebd_in_poly)
      })
      #sfStop()
      ebd_on_isl <- data.table::rbindlist(ebd_on_isl)
      readr::write_csv(ebd_on_isl, paste0("extdata/ebird/ebd_isl_split_", i, ".csv.xz")); rm(ebd_on_isl); invisible(gc())
    } else{
      # Read individual data & convert to sf object
      ebd <- vroom::vroom(paste0("extdata/ebird/ebd_split_", i, ".txt"))
      
      ebd <- ebd %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326); invisible(gc())
      
      # Subset islands according to bbox
      islands_sub <- st_crop(largeislands, xmin=bbox_isl[[i]][1], ymin=bbox_isl[[i]][2], xmax=bbox_isl[[i]][3],  ymax=bbox_isl[[i]][4])
      
      #plot(ebd)
      #plot(st_geometry(islands_sub))
      
      # identify points in polygon
      in_poly <- st_within(ebd, islands_sub, sparse=F)
      dim(in_poly)
      
      # which points fall inside a polygon?
      in_poly <- apply(in_poly, 1, any)
      
      # subset data frame
      ebd_in_poly <- ebd[in_poly,] %>% as.data.frame()
      
      # Write data.frame to file
      readr::write_csv(ebd_in_poly, paste0("extdata/ebird/ebd_isl_split_", i, ".csv.xz"))
      rm(ebd, in_poly, ebd_in_poly, islands_sub); invisible(gc()) 
    }
  }
})

ebd_largeislands <- lapply(1:81, function(x) vroom::vroom(paste0("extdata/ebird/ebd_isl_split_", x, ".csv.xz")))
ebd_largeislands <- lapply(1:81, function(x){
  colnames(ebd_largeislands[[x]]) <- c("SCIENTIFIC.NAME", "OBSERVATION.COUNT", "LOCALITY", "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER",
                          "GROUP.IDENTIFIER", "geometry")
  return(ebd_largeislands[[x]])
})
ebd_largeislands <- data.table::rbindlist(ebd_largeislands)
ebd_largeislands$x <- as.numeric(sapply(ebd_largeislands$geometry, function(x) strsplit(sub(")", "", substring(x, 3)), split=", ")[[1]][1]))
ebd_largeislands$y <- as.numeric(sapply(ebd_largeislands$geometry, function(x) strsplit(sub(")", "", substring(x, 3)), split=", ")[[1]][2]))
ebd_largeislands <- ebd_largeislands %>% dplyr::select(-geometry)
head(ebd_largeislands)
tail(ebd_largeislands)
#ebd_largeislands %>% group_by(x,y) %>% tidyr::spread(SCIENTIFIC.NAME, OBSERVATION.COUNT)
save(ebd_largeislands, file="extdata/ebd_largeislands.rda", compress="xz")

# Loop through individual ebird files
lapply(1:8, function(x){
  if(!file.exists(paste0(filedir, "/ebd_ulm_id_", x, ".csv"))){
    size1000  <- sum(nchar(readLines(con = paste0(filedir, "/ebd_isl_split_", x, ".csv"), n = 1000)))
    sizetotal <- file.size(paste0(filedir, "/ebd_isl_split_", x, ".csv"))
    total_rows <- ceiling(1000*sizetotal/size1000)
    n <- 20
    n_rows <- ceiling(total_rows/n)
    
    #read the csv file of ebird
    lapply(1:n, function(j){
      if(!file.exists(paste0(filedir, "ebd_ulm_id_", x, "_split", j, ".csv"))){
        print(j)
        k <- j-1
        if(k == 0){
          ebird <- vroom::vroom(file=paste0(filedir, "/ebd_isl_split_", x, ".csv"), n_max=n_rows)
        } else{
          ebird <- vroom::vroom(file=paste0(filedir, "/ebd_isl_split_", x, ".csv"), 
                                skip=k*n_rows, n_max=n_rows)
          colnames(ebd) <- c("SCIENTIFIC.NAME", "OBSERVATION.COUNT", "LOCALITY", "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER", "GROUP.IDENTIFIER", "geometry")
        }
        if(nrow(ebird) > 0){
          # correct coordinates
          ebird$x <- as.numeric(sapply(ebird$geometry, function(x) strsplit(sub(")", "", substring(x, 3)), split=", ")[[1]][1]))
          ebird$y <- as.numeric(sapply(ebird$geometry, function(x) strsplit(sub(")", "", substring(x, 3)), split=", ")[[1]][2]))
          
          #convert ebird csv file in shapefile
          ebird <- st_as_sf(ebird, coords = c(x = "x", y = "y"), crs = 4326)
          
          #Intersection
          ebird_islands <- st_intersection(ebird, largeislands) %>% dplyr::select(c(SCIENTIFIC.NAME, OBSERVATION.COUNT, LOCALITY, OBSERVER.ID,
                                                                               SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER, ulm_ID, geometry))
          st_write(ebird_islands, paste0(filedir, "/ebd_ulm_id_", x, "_split", j, ".csv"), layer_options = "GEOMETRY=AS_XY")
          rm(ebird, ebird_islands); gc()  
        }
      }
    })
  }
})

# Join split files to one file
x <- 12
dat <- lapply(1:19, function(j){
  st_read(paste0(filedir, "/ebd_ulm_id_", x, "_split", j, ".csv"))
})
dat <- dplyr::bind_rows(dat)
#convert ebird csv file in shapefile
dat <- st_as_sf(dat, coords = c(x = "X", y = "Y"), crs = 4326)

#' and save to CSV file
st_write(dat, dsn=paste0(filedir, "ebd_ulm_id_", x , ".csv"), layer_options = "GEOMETRY=AS_XY")
file.remove(paste0(filedir, "/ebd_ulm_id_", x, "_split", 1:20, ".csv")); gc()
# Join ulm_id split files into one

#' and save to one final csv file
ebd_ulm_id_islands <- vroom::vroom(list.files(filedir, pattern="ebd_ulm_id_", full.names=T))
#load("data/ebd_ulm_id_islands.rda") # 54731643
library(dplyr)
ebd_ulm_id_islands <- ebd_ulm_id_islands %>% filter(OBSERVATION.COUNT > 0) %>%
  select(X,Y, SCIENTIFIC.NAME, ulm_ID); gc() # 54714410
ebd_ulm_id_islands$presence <- 1
ebd_ulm_id_islands <- ebd_ulm_id_islands %>% group_by(X,Y,SCIENTIFIC.NAME, ulm_ID) %>%
  summarise(presence = sum(presence)) 
ebd_ulm_id_islands$presence <- 1
head(ebd_ulm_id_islands)
save(ebd_ulm_id_islands, file="data/ebd_ulm_id_islands.rda", compress = "xz")

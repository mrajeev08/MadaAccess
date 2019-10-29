## Init MPI Backend
## ################
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

## libraries
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(doRNG)
source("R/ttime_functions.R")

## data from malaria atlas (written out to run on cluster)
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
mada_district <- readOGR("data/MadaGIS/district_init.shp")
friction_masked <- raster("output/friction_mada_masked.tif")
friction_unmasked <- raster("output/friction_mada_unmasked.tif")

## Existing points
## Point locations
gps_locs <- read.csv(file = "data/ctar_metadata.csv", stringsAsFactors = FALSE)[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")

## candidate points
csbs <- read.csv("data/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  dplyr::filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, X_COORD = ycoor, Y_COORD = xcoor) -> csbs

point_mat <- rbind(gps_locs, csbs)
point_mat <- as.matrix(cbind(point_mat$Y_COORD, point_mat$X_COORD)) # matrix of long and lat
ttimes_best <- get.travel.times(friction = friction_masked, shapefile = mada_communes, 
                                coords = point_mat, trans_matrix_exists = TRUE, 
                                filename_trans = "output/trans_gc_masked.rds")
writeRaster(ttimes_best, "output/ttimes_best.tif", overwrite = TRUE)

### Then just close it out at the end
closeCluster(cl)
mpi.quit()

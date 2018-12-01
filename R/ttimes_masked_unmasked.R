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
source("R/get.ttimes.R")
source("R/get.catchmat.R")

## data from malaria atlas (written out to run on cluster)
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
mada_district <- readOGR("data/MadaGIS/district_init.shp")
friction_masked <- raster("output/friction_mada_masked.tif")
friction_unmasked <- raster("output/friction_mada_unmasked.tif")

## Point locations
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)
point_mat <- as.matrix(gps_locs@coords)

## get ttimes layer masked and unmasked
ttimes_masked <- get.travel.times(friction = friction_masked, shapefile = mada_communes,
                           coords = point_mat,
                           trans_matrix_exists = FALSE, 
                           filename_trans = "output/trans_gc_masked.rds")
writeRaster(ttimes_masked, "output/ttimes_masked.tif", overwrite = TRUE)

## get ttimes layer masked and unmasked
ttimes_unmasked <- get.travel.times(friction = friction_unmasked, shapefile = mada_communes,
                           coords = point_mat,
                           trans_matrix_exists = FALSE, 
                           filename_trans = "output/trans_gc_unmasked.rds")
writeRaster(ttimes_unmasked, "output/ttimes_unmasked.tif", overwrite = TRUE)

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
Sys.time()
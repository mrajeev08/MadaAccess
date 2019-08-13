####################################################################################################
##' Generate GIS files 
##' Step 2: get baseline travel times
##' Details: Getting travel time estimates for the baseline clinics (n = 31)
##' Author: Malavika Rajeev 
####################################################################################################

##' Init MPI Backend
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

##' Libraries
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(doRNG)

##' Source
source("R/ttime_functions.R")

##' GIS files written out from Malaria Atlas Project
mada_communes <- readOGR("output/shapefiles/communes.shp")
mada_district <- readOGR("output/shapefiles/districts.shp")
friction_masked <- raster("output/friction_mada_masked.tif")
friction_unmasked <- raster("output/friction_mada_unmasked.tif")

##' Locations of CTAR (n = 31)
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[, c(1, 3, 4)] ##' to do: change this so not indexing columns!
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)
point_mat <- as.matrix(gps_locs@coords)

##' Get masked and unmasked versions of travel time layers
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

check <- setValues(ttimes_unmasked, values(ttimes_masked) - values(ttimes_unmasked))
plot(check)

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
Sys.time()
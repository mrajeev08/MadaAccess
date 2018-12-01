## Init MPI Backend
## ################
rm(list = ls())
# args <- commandArgs(trailingOnly = TRUE)
# cores <- as.integer(args[9])

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
library(foreach)
source("R/get.ttimes.R")
source("R/get.catchmat.R")

## data from malaria atlas (written out to run on cluster)
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
mada_district <- readOGR("data/MadaGIS/district_init.shp")
pop10 <- raster("output/pop10.tif")

## Point locations
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)
point_mat <- as.matrix(gps_locs@coords)
rownames(point_mat) <- gps_locs$CTAR

# ## get ttimes layer
# ttimes <- get.travel.times(friction = friction_mada, shapefile = mada_communes,
#                                  coords = point_mat,
#                                  trans_matrix_exists = TRUE, filename_trans = "output/trans_gc.rds")
# writeRaster(ttimes, "output/ttimes_all.tif", overwrite = TRUE)

## testing parallelization locally
# point_mat <- point_mat[1:2, ]

## getting pops
print(paste(Sys.time(), ": started extracting pop10"))
mada_district <- raster::extract(pop10, mada_district, fun = sum,
                                 na.rm = TRUE, df = TRUE, sp = TRUE)
mada_communes <- raster::extract(pop10, mada_communes, fun = sum,
                                 na.rm = TRUE, df = TRUE, sp = TRUE)
print(paste(Sys.time(), ": finished extracting pop10"))

### getting catchments unmasked
## districts
print(paste(Sys.time(), ": started generating district catchmat unmasked"))

friction_mada <- raster("output/friction_mada_unmasked.tif")

dist_mat <- get.catchmat(point_mat = point_mat, fric = friction_mada, shape = mada_district, 
                         admin = "district", pop_rast = pop10, pop_pol = mada_district$pop10,
                         weighted = TRUE, type = "unmasked")

print(paste(Sys.time(), ": finished generating district catchmat unmasked"))

## communes
print(paste(Sys.time(), ": started generating commune catchmat unmasked"))

comm_mat <- get.catchmat(point_mat = point_mat, fric = friction_mada, shape = mada_communes,
                         admin = "commune", pop_rast = pop10, pop_pol = mada_communes$pop10,
                         weighted = TRUE, type = "unmasked")

print(paste(Sys.time(), ": finished generating commune catchmat unmasked"))

## Masked catchmats
friction_mada <- raster("output/friction_mada_masked.tif")
### getting catchments
## districts
print(paste(Sys.time(), ": started generating district catchmat masked"))

dist_mat <- get.catchmat(point_mat = point_mat, fric = friction_mada, shape = mada_district,
                         admin = "district", pop_rast = pop10, pop_pol = mada_district$pop10,
                         weighted = TRUE, type = "masked")

print(paste(Sys.time(), ": finished generating district catchmat masked"))

## communes
print(paste(Sys.time(), ": started generating commune catchmat masked"))

comm_mat <- get.catchmat(point_mat = point_mat, fric = friction_mada, shape = mada_communes,
                         admin = "commune", pop_rast = pop10, pop_pol = mada_communes$pop10,
                         weighted = TRUE, type = "masked")

print(paste(Sys.time(), ": finished generating commune catchmat masked"))

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
Sys.time()

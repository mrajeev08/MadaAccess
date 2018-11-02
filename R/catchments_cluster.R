## Init MPI Backend
## ################
rm(list = ls())
# library(doMPI)
# cl <- startMPIcluster()
# clusterSize(cl) # this just tells you how many you've got
# registerDoMPI(cl)

## libraries
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(snow)
# library(doRNG)
source("R/travel_times.R")
source("R/get_catchmat.R")

## data from malaria atlas (written out to run on cluster)
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
mada_district <- readOGR("data/MadaGIS/district_init.shp")
friction_mada <- raster("output/friction_mada.tif")
pops_resampled <- raster("output/pop2015_resamp.tif")

## Point locations
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]

names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")

## Keep only point coordinates within the shapefile bounds
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)

# ## Plot to check all good
# p <- autoplot_MAPraster(friction, shp_df = mada_communes)
# points.plot <- as.data.frame(coordinates(gps_locs))
# p[[1]] + geom_point(data = points.plot, aes(Y_COORD, X_COORD))
point_mat <- as.matrix(gps_locs@coords)

beginCluster()

mada_district <- raster::extract(pops_resampled, mada_district, fun = sum,
                                 na.rm = TRUE, df = TRUE, sp = TRUE)

# get ttimes layer
ttimes <- get.travel.times(friction = friction_mada, shapefile = mada_communes,
                                 coords = point_mat,
                                 trans_matrix_exists = TRUE,
                                 filename_trans = "output/trans_gc.rds")


## getting catchments
dist_mat <- get.catchmat(point_mat = point_mat, fric = friction_mada, shape = mada_district, 
                         place_names = mada_district$mdg_dis_co, point_names = gps_locs$CTAR, 
                         admin = "district", pop = pops_resampled, weighted = TRUE)
# 
# comm_mat <- get.catchmat(point_mat = point_mat, fric = friction_mada, shape = mada_communes, 
#                          place_names = mada_communes$mdg_com_co, point_names = gps_locs$CTAR, 
#                          admin = "commune", pop = pops_resampled, weighted = TRUE)

### Then just close it out at the end
endCluster()
# closeCluster(cl)
# mpi.quit()
Sys.time()

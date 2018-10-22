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
library(foreach)
library(doRNG)
source("R/travel_times.R")

## data from malaria atlas (written out to run on cluster)
# mada_communes <- getShp(country = "Madagascar", admin_level = "admin3") # get commune level shapefile
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")

if (file.exists("output/friction_mada.tif")) {
  friction <- raster("output/friction_mada.tif")
} else {
  friction <- malariaAtlas::getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_communes)
}

## Point locations
gps_locs <- read.csv(file = "data/ctar_gps.csv")
names(gps_locs) <- c("IdNo", "CTAR", "District", "X_COORD", "Y_COORD")

## Keep only point coordinates within the shapefile bounds
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)

# ## Plot to check all good
# p <- autoplot_MAPraster(friction, shp_df = mada_communes)
# points.plot <- as.data.frame(coordinates(gps_locs))
# p[[1]] + geom_point(data = points.plot, aes(Y_COORD, X_COORD))
point_mat <- as.matrix(gps_locs@coords)

## get ttimes layer
travel_times <- get.travel.times(friction = friction, shapefile = mada_communes, 
                                 coords = point_mat, 
                                 trans_matrix_exists = TRUE, 
                                 filename_trans = "output/trans_gc.rds")

ttimes <- raster::extract(travel_times, mada_communes, fun = mean, 
                          weights = TRUE, normalizeWeights = TRUE,
                          na.rm=TRUE, df = TRUE, sp = FALSE)

## add pop + baseline travel time to each commune
pop2015adj <- raster::extract(mada_pop2015adj, mada_communes, fun = mean, 
                              weights = TRUE, normalizeWeights = TRUE,
                              na.rm=TRUE, df = TRUE, sp = FALSE)

pop2020adj <- raster::extract(mada_pop2020adj, mada_communes, fun = mean, 
                              weights = TRUE, normalizeWeights = TRUE,
                              na.rm=TRUE, df = TRUE, sp = FALSE)

## getting catchments
catch_mat <- matrix(NA, nrow = nrow(mada_communes), ncol = nrow(point_mat))
rownames(catch_mat) <- mada_communes$mdg_com_co
colnames(catch_mat) <- gps_locs$CTAR

for(i in (1:nrow(point_mat))) {
  print(i)
  print(Sys.time())
  point_mat_sub <- t(as.matrix(point_mat[i, ]))
  travel_time_pt <- get.travel.times(friction = friction, shapefile = mada_communes, 
                                     coords = point_mat_sub, 
                                     trans_matrix_exists = TRUE, 
                                     filename_trans = "output/trans_gc.rds")
  mada.out <- raster::extract(travel_time_pt, mada_communes, fun = mean, 
                              weights = TRUE, normalizeWeights = TRUE,
                              na.rm=TRUE, df = TRUE, sp = FALSE)
  catch_mat[, i] <- mada.out[, 2]
}

write.csv(catch_mat, "output/catch_mat.csv")

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
Sys.time()
  
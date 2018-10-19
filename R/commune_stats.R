## libraries for gis
rm(list = ls())
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(foreach)
library(doMPI)
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

## data from world pop
mada_pop2015adj <- raster("data/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")
mada_pop2020adj <- raster("data/WorldPop/MDG_ppp_2020_adj_v2/MDG_ppp_2020_adj_v2.tif")

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
                                 filename_trans = "trans_gc.rds")

ttimes <- raster::extract(travel_times, mada_communes, fun = mean, 
                          weights = TRUE, normalizeWeights = TRUE,
                          na.rm=TRUE, df = TRUE, sp = FALSE)
  
## getting catchments
catch_mat <- matrix(NA, nrow = nrow(mada_communes), ncol = nrow(point_mat))
rownames(catch_mat) <- mada_communes$name_3
colnames(catch_mat) <- gps_locs$CTAR

## Init MPI Backend
## ################
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

foreach (i = 1:nrow(point_mat)) %dopar% {
  point_mat_sub <- t(as.matrix(point_mat[i, ]))
  travel_time_pt <- get.travel.times(friction = friction, shapefile = mada_communes, 
                                   coords = point_mat_sub, 
                                   trans_matrix_exists = TRUE, 
                                   filename_trans = "output/trans_gc.rds")
  mada.out <- raster::extract(travel_times, mada_communes, fun = mean, 
                              weights = TRUE, normalizeWeights = TRUE,
                              na.rm=TRUE, df = TRUE, sp = FALSE)
  catch_mat[, i] <- mada.out[, 2]
}

# getting min ttimes
inds = apply(catch_mat, 2, function (x) (which(x == min(x), arr.ind=TRUE)))
catchments <- as.data.frame(cbind(rownames(catch_mat), colnames(catch_mat[, inds])))

## add pop + baseline travel time to each commune
pop2015adj <- raster::extract(mada_pop2015adj, mada_communes, fun = mean, 
                          weights = TRUE, normalizeWeights = TRUE,
                          na.rm=TRUE, df = TRUE, sp = FALSE)

pop2020adj <- raster::extract(mada_pop2020adj, mada_communes, fun = mean, 
                           weights = TRUE, normalizeWeights = TRUE,
                           na.rm=TRUE, df = TRUE, sp = FALSE)

## output dataframe with catchments + ttimes + pop!
master <- as.data.frame(cbind(ttimes, catchments[, 2], pop2015adj[, 2], 
                              pop2020adj[, 2]))

names(master) <- c("communeID", "ttime", "catchment", "pop2015adj", "pop2020adj")

write.csv(master, "output/commune_data.csv")

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
  
  
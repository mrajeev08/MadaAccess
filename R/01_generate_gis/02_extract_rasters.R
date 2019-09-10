####################################################################################################
##' Generating GIS files Step 2
##' Details: Getting travel time estimates for the baseline clinics (n = 31)
##'   Recommended that code be run in parallel to limit compute time (particularly the resampling, # 2)
##'   On the Della cluster at Princeton with 38 cores, it takes approximately 20 minutes
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
##' Init MPI Backend
Sys.time()
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
source("R/functions/ttime_functions.R")
source("R/functions/utils.R")

##' Load in GIS files written out from Malaria Atlas Project
mada_communes <- readOGR("data/raw/shapefiles/communes.shp")
mada_districts <- readOGR("data/raw/shapefiles/districts.shp")
friction_masked <- raster("data/raw/friction_mada_masked.tif")
friction_unmasked <- raster("data/raw/friction_mada_unmasked.tif")

##' 1. Get masked and unmaksed versions of travel time layers with baseline ctar (N = 31)
##' Primarily doing this to generate transition matrix to speed up run_catchments.R script
##' ------------------------------------------------------------------------------------------------
##' Locations of CTAR (N = 31)
gps_locs <- read.csv(file = "data/raw/ctar_metadata.csv")[, c(1, 3, 4)] ##' to do: change this so not indexing columns!
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)
point_mat <- as.matrix(gps_locs@coords)
write.csv(point_mat, "data/processed/point_mat_CTAR.csv", row.names = FALSE)

##' Masked ttimes
ttimes_masked <- get.travel.times(friction = friction_masked, shapefile = mada_communes,
                           coords = point_mat,
                           trans_matrix_exists = FALSE, 
                           filename_trans = "data/processed/trans_gc_masked.rds")
writeRaster(ttimes_masked, "data/processed/ttimes_masked.tif", overwrite = TRUE)

##' Unmasked ttimes
ttimes_unmasked <- get.travel.times(friction = friction_unmasked, shapefile = mada_communes,
                           coords = point_mat,
                           trans_matrix_exists = FALSE, 
                           filename_trans = "data/processed/trans_gc_unmasked.rds")
writeRaster(ttimes_unmasked, "data/processed/ttimes_unmasked.tif", overwrite = TRUE)

##' Can check to see what the difference is between masked and unmasked (probably should use masked)
# check <- setValues(ttimes_unmasked, values(ttimes_masked) - values(ttimes_unmasked))
# plot(check)

##' 2. Aggregate World Pop to 1x1 km 
##' ------------------------------------------------------------------------------------------------
##' Load in 2015 adjusted pop estimates from World Pop (apprx 100m resolution, WGS84 projection)
pop <- raster("data/raw/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")

##' bake.nosave (adapted from POMP) either creates the file or reads it in if it already exists
##' to save cluster resources when running this script (so as not to repeat if not needed)
##' in order to make this step faster we do one district per core
##' resampling to 1x1 km apprx
bake.nosave(file = 'data/processed/dist_pops.rds', {
  foreach(i = 1:nrow(mada_districts),.packages = c('pomp', 'raster', 'rgdal', 'sp'), 
          .errorhandling = 'remove',
          .export = c("mada_districts", "pop", "ttimes_masked")
  ) %dopar% {
    cat(i)
    dist <- mada_districts[i, ]
    ttimes_dist <- crop(ttimes_masked, dist)
    ttimes_dist <- mask(ttimes_masked, dist)
    pop_dist <- crop(pop, dist)
    pop_dist <- mask(pop_dist, dist)
    ttimes_pixels <- as(ttimes_dist, "SpatialPixelsDataFrame")
    pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame")
    resampled <- aggregate(pop_pixels, ttimes_pixels, function(x) sum(x, na.rm = TRUE))
    pop1x1 <- raster(resampled["MDG_ppp_2015_adj_v2"])
  } 
}) -> dist_pops

##' then merge all districts together at the end and write to output folder
pop1x1 <- do.call(raster::merge, dist_pops)
names(pop1x1) <- "pop"
writeRaster(pop1x1, "data/processed/worldpop2015adj_mada_1x1km.tif", overwrite = TRUE)

##' 3. Extract pop1x1 to commune + district shapefiles
##' ------------------------------------------------------------------------------------------------

##' Communes
mada_communes <- extract(pop1x1, mada_communes, fun = sum, 
                                     small = TRUE, na.rm = TRUE, sp = TRUE)
writeOGR(mada_communes, "data/processed/shapefiles", layer = "communes", driver = "ESRI Shapefile", 
         overwrite_layer = TRUE)

##' Districts
mada_districts <- extract(pop1x1, mada_districts, fun = sum, 
                                      small = TRUE, na.rm = TRUE, sp = TRUE)
writeOGR(mada_districts, "data/processed/shapefiles", layer = "districts", driver = "ESRI Shapefile", 
         overwrite_layer = TRUE)

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()
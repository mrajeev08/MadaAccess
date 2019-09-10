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
library(foreach)

##' Source
source("R/functions/ttime_functions.R")
source("R/functions/utils.R")

##' Load in GIS files written out from Malaria Atlas Project
mada_districts <- readOGR("data/raw/shapefiles/districts.shp")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

##' Aggregate World Pop to 1x1 km 
##' ------------------------------------------------------------------------------------------------
##' Load in 2015 adjusted pop estimates from World Pop (apprx 100m resolution, WGS84 projection)
pop <- raster("data/raw/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")

##' in order to make this step faster we do one district per core
##' resampling to 1x1 km apprx
dist_pops <- foreach(i = 1:nrow(mada_districts),.packages = c('pomp', 'raster', 'rgdal', 'sp'), 
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

##' then merge all districts together at the end and write to output folder
pop1x1 <- do.call(raster::merge, dist_pops)
names(pop1x1) <- "pop"
writeRaster(pop1x1, "data/processed/rasters/worldpop2015adj_mada_1x1km.tif", overwrite = TRUE)

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()
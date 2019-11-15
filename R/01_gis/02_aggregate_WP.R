####################################################################################################
##' Step 2: Aggregating world pop estimates
##' Details: resampling world pop estimates to the same extent and resolution as the friction surface
##'   Recommended that code be run in parallel to limit compute time
##'   On the Della cluster at Princeton with 38 cores, it takes approximately 6 minutes
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

##' Packages
library(rgdal) # for shapefiles (also comes with sp)
library(raster) # for rasters and resampling
library(foreach) # for parallelizing
source("R/functions/utils.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

##' Aggregate World Pop to 1x1 km 
##' ------------------------------------------------------------------------------------------------
##' Load in 2015 adjusted pop estimates from World Pop (apprx 100m resolution, WGS84 projection)
pop <- raster("data/raw/WorldPop/MDG_ppp_2015_adj_v2.tif")

##' in order to make this step faster we do one district per core
##' resampling to 1x1 km apprx (same resolution as friction surface)
dist_pops <- foreach(i = 1:nrow(mada_districts),.packages = c('raster', 'rgdal', 'sp'), 
          .errorhandling = 'remove',
          .export = c("mada_districts", "pop", "friction_masked")
  ) %dopar% {
    cat(i)
    dist <- mada_districts[i, ] # filter to current district
    friction_dist <- crop(friction_masked, dist) # crop to extent of district
    friction_dist <- mask(friction_masked, dist) # mask so that NA values outside of district
    pop_dist <- crop(pop, dist) # do the same for the pop raster
    pop_dist <- mask(pop_dist, dist)
    friction_pixels <- as(friction_dist, "SpatialPixelsDataFrame") # transform to spatial pixels
    pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame") 
    resampled <- aggregate(pop_pixels, friction_pixels, function(x) sum(x, na.rm = TRUE)) # resample
    pop1x1 <- raster(resampled["MDG_ppp_2015_adj_v2"]) # transform back to raster
  }

##' then merge all districts together at the end and write to output folder
pop1x1 <- do.call(raster::merge, dist_pops)
names(pop1x1) <- "pop"
writeRaster(pop1x1, "data/processed/rasters/worldpop2015adj_mada_1x1km.tif", overwrite = TRUE)

##' quick check sum of population should be ~ 23e6
sum(getValues(pop1x1), na.rm = TRUE)

##' Saving session info
out.session(path = "R/01_gis/02_aggregate_WP.R", filename = "sessionInfo.csv")

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

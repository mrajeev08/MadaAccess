## Init MPI Backend
## ################
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

## libraries
library(foreach)
library(doRNG)
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(doRNG)
library(sp)
source("R/utils.R")

## Data
ttimes <- raster("output/ttimes_masked.tif")
mada_dist <- readOGR("data/MadaGIS/district_init.shp")
pop <- raster("data/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")

## POMP way
bake.nosave(file = 'output/dist_pops.rds', {
  foreach(i = 1:nrow(mada_dist),.packages = c('pomp', 'raster', 'rgdal', 'sp'), .errorhandling = 'remove',
          .export = c("mada_dist", "pop", "ttimes")
  ) %dopar% {
    cat(i)
    dist <- mada_dist[i, ]
    ttimes_dist <- crop(ttimes, dist)
    ttimes_dist <- mask(ttimes_dist, dist)
    pop_dist <- crop(pop, dist)
    pop_dist <- mask(pop_dist, dist)
    ttimes_pixels <- as(ttimes_dist, "SpatialPixelsDataFrame")
    pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame")
    resampled <- aggregate(pop_pixels, ttimes_pixels, function(x) sum(x, na.rm = TRUE))
    pop10 <- raster(resampled["MDG_ppp_2015_adj_v2"])
  }
}) -> dist_pops

pop10 <- do.call(raster::merge, dist_pops)

names(pop10) <- "pop"
writeRaster(pop10, "output/pop10.tif", overwrite = TRUE)

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
Sys.time()

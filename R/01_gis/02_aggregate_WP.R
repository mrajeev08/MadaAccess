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
# rm(list = ls())
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

##' Load in pop estimates
##' ------------------------------------------------------------------------------------------------
##' Load in 2015 adjusted pop estimates from World Pop (apprx 100m resolution, WGS84 projection)
pop_2015 <- raster("data/raw/WorldPop/mdg_ppp_2015.tif")
pop_2018 <- raster("data/raw/WorldPop/mdg_ppp_2018.tif")
pop_2020 <- raster("data/raw/WorldPop/mdg_ppp_2020.tif")
pop_fb <- raster("data/raw/population_mdg_2018-10-01-2/population_mdg_2018-10-01.tif")

##' Convert all to spatial pixels

##' Run resampling function 

##' in order to make this step faster we do one district per core
##' resampling to 1x1 km apprx (same resolution as friction surface)
dist_pops <- foreach(i = 1:nrow(mada_districts),.packages = c('raster', 'rgdal', 'sp'), 
          .errorhandling = 'remove',
          .export = c("mada_districts", "pop_2015", "friction_masked")
  ) %dopar% {
    cat(i)
    dist <- mada_districts[i, ] # filter to current district
    friction_dist <- crop(friction_masked, dist) # crop to extent of district
    friction_dist <- mask(friction_masked, dist) # mask so that NA values outside of district
    pop_dist <- crop(pop_2015, dist) # do the same for the pop raster
    pop_dist <- mask(pop_dist, dist)
    friction_pixels <- as(friction_dist, "SpatialPixelsDataFrame") # transform to spatial pixels
    pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame") 
    resampled <- aggregate(pop_pixels, friction_pixels, function(x) sum(x, na.rm = TRUE)) # resample
    names(resampled) <- "pop"
    pop1x1 <- raster(resampled["pop"]) # transform back to raster
  }

##' then merge all districts together at the end and write to output folder
pop1x1_2015 <- do.call(raster::merge, dist_pops)
names(pop1x1_2015) <- "pop"

## Then get raster pixels that don't fall in
friction_pixs <- rasterize(mada_districts, friction_masked, field = "distcode")
pop_pixs <- rasterize(mada_districts, pop_2015, field = "distcode")

friction_pixels <- as(friction_masked, "SpatialPixelsDataFrame") # transform to spatial pixels
friction_pixels$distcode <- over(friction_pixels, mada_districts)$distcode

pop_pixels <- as(pop_2015, "SpatialPixelsDataFrame") 
pop_pixels$distcode <- over(pop_pixels, mada_districts)$distcode
friction_pixels$cell_id <- 1:length(friction_pixels)
system.time(resampled <- aggregate(pop_pixels, friction_pixels, function(x) sum(x, na.rm = TRUE)) # resample
)

missing <- over(pop_pixels, friction_pixels)
missing_dt <- data.table(cell_id = 1:nrow(missing), 
                              fric_val = missing[, 1], 
                              fric_id = missing[, 2],
                              pop_val = pop_pixels$mdg_ppp_2015)
missing_dt <- missing_dt[!is.na(pop_val) & is.na(fric_id)]

writeRaster(pop1x1_2015, "data/processed/rasters/worldpop2015_1x1km.tif", overwrite = TRUE)
##' quick check sum of population should be ~ 23e6
sum(getValues(pop1x1_2015), na.rm = TRUE)

##' World Pop 2018
dist_pops <- foreach(i = 1:nrow(mada_districts),.packages = c('raster', 'rgdal', 'sp'), 
                     .errorhandling = 'remove',
                     .export = c("mada_districts", "pop_2018", "friction_masked")
) %dopar% {
  cat(i)
  dist <- mada_districts[i, ] # filter to current district
  friction_dist <- crop(friction_masked, dist) # crop to extent of district
  friction_dist <- mask(friction_masked, dist) # mask so that NA values outside of district
  pop_dist <- crop(pop_2018, dist) # do the same for the pop raster
  pop_dist <- mask(pop_dist, dist)
  friction_pixels <- as(friction_dist, "SpatialPixelsDataFrame") # transform to spatial pixels
  pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame") 
  resampled <- aggregate(pop_pixels, friction_pixels, function(x) sum(x, na.rm = TRUE)) # resample
  names(resampled) <- "pop"
  pop1x1 <- raster(resampled["pop"]) # transform back to raster
}

##' then merge all districts together at the end and write to output folder
pop1x1_2018 <- do.call(raster::merge, dist_pops)
names(pop1x1_2018) <- "pop"
writeRaster(pop1x1_2018, "data/processed/rasters/worldpop2018_1x1km.tif", overwrite = TRUE)

##' quick check sum of population should be ~ 23e6
sum(getValues(pop1x1_2018), na.rm = TRUE)


##' World Pop 2020
dist_pops <- foreach(i = 1:nrow(mada_districts),.packages = c('raster', 'rgdal', 'sp'), 
                     .errorhandling = 'remove',
                     .export = c("mada_districts", "pop_2020", "friction_masked")
) %dopar% {
  cat(i)
  dist <- mada_districts[i, ] # filter to current district
  friction_dist <- crop(friction_masked, dist) # crop to extent of district
  friction_dist <- mask(friction_masked, dist) # mask so that NA values outside of district
  pop_dist <- crop(pop_2020, dist) # do the same for the pop raster
  pop_dist <- mask(pop_dist, dist)
  friction_pixels <- as(friction_dist, "SpatialPixelsDataFrame") # transform to spatial pixels
  pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame") 
  resampled <- aggregate(pop_pixels, friction_pixels, function(x) sum(x, na.rm = TRUE)) # resample
  names(resampled) <- "pop"
  pop1x1 <- raster(resampled["pop"]) # transform back to raster
}

##' then merge all districts together at the end and write to output folder
pop1x1_2020 <- do.call(raster::merge, dist_pops)
names(pop1x1_2020) <- "pop"
writeRaster(pop1x1_2020, "data/processed/rasters/worldpop2020_1x1km.tif", overwrite = TRUE)

##' quick check sum of population should be ~ 23e6
sum(getValues(pop1x1_2020), na.rm = TRUE)

##' Aggregate World Pop to 1x1 km: facebook!
##' ------------------------------------------------------------------------------------------------

##' in order to make this step faster we do one district per core
##' resampling to 1x1 km apprx (same resolution as friction surface)
dist_pops <- foreach(i = 1:nrow(mada_districts),.packages = c('raster', 'rgdal', 'sp'), 
                     .errorhandling = 'remove',
                     .export = c("mada_districts", "pop_fb", "friction_masked")
) %dopar% {
  cat(i)
  dist <- mada_districts[i, ] # filter to current district
  friction_dist <- crop(friction_masked, dist) # crop to extent of district
  friction_dist <- mask(friction_masked, dist) # mask so that NA values outside of district
  pop_dist <- crop(pop_fb, dist) # do the same for the pop raster
  pop_dist <- mask(pop_dist, dist)
  friction_pixels <- as(friction_dist, "SpatialPixelsDataFrame") # transform to spatial pixels
  pop_pixels <- as(pop_dist, "SpatialPixelsDataFrame") 
  resampled <- aggregate(pop_pixels, friction_pixels, function(x) sum(x, na.rm = TRUE)) # resample
  names(resampled) <- "pop"
  pop1x1 <- raster(resampled["pop"]) # transform back to raster
}

##' then merge all districts together at the end and write to output folder
pop1x1_fb <- do.call(raster::merge, dist_pops)
names(pop1x1_fb) <- "pop"
writeRaster(pop1x1_fb, "data/processed/rasters/fb2018_1x1km.tif", overwrite = TRUE)

##' quick check sum of population should be ~ 23e6
sum(getValues(pop1x1_fb), na.rm = TRUE)

##' Saving session info
out.session(path = "R/01_gis/02_aggregate_WP.R", filename = "sessionInfo.csv")

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

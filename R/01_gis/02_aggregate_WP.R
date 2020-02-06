####################################################################################################
##' Aggregating world pop estimates
##' Details: resampling world pop estimates to the same extent and resolution as the friction surface
##'   Recommended that code be run in parallel to limit compute time
##'   On the Della cluster at Princeton with 38 cores, it takes approximately 6 minutes
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
Sys.time()

## Packages
library(rgdal) # for shapefiles (also comes with sp)
library(raster) # for rasters and resampling
library(data.table)
source("R/functions/utils.R")
source("R/functions/aggregate_pop.R")

## Load in frition surface and then convert to spatial pixels
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
friction_pixels <- as(friction_masked, "SpatialPixelsDataFrame") # transform to spatial pixels
friction_pixels$cell_id <- 1:length(friction_pixels)

##' Aggregate pop to friction surface
##' ------------------------------------------------------------------------------------------------
## Facebook 2018
fb_2018 <- raster("data/raw/population_mdg_2018-10-01-2/fb2018_aggregated.tif")
pop_fb <- as(fb_2018, "SpatialPixelsDataFrame") 
names(pop_fb) <- "pop"
pop_fb$cell_id <- over(pop_fb, friction_pixels)$cell_id
check <- data.table(pop = pop_fb$pop, cell_id = pop_fb$cell_id)
check <- check[ , .(pop = sum(pop, na.rm = TRUE)), by = "cell_id"]
check <- check[!is.na(cell_id)]
friction_pixels$pop <- check$pop[match(friction_pixels$cell_id, check$cell_id)]
fb_2018_1x1 <- raster(friction_pixels["pop"]) # transform back to raster
writeRaster(fb_2018_1x1, "data/processed/rasters/fb_2018_1x1.tif", overwrite = TRUE)

sum(getValues(fb_2018), na.rm = TRUE)
sum(getValues(fb_2018_1x1), na.rm = TRUE)

## WP 2015
wp_2015 <- raster("data/raw/WorldPop/mdg_ppp_2015.tif")
pop_2015 <- as(wp_2015, "SpatialPixelsDataFrame") 
names(pop_2015) <- "pop"
pop_2015$cell_id <- over(pop_2015, friction_pixels)$cell_id
check <- data.table(pop = pop_2015$pop, cell_id = pop_2015$cell_id)
check <- check[ , .(pop = sum(pop, na.rm = TRUE)), by = "cell_id"]
check <- check[!is.na(cell_id)]
friction_pixels$pop <- check$pop[match(friction_pixels$cell_id, check$cell_id)]
wp_2015_1x1 <- raster(friction_pixels["pop"]) # transform back to raster
writeRaster(wp_2015_1x1, "data/processed/rasters/wp_2015_1x1.tif", overwrite = TRUE)
sum(getValues(wp_2015), na.rm = TRUE)
sum(getValues(wp_2015_1x1), na.rm = TRUE)

## WP 2018
wp_2018 <- raster("data/raw/WorldPop/mdg_ppp_2018.tif")
pop_2018 <- as(wp_2018, "SpatialPixelsDataFrame") 
names(pop_2018) <- "pop"
pop_2018$cell_id <- over(pop_2018, friction_pixels)$cell_id
check <- data.table(pop = pop_2018$pop, cell_id = pop_2018$cell_id)
check <- check[ , .(pop = sum(pop, na.rm = TRUE)), by = "cell_id"]
check <- check[!is.na(cell_id)]
friction_pixels$pop <- check$pop[match(friction_pixels$cell_id, check$cell_id)]
wp_2018_1x1 <- raster(friction_pixels["pop"]) # transform back to raster
writeRaster(wp_2018_1x1, "data/processed/rasters/wp_2018_1x1.tif", overwrite = TRUE)
sum(getValues(wp_2018), na.rm = TRUE)
sum(getValues(wp_2018_1x1), na.rm = TRUE)

## WP 2020
wp_2020 <- raster("data/raw/WorldPop/mdg_ppp_2020.tif")
pop_2020 <- as(wp_2020, "SpatialPixelsDataFrame") 
names(pop_2020) <- "pop"
pop_2020$cell_id <- over(pop_2020, friction_pixels)$cell_id
check <- data.table(pop = pop_2020$pop, cell_id = pop_2020$cell_id)
check <- check[ , .(pop = sum(pop, na.rm = TRUE)), by = "cell_id"]
check <- check[!is.na(cell_id)]
friction_pixels$pop <- check$pop[match(friction_pixels$cell_id, check$cell_id)]
wp_2020_1x1 <- raster(friction_pixels["pop"]) # transform back to raster
writeRaster(wp_2020_1x1, "data/processed/rasters/wp_2020_1x1.tif", overwrite = TRUE)
sum(getValues(wp_2020), na.rm = TRUE)
sum(getValues(wp_2020_1x1), na.rm = TRUE)

## Saving session info
out.session(path = "R/01_gis/02_aggregate_WP.R", filename = "sessionInfo.csv")

## Close out cluster
print("Done :)")
Sys.time()


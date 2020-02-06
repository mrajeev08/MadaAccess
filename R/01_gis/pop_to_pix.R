####################################################################################################
##' Pop rasters to pixels  
##' Details: Matching NA pops to non NA cells; this takes a long time and lots of memory!
##' I ran it on the Princeton Cluster and allocated 25 gigs of RAM; takes abt 3 hours to run;
##' Author: Malavika 
####################################################################################################

## Set-up
library(rgdal) # for shapefiles (also comes with sp)
library(raster) # for rasters and resampling
library(data.table)
source("R/functions/utils.R")
source("R/functions/aggregate_pop.R")

## Load in frition surface and then convert to spatial pixels
friction_masked <- as(raster("data/processed/rasters/friction_mada_masked.tif"),
                      "SpatialPixelsDataFrame") # transform to spatial pixels
friction_masked$cell_id <- 1:length(friction_masked)

## FB 2018
fb_2018 <- raster("data/raw/population_mdg_2018-10-01-2/fb2018_aggregated.tif")
fb_2018_pix <- pop_to_pixels(friction_pixels = friction_masked, pop_raster = fb_2018, nmoves = 50)
sum(getValues(fb_2018), na.rm = TRUE)
sum(fb_2018_pix$pop, na.rm = TRUE)
saveRDS(fb_2018_pix, "data/raw/temp_pop/fb_2018_temp.rds")
rm(fb_2018, fb_2018_pix)
gc()

## WP 2015
wp_2015 <- raster("data/raw/WorldPop/mdg_ppp_2015.tif")
wp_2015_pix <- pop_to_pixels(friction_pixels = friction_masked, pop_raster = wp_2015, nmoves = 50)
sum(getValues(wp_2015), na.rm = TRUE)
sum(wp_2015_pix$pop, na.rm = TRUE)
saveRDS(wp_2015_pix, "data/raw/temp_pop/wp_2015_temp.rds")
rm(wp_2015, wp_2015_pix)
gc()

## WP 2018
wp_2018 <- raster("data/raw/WorldPop/mdg_ppp_2018.tif")
wp_2018_pix <- pop_to_pixels(friction_pixels = friction_masked, pop_raster = wp_2018, nmoves = 50)
sum(getValues(wp_2018), na.rm = TRUE)
sum(wp_2018_pix$pop, na.rm = TRUE)
saveRDS(wp_2018_pix, "data/raw/temp_pop/wp_2018_temp.rds")
rm(wp_2018, wp_2018_pix)
gc()

## WP 2020
wp_2020 <- raster("data/raw/WorldPop/mdg_ppp_2020.tif")
wp_2020_pix <- pop_to_pixels(friction_pixels = friction_masked, pop_raster = wp_2020, nmoves = 50)
sum(getValues(wp_2020), na.rm = TRUE)
sum(wp_2020_pix$pop, na.rm = TRUE)
saveRDS(wp_2020_pix, "data/raw/temp_pop/wp_2020_temp.rds")
rm(wp_2020, wp_2020_pix)
gc()

# ## Out session
# out.session(path = "R/01_gis/02_pop_to_pix.R", filename = "sessionInfo.csv")

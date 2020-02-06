# ------------------------------------------------------------------------------------------------ #
#' Process all raster files
#' Get masked friction surface as input for travel time estimates 
#' Create transition layer for gdistance functions
#' Aggregate up FB 2018 pop estimates to higher resolution
# ------------------------------------------------------------------------------------------------ #

# Packages
rm(list=ls())
library(malariaAtlas) # for friction surface
library(raster) # for reading in rasters
library(rgdal) # for reading in shapefiles
library(gdistance) # for making transition object
source("R/functions/out.session.R")

# Shapefile for masking to (from OCHA)
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")

# Masked friction surface
friction_masked <- getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_districts)
plot(friction_masked) ## test
writeRaster(friction_masked, "data/processed/rasters/friction_mada_masked.tif", overwrite = TRUE)

# Masked transition surface (geocorrected  transition matrix (i.e., the graph))
# NOTE: that this is RAM intensive, can be very slow for large areas!
trans <- transition(friction_masked, function(x) 1/mean(x), 8) 
trans_gc <- geoCorrection(trans)
saveRDS(trans_gc, "data/processed/rasters/trans_gc_masked.rds")

# aggregate up facebook pop data to make it easier to work with (this takes abt 10min!)
pop_fb <- raster("data/raw/population_mdg_2018-10-01-2/population_mdg_2018-10-01.tif")
system.time(pop_fb <- raster::aggregate(pop_fb, fact = 5, fun = sum, na.rm = TRUE))
writeRaster(pop_fb, "data/raw/population_mdg_2018-10-01-2/fb2018_aggregated.tif", overwrite = TRUE)

# Saving session info
out.session(path = "R/01_gis/01_process_rasters.R", filename = "sessionInfo.csv")

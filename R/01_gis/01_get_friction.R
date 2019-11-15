####################################################################################################
##' Step 1: Generating GIS files
##' Details: Getting masked friction surface as input for travel time estimates 
##' Also creating transition layer for gdistance functions
##' Author: Malavika Rajeev 
####################################################################################################

##' Packages
rm(list=ls())
library(malariaAtlas) # for friction surface
library(raster) # for reading in rasters
library(rgdal) # for reading in shapefiles
library(gdistance) # for making transition object
source("R/functions/utils.R")

##' Shapefile for masking to (from OCHA)
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")

##' Masked friction surface
friction_masked <- getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_districts)
plot(friction_masked) ## test
writeRaster(friction_masked, "data/processed/rasters/friction_mada_masked.tif", overwrite = TRUE)

##' Masked transition surface
##' Make and geocorrect the transition matrix (i.e., the graph)
trans <- transition(friction_masked, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
trans_gc <- geoCorrection(trans)
saveRDS(trans_gc, "data/processed/rasters/trans_gc_masked.rds")

##' Saving session info
out.session(path = "R/01_gis/01_get_friction.R", filename = "sessionInfo.csv")

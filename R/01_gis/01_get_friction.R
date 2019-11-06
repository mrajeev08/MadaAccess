####################################################################################################
##' Generating GIS files Step 1
##' Details: Getting masked and unmasked friction layers to input into travel time estimates 
##' Need to do this outside of the cluster because problem with connecting to external server
##' Author: Malavika Rajeev 
####################################################################################################

##' Libraries and packages
rm(list=ls())
library(malariaAtlas) # for friction surface
library(raster)
library(rgdal)
library(gdistance)

##' Shapefile for masking
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

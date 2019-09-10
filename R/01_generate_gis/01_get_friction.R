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

##' Masked friction surface
friction_masked <- getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_communes)
plot(friction_masked) ## test
writeRaster(friction_masked, "data/processed/rasters/friction_mada_masked.tif", overwrite = TRUE)

##' Masked transition surface
##' Make and geocorrect the transition matrix (i.e., the graph)
trans <- transition(friction_masked, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
trans_gc <- geoCorrection(trans)
saveRDS(trans_gc, "data/processed/rasters/trans_gc_masked.rds")

##' Unmasked friction surface (still cropped to Mada)
## takes a long time (~ 15 minutes)
friction_world <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015")
friction_unmasked <- crop(friction_world, mada_communes)
plot(friction_unmasked) ## test
writeRaster(friction_unmasked, "data/processed/rasters/friction_mada_unmasked.tif", overwrite = TRUE)

##' Unmasked transition surface
##' Make and geocorrect the transition matrix (i.e., the graph)
trans <- transition(friction_unmasked, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
trans_gc <- geoCorrection(trans)
saveRDS(trans_gc, "data/processed/rasters/trans_gc_unmasked.rds")
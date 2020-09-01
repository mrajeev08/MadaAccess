# ------------------------------------------------------------------------------------------------ #
#' Process all raster files
#' Get masked friction surface as input for travel time estimates 
#' Create transition layer for gdistance functions
# ------------------------------------------------------------------------------------------------ #

# Packages
library(malariaAtlas) # for friction surface
library(raster) # for reading in rasters
require(sf) # for reading in shapefiles
library(gdistance) # for making transition object
source("R/functions/out.session.R")

# Shapefile for masking to (from OCHA)
mada_districts <- st_read("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")

# Masked friction surface
friction_masked <- getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_districts)
writeRaster(friction_masked, "data/processed/rasters/friction_mada_masked.tif", overwrite = TRUE)

# Masked transition surface (geocorrected  transition matrix (i.e., the graph))
# NOTE: that this is RAM intensive, can be very slow for large areas!
trans <- transition(friction_masked, function(x) 1/mean(x), 8) 
trans_gc <- geoCorrection(trans)
saveRDS(trans_gc, "data/processed/rasters/trans_gc_masked.rds")

# Saving session info
out.session(path = "R/01_gis/01_process_rasters.R", filename = "output/log_local.csv")

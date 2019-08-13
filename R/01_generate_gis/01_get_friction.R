####################################################################################################
##' Generate GIS files 
##' Step 1: get friction surface and shapefiles from Malaria Atlas Project 
##' Details: Getting masked and unmasked friction layers to input into travel time estimates 
##' Author: Malavika Rajeev 
####################################################################################################

##' Libraries and packages
rm(list=ls())
library(malariaAtlas) # for friction surface

##' Shapefiles
mada_communes <- getShp(country = "Madagascar", admin_level = "admin3") ## commune level shapefile
plot(mada_communes)
writeOGR(mada_communes, "output/shapefiles", layer = "communes", driver = "ESRI Shapefile", 
         overwrite_layer = TRUE)
mada_districts <- getShp(country = "Madagascar", admin_level = "admin2") ## district level shapefile
plot(mada_districts)
writeOGR(mada_districts, "output/shapefiles", layer = "districts", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

##' Masked friction surface
friction_masked <- getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_communes)
plot(friction_masked) ## test
writeRaster(friction_masked, "output/friction_mada_masked.tif", overwrite = TRUE)

##' Unmasked friction surface (still cropped to Mada)
## takes a long time (~ 15 minutes)
friction_world <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015")
friction_unmasked <- crop(friction_world, mada_communes)
plot(friction_unmasked) ## test
writeRaster(friction_unmasked, "output/friction_mada_unmasked.tif", overwrite = TRUE)

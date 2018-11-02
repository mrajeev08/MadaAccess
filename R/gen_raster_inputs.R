## ################
rm(list = ls())

## libraries
library(rgdal)
library(raster)
library(malariaAtlas)

## raster layers
ttimes <- raster("output/ttimes_all.tif")
pop <- raster("data/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")
pop10 <- aggregate(pop, fact = 10, fun = sum) # aggregate to same res
pop10_resamp <- resample(pop10, ttimes)
writeRaster(pop10_resamp, "output/pop2015_resamp.tif", overwrite = TRUE)

## getting write friction surface
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
friction <- malariaAtlas::getRaster(
surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015")
friction_mada <- crop(friction, mada_communes)
writeRaster(friction_mada, "output/friction_mada.tif", overwrite = TRUE)

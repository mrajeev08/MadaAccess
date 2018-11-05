## ################
rm(list = ls())

## libraries
library(rgdal)
library(raster)
library(malariaAtlas)

## raster layers
ttimes <- raster("output/ttimes_all.tif")
pop <- raster("data/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")
pop10 <- aggregate(pop, fact = 10, fun = sum, na.rm = TRUE)
ttimes_pol <- rasterToPolygons(ttimes)
# pop10 <- extract(pop, ttimes_pol, fun = sum, na.rm = TRUE)

e <- intersect(extent(ttimes), extent(pop10)) # get minimum extent
ttimes <- raster::crop(ttimes, e)
pop10 <- crop(pop10, e)
pop10 <- shift(pop10, x = (origin(ttimes) - origin(pop10))[1], y = (origin(ttimes) - origin(pop10))[2])
ttimes_weighted <- ttimes*pop10

# ## getting friction surface
# mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
# friction <- malariaAtlas::getRaster(
# surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015")
# friction_mada <- crop(friction, mada_communes)
# writeRaster(friction_mada, "output/friction_mada.tif", overwrite = TRUE)

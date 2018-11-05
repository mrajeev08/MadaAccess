## ################
# rm(list = ls())

## libraries
library(rgdal)
library(raster)
library(malariaAtlas)

## raster layers
ttimes <- raster("output/ttimes_all.tif")
names(ttimes) <- "ttimes"
pop <- raster("data/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")

## extracting to larger res
ttimes_pol <- rasterToPolygons(ttimes)
ttimes_pol <- extract(pop, ttimes_pol, fun = sum, na.rm = TRUE)
pop10 <- rasterize(ttimes_pol, ttimes, field = "ttimes")
ttimes_weighted <- ttimes*pop10
names(pop10) <- "pop"



## Getting district mean travel time to 
# libraries for gis
library(raster)
library(maptools)
library(maps)
library(GISTools)
library(rgdal)
library(sp)
library(rgdal)
library(gdistance)
ttime.layer <- raster("output/study.area.accessibility.tif")
mada.district <- readOGR("data/MadaGIS/MadaPops.shp")

mada.out <- raster::extract(ttime.layer, mada.district, fun = mean, na.rm=TRUE, df = TRUE, sp = TRUE)
writeSpatialShape(mada.out, "data/MadaGIS/MadaPops")

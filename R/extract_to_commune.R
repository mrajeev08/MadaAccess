## Getting district mean travel time to 

## Init MPI Backend
## ################
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

# libraries for gis
library(raster)
library(maptools)
library(rgdal)
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")

## data from world pop
ttimes <- raster("output/ttimes_all.tif")
mada_pop2015adj <- raster("data/WorldPop/MDG_ppp_2015_adj_v2/MDG_ppp_2015_adj_v2.tif")
mada_pop2020adj <- raster("data/WorldPop/MDG_ppp_2020_adj_v2/MDG_ppp_2020_adj_v2.tif")

mada_out <- raster::extract(ttimes, mada_communes, fun = mean, na.rm=TRUE, df = TRUE, sp = TRUE)

mada_out <- raster::extract(mada_pop2015adj, mada_out, fun = sum, na.rm=TRUE, df = TRUE, sp = TRUE)

mada_out <- raster::extract(mada_pop2020adj, mada_out, fun = sum, na.rm=TRUE, df = TRUE, sp = TRUE)

writeSpatialShape(mada_out, "output/communes/communes_extract.shp")

### Then just close it out at the end
closeCluster(cl)
mpi.quit()
Sys.time()
## libraries
library(rgdal)

## files
trans.mat <- readRDS("output/study.area.Trans.GC.rds")
p4s <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mada.commune <- readOGR("data/MadaGIS/commune_mada.shp", p4s = p4s)
ctar.gps <- read.csv("data/ctar_gps.csv")
ctar.gps <- SpatialPoints(cbind(ctar.gps$LONGITUDE, ctar.gps$LATITUDE),
                          proj4string = crs(mada.commune))
ctar.atts <- readOGR("data/MadaGIS/31CTAR.shp")

## get commune centroids
commune.coords <- SpatialPoints(coordinates(mada.commune))

## getting distance between points
ttime.mat <- rSPDistance(trans.mat, commune.coords, ctar.gps, theta = 1)

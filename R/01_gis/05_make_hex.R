####################################################################################################
##' Making hexagonal 
##' Details: ran on cluster to save time on computer
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
Sys.time()
rm(list = ls())

##' Packages
library(rgdal) # for shapefiles (also comes with sp)
library(raster) # for rasters and resampling
library(geogrid)
library(data.table)
source("R/functions/utils.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")

## Getting ctar district and commune
ctar_metadata$ctar_dist <- mada_districts$distcode[match(ctar_metadata$District, 
                                                         mada_districts$district)]
pts <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$ctar_comm <- over(pts, mada_communes)$commcode
mada_districts$ctar_in_dist <- ifelse(mada_districts$distcode %in% ctar_metadata$ctar_dist, 1, 0)
mada_communes$ctar_in_comm <- ifelse(mada_communes$commcode %in% ctar_metadata$ctar_comm, 1, 0)

## Hexagonal maps
## Check the potentials, here seed 1 looks the best
# for (i in 1:6) {
#   new_cells <- calculate_grid(shape = mada_districts, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }
new_cells <- calculate_grid(shape = mada_districts, grid_type = "hexagonal", seed = 1)
district_hex <- assign_polygons(mada_districts, new_cells)
writeOGR(district_hex, dsn = "data/processed/shapefiles", layer = "mada_districts_hex", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

## Commune
## Check the potentials, here seed 3 looks the best
# for (i in 1:6) {
#   new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }
new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = 5)
commune_hex <- assign_polygons(mada_communes, new_cells)
writeOGR(commune_hex, dsn = "data/processed/shapefiles", layer = "mada_communes_hex", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

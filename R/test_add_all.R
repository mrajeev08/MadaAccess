## Init MPI Backend
rm(list = ls())

## libraries
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(doRNG)
source("R/get.ttimes.R")
source("R/get.catchmat.R")

## data from malaria atlas (written out to run on cluster)
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
mada_district <- readOGR("data/MadaGIS/district_init.shp")
friction_masked <- raster("output/friction_mada_masked.tif")
friction_unmasked <- raster("output/friction_mada_unmasked.tif")

## Point locations
csbs <- read.csv("data/csbs.csv")[,c(1, 2, 8)]
names(csbs) <- c("Y_COORD", "X_COORD", "CTAR")
coordinates(csbs) <- ~ Y_COORD + X_COORD
proj4string(csbs) <- proj4string(mada_communes)
point_mat <- as.matrix(csbs@coords)

gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")
coordinates(gps_locs) <- ~ Y_COORD + X_COORD
proj4string(gps_locs) <- proj4string(mada_communes)
point_mat <- as.matrix(gps_locs@coords)


## proportion of pop
pop10 <- raster("output/pop10.tif")
prop_pop <- pop10/sum(values(pop10), na.rm = TRUE)

## get ttimes layer masked and unmasked
system.time({
  ttimes_masked <- get.travel.times(friction = friction_masked, shapefile = mada_communes,
                                    coords = point_mat,
                                    trans_matrix_exists = TRUE, 
                                    filename_trans = "output/trans_gc_masked.rds")
  sum(prop_pop[ttimes_masked >= 60*3], na.rm = TRUE) 
})
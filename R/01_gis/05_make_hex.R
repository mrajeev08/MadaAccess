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
library(geosphere)
library(rgeos)
library(spdep)
library(tidyverse)
library(sf)
source("R/functions/utils.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

test_pol <- mada_communes[mada_communes$distcode == levels(mada_communes$distcode)[1], ]
test_points <- SpatialPoints(coordinates(test_pol), proj4string = crs(test_pol))
check <- test_points[1, ]
cands <- test_points[-1, ]
bears <- bearing(check, cands)
buff <- buffer(check, width = 2500)
pts_within <- cands[buff, ]
bears_within <- bears[!is.na(over(cands, buff))]

get.angle <- function(origin, dest) {
  atan2(sin(dest[, 1] - origin[, 1]), cos(origin[, 2]*sin(dest[, 2]) - 
                                                 sin(origin[, 1])*cos(dest[, 2])*cos(dest[, 1] - origin[, 1])))
}

angle <- get.angle(check@coords, pts_within@coords)

R = 6371e3 # earth's radius in meters
phi_1 # lat in radians
lamda_1 # long in radians
d # distance in meters
brng # bearing in degrees (clockwise from North)

# Trig functions take arguments in radians, so latitude, longitude, and bearings in degrees (either decimal or degrees/minutes/seconds) need to be converted to radians, rad = π*deg/180. When converting radians back to degrees (deg = 180*rad/π), West is negative if using signed decimal degrees.

var φ2 = Math.asin( Math.sin(φ1)*Math.cos(d/R) +
                      Math.cos(φ1)*Math.sin(d/R)*Math.cos(brng) )
var λ2 = λ1 + Math.atan2(Math.sin(brng)*Math.sin(d/R)*Math.cos(φ1)
                         Math.cos(d/R)-Math.sin(φ1)*Math.sin(φ2))

## transform radians back to degrees
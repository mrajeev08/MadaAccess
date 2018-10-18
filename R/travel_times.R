<<<<<<< HEAD
## Function for getting travel times given points 
## Adapted from https://map.ox.ac.uk/research-project/accessibility_to_cities/
## Malavika Rajeev 2018

get.travel.times <- function(friction, shapefile, coords, trans_matrix_exists = TRUE, 
                             filename_raster, filename_trans){
=======
# libraries for gis
library(raster)
library(maptools)
library(maps)
library(GISTools)
library(rgdal)
library(sp)
library(rgdal)
library(gdistance)

get.travel.times <- function(friction, shapefile, coordinates, trans.matrix = 1, 
                             filename.raster, filename.trans){
>>>>>>> origin/master
  
  ## crop friction surface to shapefile
  friction <- crop(friction, shapefile)
  plot(friction)
<<<<<<< HEAD
  points(points.plot)
  
  ## calculating travel times
  ## Fetch the number of points
  n.points <- dim(coords)[1]
  
  ## Make the graph and the geocorrected version of the graph (or read in the latter).
  if (trans_matrix_exists = TRUE) {
    # Read in the transition matrix object if it has been pre-computed
    trans_gc <- readRDS(filename_trans)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    trans <- transition(friction, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
    #saveRDS(Trans, filename.nonGCtrans)
    trans_gc <- geoCorrection(trans)
    saveRDS(trans_gc, filename_trans)
  }
  
  ## Convert the points into a matrix
  xy_df <- data.frame()
  xy_df[1:n.points, 1] <- coordinates[, 1]
  xy_df[1:n.points, 2] <- coordinates[, 2]
  xy_matrix <- as.matrix(xy_df)
  
  ## Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  travel_times <- accCost(trans_gc, xy_matrix)
  
  ## Clip to Mada
  travel_times <- crop(travel_times, shapefile)
  travel_times <- mask(travel_times, shapefile)
  
  ## Write the resulting raster
  return(travel_times)
}
=======
  points(coordinates)
  
  ## calculating travel times
  # Fetch the number of points
  n.points <- dim(ctar.gps)[1]
  
  # Make the graph and the geocorrected version of the graph (or read in the latter).
  if (trans.matrix == 1) {
    # Read in the transition matrix object if it has been pre-computed
    Trans.GC <- readRDS(filename.trans)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    Trans <- transition(friction, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
    #saveRDS(Trans, filename.nonGCtrans)
    Trans.GC <- geoCorrection(Trans)
    saveRDS(Trans.GC, filename.trans)
  }
  
  # Convert the points into a matrix
  xy.data.frame <- data.frame()
  xy.data.frame[1:n.points,1] <- coordinates[,1]
  xy.data.frame[1:n.points,2] <- coordinates[,2]
  xy.matrix <- as.matrix(xy.data.frame)
  
  # Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  travel.times <- accCost(Trans.GC, xy.matrix)
  
  # Write the resulting raster
  writeRaster(mada.pep.raster, filename.raster)
  return(travel.times)
}

friction <- raster("friction_surface_2015_v1.0/friction_surface_2015_v1.0.tif")
# filename.trans
moramanga <- readOGR("data/MoramangaGIS/Moramanga.shp")
mada.pop <- raster("data/MDG_ppp_2015_v2/MDG_ppp_2015_v2.tif")
mada <- readOGR("data/MadaGIS/district_init.shp")
ctar.gps <- read.csv("data/ctar_gps.csv", header = TRUE)
# csbs

## output moramanga pop raster
mora.pop <- mask(mada.pop, moramanga)
mora.pop <- crop(mora.pop, moramanga)

## baseline access

## add pop + baseline travel time to Moramanga and output
moramanga <- extract(mada.pop, moramanga, fun = sum, na.rm=TRUE, df = TRUE, sp = TRUE) # pop


## get district points for calculating expanded access

## travel time if expanded

## get Moramanga CSBs

## travel time if PEP at all CSB II


>>>>>>> origin/master

## libraries for gis
rm(list = ls())
library(raster)
library(malariaAtlas)

## data from malaria atlas
mada_communes <- getShp(country = "Madagascar", admin_level = "admin3") # get commune level shapefile
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = mada_communes)

check <- raster('output/study.area.accessibility.tif')
p <- autoplot_MAPraster(check, shp_df = mada_communes)
p[[1]] + geom_point(data = point.locations, aes(LONGITUDE, LATITUDE))

# ## reproject all
# friction <- projectRaster(friction, crs = p4s)
# mada_communes <- spTransform(mada_communes, crs(friction))

## Point locations
point.locations <- read.csv(file = "data/ctar_gps.csv")[ ,-1]
names(point.locations) <- c("District", "X_COORD", "Y_COORD")

# Keep only point coordinates within the shapefile bounds
coordinates(point.locations) <- ~ Y_COORD + X_COORD
proj4string(point.locations) <- proj4string(mada_communes)
# overlap <- over(point.locations, mada_communes)
# point.locations <- point.locations[!is.na(overlap$gid),]

plot(mada_communes)
plot(friction, add = TRUE)
plot(mada_communes, add = TRUE)
points(point.locations, col = "blue", pch = 20)




get.travel.times <- function(friction, shapefile, coordinates, trans.matrix = 1, 
                             filename.raster, filename.trans){
  
  ## crop friction surface to shapefile
  friction <- crop(friction, shapefile)
  plot(friction)
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



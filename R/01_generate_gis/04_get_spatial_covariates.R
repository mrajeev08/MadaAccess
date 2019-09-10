####################################################################################################
##' Step 4: Assigning catchment to each admin unit based on travel times
##' Details: Uses catchment matrixes generated in step 03 to select the catchment
##'   Does not need to be run in parallel
##' Author: Malavika Rajeev 
####################################################################################################

##' Read in files 
##' ------------------------------------------------------------------------------------------------
##' 
##' Libraries/source scripts 
##' ------------------------------------------------------------------------------------------------
##' 
##' Extract Pop 1x1 km
##' ------------------------------------------------------------------------------------------------
##' 
##' Assign catchments and travel times (weighted/unweighted and masked/unmasked)
##' ------------------------------------------------------------------------------------------------
##' 
##' Get distance to closest CTAR
##' ------------------------------------------------------------------------------------------------
##' 
##' Write out the shapefiles to processed/shapefiles/
##' ------------------------------------------------------------------------------------------------


## Set-up
rm(list = ls())
mada_districts <- readOGR("output/shapefiles/districts.shp")
mada_communes <- readOGR("output/shapefiles/communes.shp")
source("R/functions/utils.R")
source("R/functions/ttime_functions.R")

##' Get CTAR locations with name of CTAR
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")

##' Load in most recent catchmats
catchmat_dist_masked <- find.bydate(path = "output/", patt = "catchmat_district_masked", ind = 4, rank = 1)
catchmat_dist_unmasked <- find.bydate(path = "output/", patt = "catchmat_district_unmasked", ind = 4, rank = 1)
catchmat_comm_masked <- find.bydate(path = "output/", patt = "catchmat_commune_masked", ind = 4, rank = 1)
catchmat_comm_unmasked <- find.bydate(path = "output/", patt = "catchmat_commune_unmasked", ind = 4, rank = 1)




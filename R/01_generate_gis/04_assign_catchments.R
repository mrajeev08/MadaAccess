####################################################################################################
##' Step 4: Assigning catchment to each admin unit based on travel times
##' Details: Uses catchment matrixes generated in step 03 to select the catchment
##'   Does not need to be run in parallel
##' Author: Malavika Rajeev 
####################################################################################################

## Set-up
rm(list = ls())
mada_districts <- readOGR("output/shapefiles/districts.shp")
mada_communes <- readOGR("output/shapefiles/communes.shp")

##' Get CTAR locations with name of CTAR
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")

##' Load in most recent catchmats
find.bydate <- function(path, patt, ind, rank)
catchmat_dist_masked <- find.bydate(pqth = "output/catchmats/", patt = "district_catchmat_masked", ind = 4, rank = 1)
catchmat_dist_unmasked <- find.bydate(pqth = "output/catchmats/", patt = "district_catchmat_unmasked", ind = 4, rank = 1)
catchmat_comm_masked <- find.bydate(pqth = "output/catchmats/", patt = "commune_catchmat_masked", ind = 4, rank = 1)
catchmat_comm_unmasked <- find.bydate(pqth = "output/catchmats/", patt = "commune_catchmat_unmasked", ind = 4, rank = 1)

##' Get catchments
dist_catch_unmasked <- get.catchments(catchmat = catchmat_dist_unmasked, shape = mada_districts, 
                                      place_names = mada_districts$mdg_dis_co, point_names = gps_locs$CTAR,
                                      type = "unmasked", admin = "district")

dist_catch_masked <- get.catchments(catchmat = catchmat_dist_masked, shape = mada_districts, 
                                    place_names = mada_districts$mdg_dis_co, 
                                    point_names = gps_locs$CTAR,
                                    type = "masked", admin = "district")

comm_catch_unmasked <- get.catchments(catchmat = catchmat_comm_unmasked, shape = mada_communes, 
                                      place_names = mada_communes$mdg_com_co,
                                      point_names = gps_locs$CTAR,
                                      type = "unmasked", admin = "commune")

comm_catch_masked <- get.catchments(catchmat = catchmat_comm_masked, shape = mada_communes, 
                                    place_names = mada_communes$mdg_com_co, 
                                    point_names = gps_locs$CTAR,
                                    type = "masked", admin = "commune")

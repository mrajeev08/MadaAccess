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

##' Get catchments
dist_catch_unmasked <- get.catchments(catchmat = catchmat_dist_unmasked, shape = mada_districts, 
                                      place_names = mada_districts$id_2, point_names = gps_locs$CTAR,
                                      type = "unmasked", admin = "district")

dist_catch_masked <- get.catchments(catchmat = catchmat_dist_masked, shape = mada_districts, 
                                    place_names = mada_districts$id_2, 
                                    point_names = gps_locs$CTAR,
                                    type = "masked", admin = "district")

comm_catch_unmasked <- get.catchments(catchmat = catchmat_comm_unmasked, shape = mada_communes, 
                                      place_names = mada_communes$id_3,
                                      point_names = gps_locs$CTAR,
                                      type = "unmasked", admin = "commune")

comm_catch_masked <- get.catchments(catchmat = catchmat_comm_masked, shape = mada_communes, 
                                    place_names = mada_communes$id_3, 
                                    point_names = gps_locs$CTAR,
                                    type = "masked", admin = "commune")

## Checks
# library(dplyr)
# gg_communes <- fortify(mada_communes, region = "id_3")
# gg_communes %>%
#   mutate(id = as.numeric(id)) %>%
#   left_join(mada_communes@data, by = c("id" = "id_3")) %>%
#   mutate(id = as.factor(as.character(id))) %>%
#   left_join(comm_catch_masked, by = c("id" = "admin")) -> gg_communes
# ggplot() +
#   geom_polygon(data = gg_communes, aes(x = long, y = lat, group = id, fill = ttime)) +
#   scale_fill_gradient(low = "white", high = "red")
# 
# gg_communes <- fortify(mada_communes, region = "id_3")
# gg_communes %>%
#   mutate(id = as.numeric(id)) %>%
#   left_join(mada_communes@data, by = c("id" = "id_3")) %>%
#   mutate(id = as.factor(as.character(id))) %>%
#   left_join(comm_catch_unmasked, by = c("id" = "admin")) -> gg_communes
# ggplot() +
#   geom_polygon(data = gg_communes, aes(x = long, y = lat, group = id, fill = ttime)) +
#   scale_fill_gradient(low = "white", high = "red") +
#   geom_point(data = gps_locs, aes(x = Y_COORD, y = X_COORD))


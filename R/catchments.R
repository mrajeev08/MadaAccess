## getting min ttimes
rm(list = ls())
mada_district <- readOGR("data/MadaGIS/district_init.shp")
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")

## Point locations
gps_locs <- read.csv(file = "data/ctar_metadata.csv")[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")
catch_mat_dist_masked <- read.csv("output/district_catchmat_masked_20181201_101312.csv", row.names = 1)
catch_mat_dist_unmasked <- read.csv("output/district_catchmat_unmasked_20181201_100811.csv", row.names = 1)

catch_mat_comm_masked <- read.csv("output/commune_catchmat_masked_20181201_101629.csv", row.names = 1)
catch_mat_comm_unmasked <- read.csv("output/commune_catchmat_unmasked_20181201_101133.csv", row.names = 1)


get.catchments <- function(catch_mat, shape, place_names, point_names, 
                           type = "masked", admin = "district") {
  rownames(catch_mat) <- place_names
  colnames(catch_mat) <- point_names
  
  min <-apply(catch_mat, 1, function (x) (range(x[is.finite(x)])[1]))
  
  inds <-apply(catch_mat, 1, function (x) {
    which(x == range(x[is.finite(x)])[1], arr.ind=TRUE)[1]
  })
  
  catchments <- as.data.frame(cbind(rownames(catch_mat), colnames(catch_mat)[inds], min))
  
  write.csv(catchments, paste0("output/catchments_", admin, "_", type, ".csv"))
  return(catchments)
}


dist_catch_unmasked <- get.catchments(catch_mat = catch_mat_dist_unmasked, shape = mada_district, 
                      place_names = mada_district$mdg_dis_co, point_names = gps_locs$CTAR,
                      type = "unmasked", admin = "district")

dist_catch_masked <- get.catchments(catch_mat = catch_mat_dist_masked, shape = mada_district, 
                                    place_names = mada_district$mdg_dis_co, 
                                    point_names = gps_locs$CTAR,
                                    type = "masked", admin = "district")

comm_catch_unmasked <- get.catchments(catch_mat = catch_mat_comm_unmasked, shape = mada_communes, 
                                      place_names = mada_communes$mdg_com_co,
                                      point_names = gps_locs$CTAR,
                                      type = "unmasked", admin = "commune")

comm_catch_masked <- get.catchments(catch_mat = catch_mat_comm_masked, shape = mada_communes, 
                                    place_names = mada_communes$mdg_com_co, 
                                    point_names = gps_locs$CTAR,
                                    type = "masked", admin = "commune")

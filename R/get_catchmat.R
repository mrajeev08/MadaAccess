### Function for getting catchmat
get.catchmat <- function(point_mat, fric, shape, place_names, point_names, admin = "district",
                         pop, weighted = TRUE){
    ## getting catchments
    catch_mat <- matrix(NA, nrow = nrow(shape), ncol = nrow(point_mat))
    rownames(catch_mat) <- place_names
    colnames(catch_mat) <- point_names
    if (weighted == TRUE) {
      shape <- raster::extract(pop, shape, fun = sum,
                                na.rm = TRUE, df = TRUE, sp = TRUE)
    }
    
    for(i in (1:nrow(point_mat))) {
      print(i)
      print(Sys.time())
      point_mat_sub <- t(as.matrix(point_mat[i, ]))
      travel_time_pt <- get.travel.times(friction = fric, shapefile = shape, 
                                     coords = point_mat_sub, 
                                     trans_matrix_exists = TRUE, 
                                     filename_trans = "output/trans_gc.rds")
      if (weighted == TRUE){
        weighted_ttimes <- travel_time_pt*pop
        out <- raster::extract(weighted_ttimes, shape, fun = sum, 
                                 na.rm = TRUE, df = TRUE, sp = TRUE)
        out$ttimes <- out$weighted_times/out$pop
      } else {
        out <- raster::extract(travel_time_pt, shape, fun = mean, 
                               na.rm = TRUE, df = TRUE, sp = TRUE)
      }
      
      catch_mat[, i] <- out@data[, ncol(out@data)]
    }

    write.csv(catch_mat, paste0("output/", admin, "catch_mat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    return(catch_mat)
}


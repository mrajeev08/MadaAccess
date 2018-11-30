### Function for getting catchment matrix
## Admin level average or weighted average by pop for each clinic
    
get.catchmat <- function(point_mat, fric, shape, place_names, point_names, admin = "district",
                         pop_rast, pop_pol, weighted = TRUE, type = "masked"){
    ## getting catchments
    catch_mat <- matrix(NA, nrow = nrow(shape), ncol = nrow(point_mat))
    rownames(catch_mat) <- place_names
    colnames(catch_mat) <- point_names
    
    foreach(i = 1:nrow(point_mat),.packages = c('raster', 'rgdal', 'sp', 'gdistance'), 
            .errorhandling = 'stop',
            .export = c("point_mat", "fric", "shape", "place_names", "point_names", "admin",
                        "pop_rast", "pop_pol", "weighted", "type", 'get.travel.times')
    ) %dopar% {
      print(i)
      print(Sys.time())
      point_mat_sub <- t(as.matrix(point_mat[i, ]))
      travel_time_pt <- get.travel.times(friction = fric, shapefile = shape, 
                                     coords = point_mat_sub, 
                                     trans_matrix_exists = FALSE, 
                                     filename_trans = "output/trans_gc_test.rds")

      if (weighted == TRUE){
        ## add 1e6 to 0 ttimes
        travel_time_pt[travel_time_pt == 0] <- 1e-6
        weighted_ttimes <- travel_time_pt*pop_rast
        names(weighted_ttimes) <- "w_ttimes"
        out <- raster::extract(weighted_ttimes, shape, fun = sum, 
                                 na.rm = TRUE, df = TRUE, sp = TRUE)
        out$ttimes <- out$w_ttimes/pop_pol
      } else {
        names(travel_time_pt) <- "ttimes"
        out <- raster::extract(travel_time_pt, shape, fun = mean, 
                               na.rm = TRUE, df = TRUE, sp = TRUE)
      }
      catch_mat[, i] <- out$ttimes
    }
    
    write.csv(catch_mat, paste0("output/", admin, "_", "catchmat_", type, "_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    return(catch_mat)
}


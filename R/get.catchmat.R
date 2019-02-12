### Function for getting catchment matrix
## Admin level average or weighted average by pop for each clinic
    
get.catchmat <- function(point_mat, fric, shape, admin = "district",
                         pop_rast, pop_pol, weighted = TRUE, type = "masked"){
    
  ## getting catchments
    catch_mat <- foreach(coords = iter(point_mat,"row"),
                         .packages = c('raster', 'rgdal', 'sp', 'gdistance'),
                         .errorhandling = 'stop',
                         .export = 'get.travel.times',
                         .combine = "cbind"
    ) %dopar% {
      print(coords)
      print(Sys.time())
      point_mat_sub <- as.matrix(coords)
      
      if (type == "masked"){
        trans_mat <- "output/trans_gc_masked.rds"
      } else {
        trans_mat = "output/trans_gc_unmasked.rds"
      }
      
      travel_time_pt <- get.travel.times(friction = fric, shapefile = shape, 
                                     coords = point_mat_sub, 
                                     trans_matrix_exists = TRUE, 
                                     filename_trans = trans_mat)

      if (weighted == TRUE){
        ## add 1e-6 to 0 ttimes
        travel_time_pt[travel_time_pt == 0] <- 1e-6
        weighted_ttimes <- travel_time_pt*pop_rast
        names(weighted_ttimes) <- "w_ttimes"
        out <- raster::extract(weighted_ttimes, shape, fun = sum, 
                                 na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
        out$ttimes <- out$w_ttimes/pop_pol
      } else {
        names(travel_time_pt) <- "ttimes"
        out <- raster::extract(travel_time_pt, shape, fun = mean, 
                               na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
      }
      out$ttimes 
    }
    
    write.csv(catch_mat, paste0("output/", admin, "_", "catchmat_", type, "_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    return(catch_mat)
}


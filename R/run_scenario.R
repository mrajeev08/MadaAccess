## Calculate incremental decrease in burden

## Read in addtl ctar
run.scenario <- function(current_ARMC, new_ARMC, friction, shape, pop_rast, pop_pol, 
                         admin = "district", weighted = TRUE, filename_trans) {
  ## to test
  # current_ARMC = gps_locs; candidate_ARMC = csbs[seq(1, 1000, by = 100), ];
  # friction = friction_unmasked; shape = mada_district;  
  # filename_trans = "output/trans_gc_unmasked.rds";
  
  print("starting ttime calcs for addtl ARMC")
  print(Sys.time())
  
  ## getting catchments
  ttime_mat <- foreach(j = 1:nrow(new_ARMC),
                       .packages = c('raster', 'rgdal', 'sp', 'gdistance'),
                       .errorhandling = 'stop',
                       .export = 'get.travel.times',
                       .combine = "cbind"
  ) %dopar% {
    
    point_mat <- rbind(current_ARMC, new_ARMC[j, ])
    point_mat <- as.matrix(cbind(point_mat$Y_COORD, point_mat$X_COORD)) # matrix of long and lat
    ttimes <- get.travel.times(friction, shapefile, coords = point_mat, trans_matrix_exists = TRUE, 
                               filename_trans)
    
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
  
  write.csv(ttime_mat, paste0("output/", admin, "_", "scenario", type, "_", 
                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  return(ttime_mat)
}
  



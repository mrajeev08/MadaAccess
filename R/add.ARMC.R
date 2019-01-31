## Incremental analysis
## parallelized function to add all 
add.armc <- function(current_ARMC, candidate_ARMC, prop_pop, threshold, delta_tt_min = 1e-5,
                     steps = 1000, base_prop, 
                     friction, shapefile, filename_trans) {
  
  ## For the number of possible clinics you could add
  prop_under <- rep(NA, nrow(candidate_ARMC))
  new_ARMC <- data.frame(CTAR = NA, X_COORD = NA, Y_COORD = NA) ## empty data frame to bind to
  
  for (i in 1:steps) {
    while (nrow(candidate_ARMC) > 0) {
      ## Iterate through and find the one that when you add it reduces travel times the most
      ranked_coords <- foreach(j = iter(candidate_ARMC, "row"),
                         .packages = c('raster', 'rgdal', 'sp', 'gdistance'),
                         .errorhandling = 'stop',
                         .export = 'get.travel.times',
                         .combine = "c"
      ) %dopar% {
        point_mat <- rbind(current_ARMC, candidate_ARMC[j, ])
        point_mat <- as.matrix(cbind(point_mat$Y_COORD, point_mat$X_COORD)) # matrix of long and lat
        times <- get.travel.times(friction, shapefile, coords = point_mat, trans_matrix_exists = TRUE, 
                                 filename_trans)
        
        ## proportion of pop living greater than threshold hrs away from CTAR (defaults to 3 hrs)
        sum(prop_pop[ttimes >= 60*threshold], na.rm = TRUE) 
      }
      
      ## rank the one that minimizes this
      prop_under[i] <- min(ranked_coords, na.rm = TRUE)
      new_armc <- rbind(new_ARMC, candidate_ARMC[which(ranked_coords == min(ranked_coords, na.rm = TRUE)), ])
      candidate_ARMC <- candidate_ARMC[-which(ranked_coords == min(ranked_coords, na.rm = TRUE)), ] 
      ## remove top ranked
      candidate_ARMC <- candidate_ARMC[-which(ranked_coords - base_prop <= delta_tt_min), ] 
      ## remove all which don't reduce above a certain threshold
    }
  }
  new_armc$prop_under <- prop_under
  return(new_armc)
}


## Incremental analysis
## parallelized function to add all 
add.armc <- function(current_ARMC, candidate_ARMC, prop_pop, threshold, delta_tt_min = 1e-5,
                     steps = 1000, base_prop, 
                     friction, shapefile, filename_trans) {
  ## to test
  # current_ARMC = gps_locs; candidate_ARMC = csbs[seq(1, 1000, by = 100), ];
  # threshold = 3; delta_tt_min = 1e-20;
  # steps = 10;
  # friction = friction_unmasked; shapefile = mada_district;
  # filename_trans = "output/trans_gc_unmasked.rds";

  ## For the number of possible clinics you could add
  prop_under <- rep(NA, ncol(candidate_ARMC))
  cands <- nrow(candidate_ARMC)
  new_ARMC <- data.frame(CTAR = rep(NA, cands), X_COORD = rep(NA, cands), Y_COORD = rep(NA, cands))

  ## empty data frame to bind to
  
  for (i in 1:steps) {
    if (nrow(candidate_ARMC) > 0) {
      ## Iterate through and find the one that when you add it reduces travel times the most
      ranked_coords <- foreach(j = 1:nrow(candidate_ARMC),
                         .packages = c('raster', 'rgdal', 'sp', 'gdistance'),
                         .errorhandling = 'stop',
                         .export = 'get.travel.times', 
                         .combine = c
      ) %dopar% {
        point_mat <- rbind(current_ARMC, candidate_ARMC[j, ])
        point_mat <- as.matrix(cbind(point_mat$Y_COORD, point_mat$X_COORD)) # matrix of long and lat
        ttimes <- get.travel.times(friction, shapefile, coords = point_mat, trans_matrix_exists = TRUE, 
                                 filename_trans)
        
        ## proportion of pop living greater than threshold hrs away from CTAR (defaults to 3 hrs)
        sum(prop_pop[ttimes >= 60*threshold], na.rm = TRUE) 
      }
      
      if(min(ranked_coords, na.rm = TRUE)[1] != Inf){
        ## rank the one that minimizes this and add to current armc
        current_ARMC <- rbind(current_ARMC, 
                              candidate_ARMC[which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ])
        
        ## add prop_under to the top ranked at the row i + 1 (because of place holder NA row)
        prop_under[i] <- min(ranked_coords, na.rm = TRUE)[1]
        
        ## add to new armc list
        new_ARMC[i, ] <- candidate_ARMC[which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ]
      }
      
      ## remove top ranked and also will result in no rows if ranked_coords is all NA
      candidate_ARMC <- candidate_ARMC[-which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ]
      
      # candidate_ARMC <- candidate_ARMC[-which(ranked_coords - base_prop <= delta_tt_min), ] 
      ## remove all which don't reduce above a certain threshold
    }
    else{
      next()
    }
  }
  new_ARMC$prop_under <- prop_under
  return(new_ARMC)
}


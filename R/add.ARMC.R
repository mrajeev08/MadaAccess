## Incremental analysis
## parallelized function to add ARMC sequentially
## updating base proportion at each step to eliminate ones that don't reduce that threshold further
## (i.e. should remove clustered clinic locations)

add.armc <- function(current_ARMC, candidate_ARMC, prop_pop, threshold, delta_tt_min = 1e-4,
                     steps = 1000, base_prop,
                     friction, shapefile, filename_trans, key_data) {
  ## to test
  # current_ARMC = gps_locs; candidate_ARMC = csbs[seq(1, 1000, by = 100), ];
  # threshold = 3; delta_tt_min = 1e-20;
  # steps = 10;
  # friction = friction_unmasked; shapefile = mada_district;
  # filename_trans = "output/trans_gc_unmasked.rds";

  ## For the number of possible clinics you could add
  prop_under <- rep(NA, ncol(candidate_ARMC))
  cands <- nrow(candidate_ARMC)
  
  ## empty data frame to bind to
  new_ARMC <- data.frame(CTAR = rep(NA, cands), X_COORD = rep(NA, cands), Y_COORD = rep(NA, cands))
  dist_mat <- matrix(NA, nrow = length(unique(key_data$district)), ncol = steps + 1)
  comm_mat <- matrix(NA, nrow = length(unique(key_data$commune)), ncol = steps + 1)
  
  ## Baseline one (also just for matching up row ids)
  check_dist <- key_data[, .(ttimes_dist = sum(ttimes_base*pop, na.rm = TRUE)/sum(pop, na.rm = TRUE)), by = district]
  check_comm <- key_data[, .(ttimes_comm = sum(ttimes_base*pop, na.rm = TRUE)/sum(pop, na.rm = TRUE)), by = commune]
  
  dist_mat[, 1] <- check_dist$ttimes_dist
  rownames(dist_mat) <- check_dist$district
  comm_mat[, 1] <- check_comm$ttimes_comm
  rownames(comm_mat) <- check_comm$commune
  
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
      
      ## remove top ranked and also will result in no rows if ranked_coords is all NA
      candidate_ARMC <- candidate_ARMC[-which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ]
      
      ## remove all which don't reduce above a certain threshold
      candidate_ARMC <- candidate_ARMC[-which(base_prop - ranked_coords <= delta_tt_min), ] 
      
      ## output to continue analysis in case of cluster timeout
      write.csv(candidate_ARMC, "output/temp_candidates.csv")
    
      
      ## save the data for the one that gets added
      if(min(ranked_coords, na.rm = TRUE)[1] != Inf){
        ## rank the one that minimizes this and add to current armc
        current_ARMC <- rbind(current_ARMC, 
                              candidate_ARMC[which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ])
        
        ## add prop_under to the top ranked at the row i + 1 (because of place holder NA row)
        prop_under[i] <- min(ranked_coords, na.rm = TRUE)[1]
        
        ## add to new armc list
        new_ARMC[i, ] <- candidate_ARMC[which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ]
        write.csv(new_ARMC, "output/temp_ARMC.csv")
        
        ## Getting layer and weighted means for the new set
        point_mat <- current_ARMC
        point_mat <- as.matrix(cbind(point_mat$Y_COORD, point_mat$X_COORD)) # matrix of long and lat
        ttimes <- get.travel.times(friction, shapefile, coords = point_mat, trans_matrix_exists = TRUE, 
                                   filename_trans)
        key_data$ttimes_new <- values(ttimes)
      
        check_dist <- key_data[, .(ttimes_dist = sum(ttimes_new*pop, na.rm = TRUE)/sum(pop, na.rm = TRUE)), by = district]
        check_comm <- key_data[, .(ttimes_comm = sum(ttimes_new*pop, na.rm = TRUE)/sum(pop, na.rm = TRUE)), by = commune]
        
        dist_mat[, i + 1] <- check_dist$ttimes_dist
        write.csv(dist_mat, "output/temp_disttimes.csv")
        comm_mat[, i + 1] <- check_comm$ttimes_comm
        write.csv(comm_mat, "output/temp_commttimes.csv")
        
        ## new baseline proportion to diff against 
        base_prop <- sum(prop_pop[ttimes >= 60*threshold], na.rm = TRUE) 
      }
    }
    else{
      next()
    }
  }
  # new_ARMC$prop_under <- prop_under ## This throws errors due to discrepancies so avoid this for now
  return(list(new_ARMC = new_ARMC, prop_under = prop_under, dist_mat, comm_mat))
}


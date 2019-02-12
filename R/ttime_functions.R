## Functions for working with friction surface + travel time layer
## Malavika Rajeev 2018


# 1. Getting travel time raster layer from set of points -----------------------------------------
## Function for getting travel times given points 
## Adapted from https://map.ox.ac.uk/research-project/accessibility_to_cities/

get.travel.times <- function(friction, shapefile, coords, trans_matrix_exists = TRUE, 
                             filename_trans){
  
  ## crop friction surface to shapefile
  friction <- crop(friction, shapefile)
  
  ## calculating travel times
  ## Fetch the number of points
  n_points <- dim(coords)[1]
  
  ## Make the graph and the geocorrected version of the graph (or read in the latter).
  if (trans_matrix_exists == TRUE) {
    # Read in the transition matrix object if it has been pre-computed
    trans_gc <- readRDS(filename_trans)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    trans <- transition(friction, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
    #saveRDS(Trans, filename.nonGCtrans)
    trans_gc <- geoCorrection(trans)
    saveRDS(trans_gc, filename_trans)
  }
  
  ## Convert the points into a matrix
  xy_df <- data.frame()
  xy_df[1:n_points, 1] <- coords[, 1]
  xy_df[1:n_points, 2] <- coords[, 2]
  xy_matrix <- as.matrix(xy_df)
  
  ## Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  travel_times <- accCost(trans_gc, xy_matrix)
  
  # ## Clip to Mada
  # travel_times <- crop(travel_times, shapefile)
  # travel_times <- mask(travel_times, shapefile)

  ## Write the resulting raster
  return(travel_times)
}


# 2. Get catchments for each admin unit (district or commune) ------------------------------------
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

# 3. Add ARMC for scenario analysis --------------------------------------------------------------
## parallelized function to add ARMC sequentially
## updating base proportion at each step to eliminate ones that don't reduce that threshold further
## (i.e. should remove clustered clinic locations)
add.armc <- function(current_ARMC, candidate_ARMC, prop_pop, threshold, delta_tt_min = 1e-4,
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
  
  ## empty data frame to bind to
  new_ARMC <- data.frame(CTAR = rep(NA, cands), X_COORD = rep(NA, cands), Y_COORD = rep(NA, cands))
  
  print("starting iterative adding of ARMC")
  print(Sys.time())
  
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
      
      ## save the data for the one that gets added
      if(min(ranked_coords, na.rm = TRUE)[1] != Inf){
        ## rank the one that minimizes this and add to current armc
        current_ARMC <- rbind(current_ARMC, 
                              candidate_ARMC[which(ranked_coords == min(ranked_coords, 
                                                                        na.rm = TRUE)[1]), ])
        write.csv(current_ARMC, "output/temp_ARMC.csv")
        
        ## add prop_under to the top ranked at the row i + 1 (because of place holder NA row)
        prop_under[i] <- min(ranked_coords, na.rm = TRUE)[1]
        write.csv(prop_under, "output/temp_prop_under.csv")
        
        ## add to new armc list
        new_ARMC[i, ] <- candidate_ARMC[which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ]
      }
      
      ## remove top ranked and also will result in no rows if ranked_coords is all NA
      candidate_ARMC <- candidate_ARMC[-which(ranked_coords == min(ranked_coords, na.rm = TRUE)[1]), ]
      
      ## remove all which don't reduce above a certain threshold
      candidate_ARMC <- candidate_ARMC[-which(base_prop - ranked_coords <= delta_tt_min), ] 
      
      ## output to continue analysis in case of cluster timeout
      write.csv(candidate_ARMC, "output/temp_candidates.csv")
      
      ## new baseline proportion to diff against 
      base_prop <- prop_under[i] 
      
      ## check for each loop to keep track of time
      print(paste(i, "ARMC added"))
      print(Sys.time())
      print(paste(nrow(candidate_ARMC), "remaining"))
    }
    else{
      next()
    }
  }
  # new_ARMC$prop_under <- prop_under ## This throws errors due to discrepancies so avoid this for now
  return(list(current_ARMC = current_ARMC, new_ARMC = new_ARMC, prop_under = prop_under))
}


# 4. Get travel times at admin level for scenario analysis ------------------------------------
## Calculating travel times at whatever admin level for the scenario analysis
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
    
    point_mat <- rbind(current_ARMC, new_ARMC[1:j, ])
    point_mat <- as.matrix(cbind(point_mat$Y_COORD, point_mat$X_COORD)) # matrix of long and lat
    ttimes <- get.travel.times(friction, shape, coords = point_mat, trans_matrix_exists = TRUE, 
                               filename_trans)
    
    if (weighted == TRUE){
      ## add 1e-6 to 0 ttimes
      ttimes[ttimes == 0] <- 1e-6
      weighted_ttimes <- ttimes*pop_rast
      names(weighted_ttimes) <- "w_ttimes"
      out <- raster::extract(weighted_ttimes, shape, fun = sum, 
                             na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
      out$ttimes <- out$w_ttimes/pop_pol
    } else {
      names(ttimes) <- "ttimes"
      out <- raster::extract(ttimes, shape, fun = mean, 
                             na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
    }
    out$ttimes 
  }
  
  write.csv(ttime_mat, paste0("output/", admin, "_", "scenario", type, "_", 
                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  return(ttime_mat)
}

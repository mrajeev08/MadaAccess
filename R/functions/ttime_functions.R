# 1. Get travel times -----------------------------------------------------------------------------
#' \code{get.ttimes} calculates the minimum travel times/distance for an input raster to 
#' an input set of 
#' GPS points. 
#' This function uses the friction surface from the Malaria Atlas Project. Script adapted
#' from https://map.ox.ac.uk/research-project/ttimesibility_to_cities/. Uses least-cost algorithms
#' from the gdistance package.
#' @param friction raster, the friction surface downloaded from MAP website
#' @param shapefile polygon shapefile, to mask the friction surface to
#' @param coords matrix of two columns of x (first col, longitude) and y (second col, latitude) points to 
#'   input to calculate the least-cost distance (here travel times)
#' @param trans_matrix_exists logical, if TRUE then looks for file as specified by filename_trans
#'   if FALSE then creates the transition matrix using function transition from gdistance package
#' @param filename_trans character, the path to which the transition matrix should either be 
#'   read from or written to
#' @return raster at same resolution as the input friction surface and cropped to the shapefile with 
#'   the minimum ttimes metric estimate as the values
#' @section Dependencies:
#'  Packages: gdistance, raster, rgdal, sp

get.ttimes <- function(friction, shapefile, coords, trans_matrix_exists = TRUE, 
                             filename_trans){
  
  # crop friction surface to shapefile
  friction <- crop(friction, shapefile)
  
  # Fetch the number of points
  n_points <- nrow(coords)
  
  # Make the graph and the geocorrected version of the graph (or read in the latter).
  if (trans_matrix_exists == TRUE) {
    # Read in the transition matrix object if it has been pre-computed
    trans_gc <- readRDS(filename_trans)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    trans <- transition(friction, function(x) 1/mean(x), 8) # RAM intensive, 
    # can be very slow for large areas
    trans_gc <- geoCorrection(trans)
    saveRDS(trans_gc, filename_trans)
  }
    
  # Run the accumulated cost algorithm to make the final output map. 
  # This can be quite slow depending on the area
  ttimes <- accCost(trans_gc, coords)
  
  # Return the resulting raster
  return(ttimes)
}

# 2. Getting ranked clinics and district/commune covariates  ----------------------------------
#' Rank clinics and summarize travel times and catchments at admin level
#' \code{add.armc} gets ranked ARMC and associated travel times and catchments at the district and
#' commune levels using catchment matrix of travel times at each grid cell (rows) for each clinic
#' (columns) to rank which order clinics should be added based on how adding them shifts the 
#' distribution of travel times at the population level.
#' 
#' @param base_df a data.table with the following rows for each grid cell:
#'  ttimes (the baseline travel times), prop_pop (the proportion of the population), 
#'  catchment (the baseline clinic catchment id), pop_dist (the total population in the district
#'  in which the grid cell falls), pop_dist (the total population in the commune in which the grid 
#'  cell falls), commcode (corresponds to commcode in shapefile)), 
#'  distcode (corresponds to row number in distcode in shapefile);
#' @param clinic_names character vector of the names of the candidate ARMC to be added
#' @param clinic_catchmat a matrix of ttimes estimates for each of the grid cells (rows) in the
#' shapefile for each of the candidate clinics (columns, should match length of clinic_names vector) 
#' @param prop_pop a numeric vector of the proportion of the total population in each grid cell
#' @param max_clinics numeric, the number of clinics that you want to add (when to stop adding)
#' @param thresh_ttimes numeric, the threshold travel times, any decreases in travel times resulting 
#' from addition of a clinic are ignored (trying to target populations with worst access)
#' @param thresh_prop numeric between 0-1, if a clinic is added and it shifts travel times above 
#' thresh_ttimes but only for less than thresh_prop, then the clinic is filtered out
#' @param dir_name the directory name to output the resulting data frames into
#' @param overwrite boolean, if TRUE then overwrites data at first step and then appends; if FALSE
#' then appends all to existing files
#' 
#' @return the final travel times and catchments at the grid cell level 
#' @details The results, aggregated to the district and commune levels, are written to a file at each 
#' step to limit memory used.
#' @section Dependencies:
#'     Packages: data.table, foreach

add.armc <- function(base_df, clinic_names, clinic_catchmat, 
                     max_clinics = ncol(clinic_catchmat), thresh_ttimes, 
                     thresh_prop, dir_name, overwrite = TRUE) {
  
  prop.lessthan <- function(x, prop_pop, base_ttimes, threshold) {
    # Sum of the proportion of the population for which ttimes decreased with addition of clinic
    # BUT only for those people living above threshold ttimes
    sum(prop_pop[which(x < base_ttimes & x > threshold)], na.rm = TRUE)
  }
  
  # Add clinics incrementally
  for (i in 1:max_clinics) {
    
    print(ncol(clinic_catchmat))
    
    if (ncol(clinic_catchmat) > 0) {
      print(i)

      sum.prop <-
        foreach(vals = iter(clinic_catchmat, by = "col"),
                .combine = c) %dopar% {
                  prop.lessthan(vals, prop_pop = base_df$prop_pop, 
                                        base_ttimes = base_df$ttimes, threshold = thresh_ttimes)
                }
      
      # In case all admin units go below the threshold: stop adding
      clinic_id <- clinic_names[which.max(sum.prop)]
      base_df[, new_ttimes := clinic_catchmat[[which.max(sum.prop)]]]
      
      # If ttimes improved then, new ttimes replaces the baseline and catchment
      base_df[, c('ttimes', 'catchment') := .(fifelse(new_ttimes < ttimes, new_ttimes,
                                                          ttimes), 
                                                  fifelse(new_ttimes < ttimes, clinic_id,
                                                          catchment))]
      
      # So as to not use Inf in summarizing by district/commune
      base_df[, raw_ttimes := ifelse(is.infinite(ttimes), NA, ttimes)]
      
      # Take out the max ones and any below ttime + pop thresholds
      clinic_names <- clinic_names[-c(which.max(sum.prop), which(sum.prop == 0),
                                      which(sum.prop < thresh_prop))]
      clinic_catchmat[, unique(c(which.max(sum.prop), which(sum.prop == 0), 
                                 which(sum.prop < thresh_prop))) := NULL]
      
      # deal with NAs
      base_to_agg <- base_df[!is.na(raw_ttimes)]
      base_to_agg[, pop_wt_dist := sum(pop, na.rm = TRUE), by = distcode]
      base_to_agg[, pop_wt_comm := sum(pop, na.rm = TRUE), by = commcode]
      
      # create district and commune dataframes
      district_df <-
        base_to_agg[, .(ttimes_wtd = sum(raw_ttimes * pop, na.rm = TRUE), 
                      prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_dist[1],
                      pop_wt_dist = pop_wt_dist[1],
                      scenario = i, clinic_added = clinic_id, pop = pop_dist[1]), 
                by = .(distcode, catchment)]
      district_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop_wt_dist, by = distcode]
      
      commune_df <-
        base_to_agg[, .(ttimes_wtd = sum(raw_ttimes * pop, na.rm = TRUE),
                      prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_comm[1],
                      pop_wt_comm = pop_wt_comm[1],
                      scenario = i, clinic_added = clinic_id, pop = pop_comm[1]), 
                by = .(commcode, catchment)]
      commune_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop_wt_comm, by = commcode]
      
      if(overwrite == TRUE & i == 1) { # overwrite on first one & use gzip to commpress)
        fwrite(district_df, paste0(dir_name, "district.gz"))
        fwrite(commune_df,  paste0(dir_name, "commune.gz"))
      } else {
        fwrite(district_df, paste0(dir_name, "district.gz"), append = TRUE)
        fwrite(commune_df,  paste0(dir_name, "commune.gz"), append = TRUE)
      }
      
    } else {
      break
    }
  }
  
  # Return the last catch and ttimes vals
  return(list(ttimes = base_df$ttimes, catches = base_df$catchment))
}


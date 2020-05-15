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
#'  in which the grid cell falls), pop_comm (the total population in the commune in which the grid 
#'  cell falls), commcode (corresponds to commcode in shapefile)), 
#'  distcode (corresponds to distcode in shapefile);
#' @param clinic_names vector of the names or ids of the candidate ARMC to be added
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
#' step to limit memory used. Note that the pop_wt_comm/dist is the population for which travel times are 
#' not infinite for a given clinic
#' @section Dependencies:
#'     Packages: data.table, foreach

# Pass through base df, otherwise you need all the vectors!
add.armc <- function(base_df, cand_df, max_clinics, thresh_ttimes, 
                     thresh_prop, dir_name, overwrite = TRUE) {
  
  # Pull in references to files (that way not as big!)
  cand_list <- vector("list", nrow(cand_df))
  foreach(j = 1:nrow(cand_df), .packages = "raster") %do% {
    cand_list[[j]] <- raster(cand_df$candfile[j], band = cand_df$band[j])
  }
  
  for (i in 1:max_clinics) {
    
    if (nrow(cand_df) > 0) {

      foreach(j = 1:nrow(cand_df), .combine = c, .packages = "raster") %dopar% {
              cand_ttimes <- values(cand_list[[j]])
              inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes)
              prop_wtd <- sum(base_df$prop_pop[inds] * # pop affected
                              ((base_df$ttimes[inds] - cand_ttimes[inds])/base_df$ttimes[inds]), 
                              na.rm = TRUE) # weighted by reduction 
      } -> sum_prop
  
      # In case all admin units go below the threshold: stop adding
      clin_chosen <- cand_df[which.max(sum_prop), ]
      prop_pop <- max(sum_prop[!is.infinite(sum_prop)], na.rm = TRUE)
      prop_df <- data.table(scenario = i, clin_chosen, prop_pop)
      new_ttimes <- values(cand_list[[which.max(sum_prop)]]) # add to base_df
      
      # Take out the max ones and any below ttime + pop thresholds
      remove <- unique(c(which.max(sum_prop), which(sum_prop == 0), which(sum_prop < thresh_prop)))
      cand_df <- cand_df[-remove, ]
      cand_list <- cand_list[-remove] 
      
      # Aggregating to district & commune
      # If ttimes improved then, new ttimes replaces the baseline and catchment
      base_df[, c('ttimes', 'catchment') := 
                .(fifelse((new_ttimes < ttimes & !is.na(new_ttimes)) |
                            (!is.na(new_ttimes) & is.na(ttimes)), new_ttimes, ttimes), 
                  fifelse((new_ttimes < ttimes & !is.na(new_ttimes)) |
                            (!is.na(new_ttimes) & is.na(ttimes)), clin_chosen$clinic_id, catchment))]
  
      district_df <- aggregate.admin(base_df = base_df, admin = "distcode", scenario = i)
      district_maxcatch <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                                       by = .(distcode, scenario)]
      
      # Commune
      commune_df <- aggregate.admin(base_df = base_df, admin = "commcode", scenario = i)
      commune_maxcatch <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                                     by = .(commcode, scenario)]
      
      # Do the max one here too!
      if(overwrite == TRUE & i == 1) { # overwrite on first one & use gzip to commpress)
        fwrite(district_df, paste0(dir_name, "district_allcatch.gz"))
        fwrite(commune_df,  paste0(dir_name, "commune_allcatch.gz"))
        fwrite(district_maxcatch, paste0(dir_name, "district_maxcatch.gz"))
        fwrite(commune_maxcatch,  paste0(dir_name, "commune_maxcatch.gz"))
        fwrite(prop_df, paste0(dir_name, "prop_df.csv"))
      } else {
        fwrite(district_df, paste0(dir_name, "district_allcatch.gz"), append = TRUE)
        fwrite(commune_df,  paste0(dir_name, "commune_allcatch.gz"), append = TRUE)
        fwrite(district_maxcatch, paste0(dir_name, "district_maxcatch.gz"), append = TRUE)
        fwrite(commune_maxcatch,  paste0(dir_name, "commune_maxcatch.gz"), append = TRUE)
        fwrite(prop_df, paste0(dir_name, "prop_df.csv"), append = TRUE)
      }

      print(paste(i, "clinic added:", clin_chosen$district, "District", clin_chosen$commune, 
                  "Commune"))
            
    } else {
      break
    }
  }
  
  # Return the last catch and ttimes vals
  return(list(ttimes = base_df$ttimes, catches = base_df$catchment))
}

# Get grid cell catchments for set of clinics ------------------------------------------------
update.base <- function(cand_df, base_df, nsplit = 2) {
  
  base <- copy(base_df)
  
  # Pull in references to files (that way not as big!)
  cand_list <- vector("list", nrow(cand_df))
  
  foreach(j = 1:nrow(cand_df), .packages = "raster") %do% {
    cand_list[[j]] <- raster(cand_df$candfile[j], band = cand_df$band[j])
  }
  
  names(cand_list) <- cand_df$clinic_id

  # Split candidates
  out <- split(cand_list, rep(1:nsplit, each = (ceiling(length(cand_list)/nsplit))))
  
  multicomb <- function(x, ...) {
    mapply(cbind, x, ..., SIMPLIFY = FALSE)
  }
  
  foreach(i = 1:length(out),
          .packages = c("raster", "data.table"), .combine = multicomb) %dopar% {
          
          sub <- out[[i]]
          cand_ttimes <- values(sub[[1]])
          cand_catch <- ifelse(!is.na(cand_ttimes), names(sub)[1], NA)
            
          for(j in 2:length(sub)) {
            cand_comp <- values(sub[[j]])
            cand_catch <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
                                  (!is.na(cand_comp) & is.na(cand_ttimes)), names(sub)[j], 
                                  cand_catch)
            cand_ttimes <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
                                     (!is.na(cand_comp) & is.na(cand_ttimes)), cand_comp, cand_ttimes)
          }
          list(catches = cand_catch, ttimes = cand_ttimes)
  } -> catches
  
  min_ttimes <- catches[["ttimes"]]
  min_catches <- catches[["catches"]]
  cand_ttimes <- min_ttimes[, 1]
  cand_catch <- min_catches[, 1]
  
  for(i in 2:ncol(min_ttimes)) {
    cand_comp <- min_ttimes[, i]
    cand_catch_comp <- min_catches[, i]
    cand_catch <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
                            (!is.na(cand_comp) & is.na(cand_ttimes)), cand_catch_comp, cand_catch)
    cand_ttimes <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
                             (!is.na(cand_comp) & is.na(cand_ttimes)), cand_comp, cand_ttimes)
  }
  
  cand_catch <- as.numeric(cand_catch)
  
  base[, c('ttimes', 'catchment') := 
            .(fifelse((cand_ttimes < ttimes & !is.na(cand_ttimes)) |
                        (!is.na(cand_ttimes) & is.na(ttimes)), cand_ttimes, ttimes), 
              fifelse((cand_ttimes < ttimes & !is.na(cand_ttimes)) |
                        (!is.na(cand_ttimes) & is.na(ttimes)), cand_catch, catchment))]
  
  return(base)
}


# Get max catch -------------------------------------------------------------------------------

get.maxcatch <- function(admin_df, admin = "distcode") {
  admin_df <- admin_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                                 by = .(get(admin), scenario)]
  setnames(admin_df, "get", admin)
  return(admin_df)
}


# Get brick list ------------------------------------------------------------------------------
get.bricks <- function(brick_dir = "output/ttimes/candidates") {
  
  bricks <- list.files(brick_dir, full.names = TRUE)
  
  foreach(i = iter(bricks), .combine = rbind) %do% {
    cands <- as.numeric(gsub("[.a-z]", "", grep("cand[0-9]", unlist(strsplit(i, "_")),  
                                                value = TRUE)))
    out <- data.table(clinic_id = min(cands):max(cands), candfile = i, min = min(cands), 
                      max = max(cands))
    out[, band := (clinic_id - min + 1)]
    out
  } -> brick_dt
  
  return(brick_dt)
}

# Separate function for aggregate to admin ----------------------------------------------------
aggregate.admin <- function(base_df, admin = "distcode", scenario) {
  base <- copy(base_df)
  base[, c("pop_admin", "pop_wt") := 
            .(sum(pop, na.rm = TRUE), sum(pop[!is.na(ttimes)], na.rm = TRUE), scenario), 
          by = get(admin)]
  
  admin_df <-
    base[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE),
                ttimes_un = sum(ttimes, na.rm = TRUE),
                ncells = .N,
                prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt[1],
                pop_wt = pop_wt[1],
                pop_admin = pop_admin[1]),
            by = .(get(admin), catchment)] # first by catchment 
  admin_df[, c("ttimes_wtd", "ttimes_un", "scenario") := 
             .(sum(ttimes_wtd, na.rm = TRUE)/pop_wt, 
               sum(ttimes_un, na.rm = TRUE)/sum(ncells, na.rm = TRUE),
               scenario), by = get] # then by district
  
  admin_df <- admin_df[!is.na(catchment)]
  
  setnames(admin_df, "get", admin)
  
  return(admin_df)
}

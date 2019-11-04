####################################################################################################
##' Access functions 
##' Details: Various functions for getting travel times, minimum distance, catchments, etc. 
##' Author: Malavika Rajeev 
####################################################################################################


##' 1. Getting access metrics at raster level
##' ------------------------------------------------------------------------------------------------
#' Get minimum travel times or distance
#' \code{get.access} calculates the minimum travel times/distance for an input raster to 
#' an input set of 
#' GPS points. 
#' This function uses the friction surface from the Malaria Atlas Project. Script adapted
#' from https://map.ox.ac.uk/research-project/accessibility_to_cities/. Uses least-cost algorithms
#' from the gdistance package.
#' @param friction raster, the friction surface downloaded from MAP website. If metric = "distance"
#' then all non-NA values in the friction surface are set to 1 (for 1 x 1 km raster)
#' @param metric character, either "ttimes" or "distance"
#' @param shapefile polygon shapefile, to mask the friction surface to
#' @param coords matrix of two columns of x(longitude) and y (latitude) points to 
#'   input to calculate the least-cost distance (here travel times)
#' @param trans_matrix_exists logical, if TRUE then looks for file as specified by filename_trans
#'   if FALSE then creates the transition matrix using function transition from gdistance package
#' @param filename_trans character, the path to which the transition matrix should either be 
#'   read from or written to
#' @return raster at same resolution as the input friction surface and cropped to the shapefile with 
#'   the minimum access metric estimate as the values
#' @section Dependencies:
#'  Packages: gdistance, raster, rgdal, sp, geosphere

get.access <- function(friction, shapefile, coords, trans_matrix_exists = TRUE, 
                             filename_trans, metric = "ttimes"){
  
  ## crop friction surface to shapefile
  friction <- crop(friction, shapefile)
  
  if (metric == "distance") {
    fric_coords <- coordinates(friction)[-which(is.na(values(friction))), ]
    access <- friction
    access[!is.na(access)] <- apply(distm(fric_coords, coords), 1, min)/1000
  }
  
  if(metric == "ttimes") {
    ## Fetch the number of points
    n_points <- nrow(coords)
    
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
    
    ## Run the accumulated cost algorithm to make the final output map. 
    ## This can be quite slow (potentially hours).
    access <- accCost(trans_gc, coords)
  }
  
  ## Return the resulting raster
  return(access)
}


##' 2. Getting access metric for each point to each admin unit 
##' ------------------------------------------------------------------------------------------------
#' Get access matrix for admin units x GPS points
#' \code{get.catchmat} uses \code{get.access} and \code{raster::extract} to calculate minimum
#' access metric (either distance or travel times) for each admin unit to each point.
#' Using the foreach package to parallelize. In order to speed up, the transition matrix must already
#' have been created.
#' @param point_mat two-column matrix with x (longitude) and y (latitude)
#' @param point_names names of clinics corresponding to each point in point_mat, foreach loop keeps the output
#' in order so these are passed as colnames to the output matrix
#' @param admin_names names of admin units corresponding to shapefile, foreach loop keeps the output
#' in order so these are passed as rownames to the output matrix
#' @param fric raster friction surface to pass to \code{get.access}; either "masked" or "unmasked", 
#' to get ttimes for coastal admin unit/islands used unmasked (allows for travel by sea basically, 
#' see https://www.nature.com/articles/nature25181 
#' for more details on how they treat travel times over water).
#' @param shape polygon shapefile to pass to \code{get.access} and extract travel times to
#' @param pop_rast raster of population size at same resolution and extent as friction surface
#' @param pop_pol vector of population sizes associated with the shapefile polygons 
#' @param trans_mat character, name of file name of transition matrix, this must already exist!
#' @param weighted boolean, whether to weight by population in grid cells
#' @param met character, either "ttimes" or "distance"
#' @return Returns a matrix of access estimates for each of the admin units in the
#' shapefile to each of the gps points 
#' @section Dependencies:
#'     Packages: gdistance, raster, foreach, rgdal, sp
#'     Functions: get.access

get.catchmat <- function(point_mat, point_names, admin_names, fric, shape, pop_rast, 
                         pop_pol, trans_mat = "data/processed/rasters/trans_gc_masked.rds", 
                         weighted = TRUE, 
                         met = "ttimes"){
  
  ## getting catchments
  catchmat <- foreach(coords = iter(point_mat,"row"),
                       .packages = c('raster', 'rgdal', 'sp', 'gdistance', 'geosphere'),
                       .errorhandling = 'stop',
                       .export = 'get.access',
                       .combine = "cbind"
  ) %dopar% {
    print(coords)
    print(Sys.time())
    point_mat_sub <- as.matrix(coords)

    access_pt <- get.access(friction = fric, shapefile = shape, 
                                       coords = point_mat_sub, 
                                       trans_matrix_exists = TRUE, 
                                       filename_trans = trans_mat, 
                                       metric = met)
    
    if (weighted == TRUE){
      ## add 1e-6 to 0 access
      access_pt[access_pt == 0] <- 1e-6
      weighted_access <- access_pt*pop_rast
      names(weighted_access) <- "w_access"
      out <- raster::extract(weighted_access, shape, fun = sum, 
                             na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
      out$access <- out$w_access/pop_pol
    } else {
      names(access_pt) <- "access"
      out <- raster::extract(access_pt, shape, fun = mean, 
                             na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
    }
    out$access 
  }
  
  colnames(catchmat) <- point_names
  rownames(catchmat) <- admin_names
  
  return(catchmat)
}

##' 3. Getting ranked clinics to add from list of potential clinics
##' ------------------------------------------------------------------------------------------------
#' Getting ranked ARMC to add from list of potential ARMC
#' Using catchment matrix generated from get.catchmat to rank which order clinics should be added based
#' on how adding them changes the admin level estimates of access
#' @param base_metric numeric vector of the base estimates of access (length should equal the number of rows
#'  in @param clinic_catchmat)
#' @param clinic_names character vector of the names of the candidate ARMC to be added
#' @param clinic_catchmat a matrix of access estimates for each of the admin units (rows) in the
#' shapefile for each of the candidate clinics (columns, should match the clinic_names vector) TO DO
#' change so that uses rownames from this instead?
#' @param prop_pop a numeric vector of the proportion of the total population in each admin unit
#' @param max_clinics numeric, the number of clinics that you want to add total
#' @param threshold numeric, the threshold distance or travel times for which you're trying to improve
#' access
#' @return A matrix with the minimum access metrics for each admin unit as each clinic is added.
#' Column names correspond to which clinic was added.
#' @section Dependencies:
#'     Packages: none


add.armc <- function(base_metric, base_catches, clinic_names, clinic_catchmat, prop_pop, 
                     max_clinics = ncol(clinic_catchmat), threshold) {
  # base_metric = rep(1e6, nrow(stacked_ttimes)); base_catches = rep(NA, nrow(stacked_ttimes));
  # clinic_names = 1:ncol(stacked_ttimes);
  # clinic_catchmat = stacked_ttimes;
  # prop_pop = prop_pop;
  # max_clinics = ncol(stacked_ttimes);
  # threshold = 3*60;

  metric_mat <- matrix(NA, nrow = nrow(clinic_catchmat), ncol = max_clinics)
  catch_mat <- matrix(NA, nrow = nrow(clinic_catchmat), ncol = max_clinics)
  
  colnames(metric_mat) <- 1:max_clinics
  
  ## helper function for add armc
  sum.lessthan <- function(x, prop_pop, base_metric, threshold) {
    ## Sum of the proportion of the population changes and changes to less than threshold
    mets <- prop_pop*(base_metric - x)
    sum(mets[which(x < base_metric & x > threshold)], na.rm = TRUE)
  }
  
  ## Add clinics incrementally
  for (i in 1:(max_clinics - 1)) {
    
    print(i)
    sum.change <- foreach(vals = iter(clinic_catchmat, by = "col"), 
                          .combine = c) %dopar% {
      sum.lessthan(vals, prop_pop = prop_pop, base_metric = base_metric, 
                   threshold = threshold)
    }
    
    ## In case all admin units go below the threshold
    if(sum(sum.change, na.rm = TRUE) == 0) {
      threshold <- 0
      sum.change <- foreach(vals = iter(clinic_catchmat, by = "col"), .combine = c) %dopar% {
        sum.lessthan(vals, prop_pop = prop_pop, base_metric = base_metric, 
                     threshold = threshold)
      }
    }
    
    print(threshold)
    
    if(sum(sum.change, na.rm = TRUE) == 0) {
      break
    } else {
      
      colnames(metric_mat)[i] <- as.character(clinic_names[which.max(sum.change)])
      new_metric <- clinic_catchmat[, which.max(sum.change)]
      base_catches[which(new_metric < base_metric)] <- as.character(clinic_names[which.max(sum.change)])
        
      base_metric[which(new_metric < base_metric)] <- new_metric[which(new_metric < base_metric)]
      
      clinic_names <- clinic_names[-which.max(sum.change)]
      clinic_catchmat <- clinic_catchmat[, -which.max(sum.change)]
      
      metric_mat[, i] <- base_metric
      catch_mat[, i] <- base_catches
    }
  }
  
  ## last clinic to be added
  if(is.null(dim(clinic_catchmat))) {
    ## Do catches 1st so that it is less than
    base_catches[which(clinic_catchmat < base_metric)] <- as.character(clinic_names)
    base_metric[which(clinic_catchmat < base_metric)] <- clinic_catchmat[which(clinic_catchmat < base_metric)]
    metric_mat[, ncol(metric_mat)] <- base_metric
    catch_mat[, ncol(metric_mat)] <- base_catches
    colnames(metric_mat)[ncol(metric_mat)] <- as.character(clinic_names)
    
  } else {
    
    sum.change <- foreach(vals = iter(clinic_catchmat, by = "col"), .combine = c) %dopar% {
      sum.lessthan(vals, prop_pop = prop_pop, base_metric = base_metric, 
                   threshold = threshold)
    }
    
    colnames(metric_mat)[ncol(metric_mat)] <- as.character(clinic_names[which.max(sum.change)])
    new_metric <- clinic_catchmat[, which.max(sum.change)]
    base_metric[which(new_metric < base_metric)] <- new_metric[which(new_metric < base_metric)]
    base_catches[which(new_metric < base_metric)] <- as.character(clinic_names[which.max(sum.change)])
    
    metric_mat[, ncol(metric_mat)] <- base_metric
    catch_mat[, ncol(metric_mat)] <- base_catches
  }
  
  return(list(metric_mat = metric_mat, catch_mat = catch_mat))
}


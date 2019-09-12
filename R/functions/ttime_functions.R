####################################################################################################
##' Travel time functions 
##' Details: Various functions for getting travel times, catchments, etc. 
##' Author: Malavika Rajeev 
####################################################################################################


##' 1. Getting travel times 
##' ------------------------------------------------------------------------------------------------
#' Get minimum travel times
#' \code[get.travel.times] calculates the minimum travel times for an input raster to an input set of 
#' GPS points. 
#' This function uses the friction surface from the Malaria Atlas Project. Script adapted
#' from https://map.ox.ac.uk/research-project/accessibility_to_cities/. Uses least-cost algorithms
#' from the gdistance package.
#' @param friction raster, the friction surface downloaded from MAP website
#' @param shapefile shapefile, to mask the friction surface to
#' @param coords matrix of two columns of x(longitude) and y (latitude) points to 
#'   input to calculate the least-cost distance (here travel times)
#' @param trans_matrix_exists logical, if TRUE then looks for file as specified by filename_trans
#'   if FALSE then creates the transition matrix using function transition from gdistance package
#' @param filename_trans character vector, the path to which the transition matrix should either be 
#'   read from or written to
#' @return raster at same resolution as the input friction surface and cropped to the shapefile with 
#'   the minimum travel time estimate as the values
#' @section Dependencies:
#'  Packages: gdistance, raster 
# 1. Getting travel time raster layer from set of points -----------------------------------------
## Function for getting travel times given points 
## Adapted from 

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


##' 2. Getting minimum travel times for each point to each admin unit 
##' ------------------------------------------------------------------------------------------------
#' Title
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

## Admin level average or weighted average by pop for each clinic

get.catchmat <- function(point_mat, fric, shape, admin = "district", pop_rast, 
                         pop_pol, trans_mat, weighted = TRUE, type = "masked"){
  
  ## getting catchments
  catchmat <- foreach(coords = iter(point_mat,"row"),
                       .packages = c('raster', 'rgdal', 'sp', 'gdistance'),
                       .errorhandling = 'stop',
                       .export = 'get.travel.times',
                       .combine = "cbind"
  ) %dopar% {
    print(coords)
    print(Sys.time())
    point_mat_sub <- as.matrix(coords)

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
  return(catchmat)
}


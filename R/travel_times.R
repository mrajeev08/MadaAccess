## Function for getting travel times given points 
## Adapted from https://map.ox.ac.uk/research-project/accessibility_to_cities/
## Malavika Rajeev 2018

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
  
  ## Clip to Mada
  travel_times <- crop(travel_times, shapefile)
  travel_times <- mask(travel_times, shapefile)
  
  ## Write the resulting raster
  return(travel_times)
}
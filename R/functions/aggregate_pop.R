#' Aggregate population to friction surface raster
#' Takes two spatial pixels that have the admin id
#' Filters by admin id and aggregates
#' @param Parameters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

aggregate.pop <- function(friction_pixels, pop_pixels, levels, pop_rast) {
  
  dist_pops <- foreach(i = 1:levels) %dopar% {
    friction <- friction_pixels[friction_pixels$layer %in% i, ] # filter to district
    pop <- pop_pixels[pop_pixels$layer %in% i, ]
    friction$cell_id <- 1:length(friction)
    pop$fric_id <- over(pop, friction)$cell_id
    
    # Add to nearest non-NA neighbor with indexing
    tomatch <- pop[which(is.na(pop$fric_id) & !is.na(pop$pop_val)), ]
    opts <- pop[which(!is.na(pop$fric_id) & !is.na(pop$pop_val)), ]
    
    match_index <- find.nonNA(rast = pop_rast, match_inds = tomatch@grid.index, 
                              opt_inds = opts@grid.index, njumps = 500)
    
    # Data table
    ind_add <- data.table(pop = tomatch$pop_val, match_index)
    ind_add <- ind_add[ , .(pop = sum(pop)), by = "match_index"]
    
    # Add to closest non-NA fric val!
    pop$pop_val[pop@grid.index %in% ind_add$match_index] <- pop$pop_val[pop@grid.index %in%
                                                                  ind_add$match_index] + ind_add$pop
    
    resampled <- aggregate(pop["pop_val"], friction, function(x) sum(x, na.rm = TRUE)) # resample
    names(resampled) <- "pop"
    pop1x1 <- raster(resampled["pop"]) # transform back to raster
  }
  
  pop1x1 <- do.call(raster::merge, dist_pops)
  names(pop1x1) <- "pop"
  return(pop1x1)
}

#' Find nearest non-NA location of where to place pop
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

find.nonNA <- function(rast, match_inds, opt_inds, njumps) {
  
  ind_find <- data.table(rowColFromCell(rast, match_inds), 
                         index = match_inds)
  ind_find$matched <- FALSE
  ind_find$index_new <- ind_find$index
  x_move <- seq(1, njumps, by = 1)
  y_move <- seq(1, njumps, by = 1)
  ways_to_move <- rbind(data.table(x = x_move, y = y_move),
                            data.table(x = x_move, y = y_move - 1),
                            data.table(x = x_move - 1, y = y_move))
  ways_to_move <- rbind(ways_to_move, data.table(x = -ways_to_move$x, y = ways_to_move$y), 
                        data.table(x = ways_to_move$x, y = -ways_to_move$y),
                        data.table(x = -ways_to_move$x, y = -ways_to_move$y))
  ways_to_move[, c("order_x", "order_y") := list(abs(x), abs(y))]
  
  setorder(ways_to_move, order_x, order_y)
  
  for(i in 1:nrow(ways_to_move)) {
    move <- ways_to_move[i, ]
    if(length(ind_find$matched[ind_find$matched == FALSE]) > 0){
      ind_find[, c("index_new",
                 "matched") := list(ifelse(matched %in% FALSE, 
                                           cellFromRowCol(rast, row + move$x, col + move$y),
                                           index_new),
                                    ifelse(matched %in% FALSE,
                                           cellFromRowCol(rast, row + move$x, col + move$y) %in%
                                             opt_inds, matched))]
    } else {
      break
    }
  }
  return(ind_find$index_new)
}

#' Get unique pixels within a buffer of a list of other pixels
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

buffer.pixels <- function(row_left, row_right, col_left, col_right, pixel_df, rast) {
  
}
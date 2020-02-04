#' Aggregate population to friction surface raster
#' \code{aggregate.pop} takes the pop raster and aggregates it to the friction surface. Any pop grid
#' grid cells with no matching friction surface cell get assigned to closest cell with a match. 
#' Filters by admin id and aggregates
#' @param friction_pixels friction surface converted to a spatial pixels data.frame
#' @param pop_pixels population raster converted to a spatial pixels data.frame
#' @param pop_rast the original population raster
#' @param chunk_size how you want to split for parallelization, default = 500
#' @param nmoves how many cells you want to look around to find where to assign pop cells that have
#' no matching friction surface values
#' @return Population raster resampled to the friction raster at ~ 1x1 km scale
#' @section Dependencies:
#'     iterators, data.table, raster, sp, foreach
#'     find.nonNA (user defined function in this file for finding closest cell with a match to the 
#'     friction surface)

aggregate.pop <- function(fric_pix, pop_pix, pop_rast, chunksize = 500) {

  ## Then parallelize by splitting pixel df and aggregating
  fric_pix$splitter <- floor(1:length(fric_pix)/chunksize)
  iters <- max(fric_pix$splitter)
  pop_pix$cell_id <- over(pop_pix, fric_pix)$cell_id
  
  dist_pops <- foreach(i = 0:iters, .packages = c("raster", "sp")) %dopar% {
    friction <- fric_pix[fric_pix$splitter %in% i, ] # filter to subset
    pop <- pop_pix[pop_pix$cell_id %in% friction$cell_id, ]
    resampled <- aggregate(pop["pop"], friction, function(x) sum(x, na.rm = TRUE)) # aggregate
    pop1x1 <- raster(resampled["pop"]) # transform back to raster
  }
  
  ## Merge all rasters from foreach loop
  pop1x1 <- do.call(raster::merge, dist_pops)
  return(pop1x1)
}

#' Create temporary pop pixels with matched friction ids + reallocated pops
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions
pop_to_pixels <- function(friction_pixels, pop_raster, nmoves = 10){
  
  ## Convert to spatial pixels
  pop_pixels <- as(pop_raster, "SpatialPixelsDataFrame") 
  names(pop_pixels) <- "pop"
  
  ## Get cell id for corresponding fric surface
  pop_pixels$cell_id <- over(pop_pixels, friction_pixels)$cell_id
  
  ## First find a match for any pop that is not covered by a friction grid cell
  tomatch <- pop_pixels[which(is.na(pop_pixels$cell_id) & !is.na(pop_pixels$pop)), ]
  opts <- pop_pixels[which(!is.na(pop_pixels$cell_id) & !is.na(pop_pixels$pop)), ]
  match_index <- find.nonNA(rast = pop_raster, match_inds = tomatch@grid.index,
                            opt_inds = opts@grid.index, nmoves = nmoves)
  ind_add <- data.table(pop = tomatch$pop, match_index)
  ind_add <- ind_add[ , .(pop = sum(pop)), by = "match_index"]

  ## Move from index to closest matching 
  to_add <- ind_add$pop[match(pop_pixels@grid.index, ind_add$match_index)]
  to_add[is.na(to_add)] <- 0
  pop_pixels$pop <- pop_pixels$pop + to_add
  
  return(pop_pixels)
}

#' Find nearest matching cells
#' \code{find.nonNA} takes a vector of raster indexes (the cell id) and finds the closest cell in
#'  a second 
#' vector of indexes. Uses the rowColFromCell and cellFromRowCol functions in the raster package
#' to search outward from the origin cells up to a certain # of cells away (nmoves). The function
#' stops either when all indexes have been matched or when the maximum number of cells away have been
#' searched.
#' @param rast
#' @param match_inds origin indexes to search around for closest match
#' @param opt_inds index options where the match could be found
#' @param nmoves the maximum number of cells away to look for the match
#' @return index of closest matching cells from opt_inds
#' @section Dependencies:
#'     data.table, raster

find.nonNA <- function(rast, match_inds, opt_inds, nmoves) {
  
  ind_find <- data.table(rowColFromCell(rast, match_inds), 
                         index = match_inds)
  ## set match variables
  ind_find$matched <- FALSE 
  ind_find$index_new <- ind_find$index
  
  ## get dt for where to look at sequentially
  x_move <- seq(1, nmoves, by = 1)
  y_move <- seq(1, nmoves, by = 1)
  ways_to_move <- rbind(data.table(x = x_move, y = y_move),
                            data.table(x = x_move, y = y_move - 1),
                            data.table(x = x_move - 1, y = y_move))
  ways_to_move <- rbind(ways_to_move, data.table(x = -ways_to_move$x, y = ways_to_move$y), 
                        data.table(x = ways_to_move$x, y = -ways_to_move$y),
                        data.table(x = -ways_to_move$x, y = -ways_to_move$y))
  ways_to_move[, c("order_x", "order_y") := list(abs(x), abs(y))]
  setorder(ways_to_move, order_x, order_y)
  
  ## loop through until a match found or all possible cells have been searched
  for(i in 1:nrow(ways_to_move)) {
    move <- ways_to_move[i, ]
    if(length(ind_find$matched[ind_find$matched == FALSE]) > 0){
      ind_find[, c("index_new",
                 "matched") := list(fifelse(matched %in% FALSE, 
                                           cellFromRowCol(rast, row + move$x, col + move$y),
                                           index_new),
                                    fifelse(matched %in% FALSE,
                                           cellFromRowCol(rast, row + move$x, col + move$y) %in%
                                             opt_inds, matched))]
    } else {
      break
    }
  }
  if(length(ind_find$matched[ind_find$matched == FALSE]) > 0) print("Warning: not all matched!")
  return(ind_find$index_new)
}

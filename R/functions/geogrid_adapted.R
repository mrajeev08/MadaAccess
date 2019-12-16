#' Calculate grid from spatial polygons.
#'
#' Given an input multipolgyon spatial data frame this function calculates a hexagonal or regular grid that strives to preserve the original geography.
#' @param shape A 'SpatialPolygonsDataFrame' or an sf object representing the original spatial polygons.
#' @param learning_rate The rate at which the gradient descent finds the optimum cellsize to ensure that your gridded points fit within the outer boundary of the input polygons.
#' @param grid_type Either 'hexagonal' for a hexagonal grid (default) or 'regular' for a regular grid.
#' @param seed An optional random seed integer to be used for the grid calculation algorithm.
#' @param verbose A logical indicating whether messages should be printed as the algorithm iterates.
#' @importFrom sp spsample HexPoints2SpatialPolygons SpatialPixels
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' @export
#' @examples
#' library(sf)
#' input_file <- system.file('extdata', 'london_LA.json', package = 'geogrid')
#' original_shapes <- st_read(input_file) %>% st_set_crs(27700)
#'
#' # calculate grid
#' new_cells <- calculate_grid(shape = original_shapes,
#'   grid_type = 'hexagonal', seed = 1)
#' grid_shapes <- assign_polygons(original_shapes, new_cells)
#' plot(grid_shapes)
#'
#' par(mfrow = c(1, 2))
#' plot(st_geometry(original_shapes))
#' plot(st_geometry(grid_shapes))
#'
#' \dontrun{
#' # look at different grids using different seeds
#' par(mfrow=c(2, 3), mar = c(0, 0, 2, 0))
#' for (i in 1:6) {
#'   new_cells <- calculate_grid(shape = original_shapes, grid_type = 'hexagonal', seed = i)
#'   plot(new_cells, main = paste('Seed', i, sep=' '))
#' }
#' }
calculate_grid <- function(shape, learning_rate = 0.03, grid_type = c("hexagonal",
                                                                      "regular"), 
                           seed = NULL, npts = NULL, verbose = FALSE) {
  UseMethod("calculate_grid")
}

#' @rdname calculate_grid
#' @export
calculate_grid.SpatialPolygonsDataFrame <- function(shape, learning_rate = 0.03,
                                                    grid_type = c("hexagonal", "regular"), 
                                                    seed = NULL, npts = NULL, verbose = FALSE) {
  
  if (!is.null(seed))
    set.seed(seed)
  # = c('regular', 'hexagonal') check that regular and hexagon dont return
  # different lists of points (list and list[[]] respectively?)
  
  shape_details <- get_shape_details_internal(shape)
  
  grid_type <- match.arg(grid_type)
  
  if (!inherits(shape_details, "shape_details"))
    stop("'shape_details' must be an object obtained ", "from calling get_shape_details().")
  
  # Lets find some bounds for the optimisation that make sense.  max_allowed_area
  # <- shape_details$total_area / shape_details$nhex hexagon_diam <-
  # sqrt(max_allowed_area / 2.598076) * 2
  
  cellsize <- shape_details$start_size
  
  if(!is.null(npts)) {
    shape_details$nhex <- npts
  }
  
  repeat {
    hex_pts <- sp::spsample(shape, type = grid_type, cellsize = cellsize, iter = 10000)
    npolygons <- length(hex_pts)
    if (verbose) {
      message(npolygons)
      message(cellsize)
    }
    
    if (npolygons == shape_details$nhex) {
      break
    } else if (npolygons > shape_details$nhex) {
      if (verbose)
        message("too many polygons")
      cellsize_new <- cellsize * (1 + learning_rate)
      cellsize <- cellsize_new
    } else {
      # else (npolygons < shape_details$nhex)
      if (verbose)
        message("too few polygons")
      cellsize_new <- cellsize * (1 - learning_rate)
      cellsize <- cellsize_new
    }
  }
  
  if (verbose)
    message("The cellsize is ", cellsize)
  
  if (grid_type == "hexagonal") {
    pols <- sp::HexPoints2SpatialPolygons(hex_pts)
  } else {
    pols <- sp::SpatialPixels(hex_pts)
    pols <- methods::as(pols, "SpatialPolygons")
  }
  # or spatial polygons? need to turn this into same object as hexagons above try
  # making dataframe and going that route. need correct ids for match between then
  # and now note <- cellsize could be unsolveable. Add rotation of grid if needed.
  
  res <- list(hex_pts, pols)
  class(res) <- c("geogrid", "list")
  
  return(res)
}

#' Extract details from provided polygons.
#'
#' Extract spatial extent, range and other geospatial features from the output of read_polygons. Items are returned as a list for use in \code{\link{calculate_grid}}.
#'
#' @param input_shape A "SpatialPolygonsDataFrame" object representing the original spatial polygons.
get_shape_details_internal <- function(input_shape) {
  
  nhex <- length(input_shape)
  
  if (nhex < 4)
    message("Your shape has fewer than 5 polygons. ",
            "Please be aware that a geogrid may have limited value.")
  
  # Start off with guidance but start with bins that are too large
  # (cellsize too large)
  # shape_summary <- input_shape@bbox
  
  #xmax <- shape_summary[2][[1]][1, 2]
  xmax <- input_shape@bbox[3]
  #ymax <- shape_summary[2][[1]][2, 2]
  ymax <- input_shape@bbox[4]
  #xmin <- shape_summary[2][[1]][1, 1]
  xmin <- input_shape@bbox[1]
  #ymin <- shape_summary[2][[1]][2, 1]
  ymin <- input_shape@bbox[2]
  xrange <- (xmax - xmin)
  yrange <- (ymax - ymin)
  start_width <- ifelse(xrange > yrange, xrange, yrange)
  
  # Let's assume that the user want's something more than 4 hexagons wide
  # or long. If they want something this small then a geogrid is probably
  # not worth it.
  start_size <- start_width / 100
  total_area <- input_shape@polygons[[1]]@Polygons[[1]]@area
  
  shape_details <- list(nhex = nhex, xmax = xmax, ymax = ymax,
                        xmin = xmin, ymin = ymin, xrange = xrange, yrange = yrange,
                        start_size = start_size, total_area = total_area)
  class(shape_details) <- c("shape_details", "list")
  
  return(shape_details)
}

#' Extract details from provided polygons (deprecated).
#'
#' Extract spatial extent, range and other geospatial features from the output of read_polygons. Items are returned as a list for use in \code{\link{calculate_grid}}.
#'
#' @param input_shape A "SpatialPolygonsDataFrame" object representing the original spatial polygons.
#' @export
get_shape_details <- function(input_shape) {
  stop("get_shape_details() has been deprecated. ",
       "It is now handled automatically in calculate_grid().",
       call. = FALSE)
}

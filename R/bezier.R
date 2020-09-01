#' Title
#' Description
#' Details
#'
#' @param from 
#' @param to 
#' @param frac 
#' @param transform 
#'
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions
#' Bezier curve control points 
#' Details: adapted from Dudas et al. Curonia 
#' https://github.com/evogytis/baltic/blob/master/curonia.ipynb
#' Output = data.frame with to/mid/from
get.bezier.pts <- function(from, to, frac = 0.8, transform = function(x) sqrt(1/x)*0.5) {
  
  sign <- ifelse(from[, "long"] > to[, "long"], -1, 1)
  slope <- (to[, "lat"] - from[, "lat"]) / (to[, "long"] - from[, "long"])
  distance <- sqrt((to[, "lat"] - from[, "lat"])^2 + (to[, "long"] - from[, "long"])^2) 
  height <- transform(distance)
  
  hdist <- sqrt(height^2 + (distance*frac)^2) # distance between desired height and point along line

  from <-  data.frame(long = from[, "long"], lat = from[, "lat"], index = 1, group = 1:nrow(from))
  ctrl <- data.frame(long = from[, "long"] + hdist*cos(atan(height/distance/frac) + atan(slope))*sign,
                     lat = from[, "lat"] + hdist*sin(atan(height/distance/frac) + atan(slope))*sign, 
                     index = 2, group = 1:nrow(from)) # the magic control point
  to <- data.frame(long = to[, "long"], lat = to[, "lat"], index = 3, group = 1:nrow(from))
  
  df <- do.call(rbind, list(from, ctrl, to))
  df %>%
    group_by(group) %>%
    arrange(index) -> df
  return(df) # return coordinate df sorted
}


#' Title
#' Description
#' Details
#' bbox is from st_bbox
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions
#'     
get.bezleg <- function(bbox, n_pts, size_vec, min_size, offset_long, offset_lat) {
  
  long_min <- bbox$xmin + offset_long # pulls it in from bbox edge
  long_max <- bbox$xmax - offset_long # pulls it in from bbox edge
  lat_pt<- bbox$ymin + offset_lat # pulls it down away from Mada 
  
  long <- seq(long_min, long_max, length.out = n_pts)
  
  long_to <- long[2:(floor(n_pts/2))]
  lat_to <- rep(lat_pt, length(long_to))
  
  long_from <- rev(long[(floor(n_pts/2) + 2):(n_pts - 1)])
  lat_from <- rep(lat_pt, length(long_from))
  
  bez_pts <- get.bezier.pts(from = data.frame(long = long_from, lat = lat_from),
                                to = data.frame(long = long_to, lat = lat_to), 
                                frac = 0.5, transform = function(x) x/2)
  
  size_df <- data.frame(sizes = rev(size_vec), group = 1:length(long_from), type = "bez")
  bez_pts <- left_join(bez_pts, size_df, by = c("group" = "group"))
  min_pt <- data.frame(long = median(long), lat = lat_pt, index = 1, sizes = min_size,
                       group = length(long_from) + 1, type = "min")
  
  all_pts <- bind_rows(bez_pts, min_pt)
  
  return(all_pts)
}

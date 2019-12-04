####################################################################################################
##' Bezier curve control points 
##' Details: adapted from Dudas et al. Curonia 
##' https://github.com/evogytis/baltic/blob/master/curonia.ipynb
##' Author: Malavika Rajeev 
####################################################################################################
get.bezier.pts <- function(origin, destination, frac, transform) {
  
  sign <- ifelse(origin[, "x"] > destination[, "x"], -1, 1)
  slope <- (destination[, "y"] - origin[, "y"]) / (destination[, "x"] - origin[, "x"])
  distance <- sqrt((destination[, "y"] - origin[, "y"])^2 + (destination[, "x"] - origin[, "x"])^2) 
  height <- transform(distance)
  
  hdist <- sqrt(height^2 + (distance*frac)^2) ## distance between desired height and point along line
  
  out_x <- origin[, "x"] + hdist*cos(atan(height/distance/frac) + atan(slope))*sign ## magic
  out_y <- origin[, "y"] + hdist*sin(atan(height/distance/frac) + atan(slope))*sign
  
  return (cbind(out_x, out_y)) ## return third point's coordinate
}



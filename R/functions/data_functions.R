####################################################################################################
##' Functions for data wrangling/processing 
##' Details: for getting days to include, reporting, etc. 
##' Author: Malavika Rajeev 
####################################################################################################

##' Function for getting days to include via RLEs 
##' ------------------------------------------------------------------------------------------------
rle.days <- function(vec, threshold = 10) {
  
  rle(vec) %>%
    unclass() %>%
    as.data.frame() %>%
    mutate(end = cumsum(lengths),
    start = c(1, lag(end)[-1] + 1),
    include = ifelse(values == 0 &
    lengths >= threshold, 0, 1)) -> rles
  
  include <- rep(NA, length(vec))
  
  for(i in 1:nrow(rles)) {
    include[rles$start[i]:rles$end[i]] <- rles$include[i]
  }
  
  return(include)
}

## Function for getting bite estimates?


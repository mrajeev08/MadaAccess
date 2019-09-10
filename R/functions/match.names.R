####################################################################################################
##' Function for matching names between data and shapefiles 
##' Details: Match within districts for communes and output ranked matches to manually fix 
##' Author: Malavika Rajeev 
####################################################################################################

#' Matching names in data to key
#' 
#' \code{match.admin} ranks potential matches for a character vector
#' 
#' Can be used with a nesting category (i.e. matching commune names within each district name)
#' or for matching names in general. Provide vectors of names in data (data_names) to a vector of names
#' to match to (match_names) and a vector of shared nesting names (data_nest and match_nest). 
#' The match_method comes from stringdistmatrix. Uses exact and then partial matching. If no matches below
#' a certain threshold, then uses partial. 
#' 
#' @param data_names vector of names in data that need to be matched
#' @param data_nest vector of corresponding nesting factor for the names in the data 
#' @param match_names vector of potential matches 
#' @param match_nest vector of corresponding nesting factor for the potential matches
#' @param match_threshold numeric, the threshold above which partial matching is used
#' @param match_method  character string either "osa" "lv" or "dl. see Details. 
#' @param nested
#' 
#' @return Dataframe with minimum distance and matches returned in columns ranked by distance 
#' @section Dependencies: stringdist, dplyr

match.admin <- function(data_names, data_nest, match_names, match_nest, 
                        match_threshold, match_method, nested = TRUE) {
  
  require(stringdist)
  
  ## trouble shooting
  # match_threshold = 5
  # match_method = "osa"
  # 
  # ## For nested = TRUE
  # # data_names = as.character(peripheral$commune)
  # # data_nest = as.character(peripheral$distcode)
  # # match_names = as.character(mada_communes$ADM3_EN)
  # # match_nest = as.character(mada_communes$distcode)
  # # nested = TRUE
  # 
  # ## Test for nested = FALSE
  # data_names = as.character(IPM$fiv)
  # data_nest = NULL
  # match_names = as.character(mada_districts$ADM2_EN)
  # match_nest = NULL
  # nested = FALSE
  
  ## Make sure they are character vectors
  if (class(data_names) != "character") data_names = as.character(data_names)
  if (class(match_names) != "character") match_names = as.character(match_names)
  
  ## If nested rank matches within nested factor
  if (nested == TRUE) {
    
    if (class(data_nest) != "character") data_nest = as.character(data_nest)
    if (class(match_nest) != "character") data_nest = as.character(match_nest)
    
    unique_nests <- unique(match_nest)
    match_df <- NULL
    
    for (i in 1:length(unique_nests)){
    
      ## make sure lower case and unique
      matches <- tolower(match_names[match_nest == unique_nests[i]])
      tomatch <- unique(tolower(data_names[data_nest == unique_nests[i]]))
      
      ## should have some tomatch in each nest and also more than one potential match
      if(length(tomatch) > 0 & length(matches) > 1) {

        ## do simple lev. distances with stringdistmatrix with a match threshold
        distance <- stringdistmatrix(tomatch, matches, method = match_method)
        ranked_matches <- t(apply(distance, 1, function(x) matches[order(x)]))
        min_dist <- apply(distance, 1, min)
        
        partial <- adist(tomatch, matches, partial = TRUE)
        min_part <- apply(partial, 1, min)
        partial_matches <- t(apply(partial, 1, function(x) matches[order(x)]))
        
        ## only keep if the partial matches if it meets the following conditions
        ## 1. it is less than the length of the name to match (- 1)
        ## 2. it is less than the minimum of the lev. distances
        inds_to_rep <- which(
          min_part <= min_dist & min_part < nchar(tomatch) - 1)
        ranked_matches[inds_to_rep, ] <-  partial_matches[inds_to_rep, ]      
        min_dist[inds_to_rep] <- min_part[inds_to_rep]
        
        ## make df
        df <- data.frame(list(names_tomatch = tomatch, nest = unique_nests[i], min = min_dist, 
                              ranked_matches))
      }
      
      if(length(matches) == 1) {
        df <- data.frame(list(names_tomatch = tomatch, nest = unique_nests[i],
                                min = NA, match = matches))
      }
      match_df <- bind_rows(match_df, df)
    }
  } else {
    
    tomatch <- unique(tolower(data_names))
    matches <- tolower(match_names)
    
    ## do simple lev. distances with stringdistmatrix with a match threshold
    distance <- stringdistmatrix(tomatch, matches, method = match_method)
    ranked_matches <- t(apply(distance, 1, function(x) matches[order(x)]))
    min_dist <- apply(distance, 1, min)
    
    partial <- adist(tomatch, matches, partial = TRUE)
    min_part <- apply(partial, 1, min)
    partial_matches <- t(apply(partial, 1, function(x) matches[order(x)]))
    
    ## only keep if the partial matches if it meets the following conditions
    ## 1. it is less than the length of the name to match (- 1)
    ## 2. it is less than the minimum of the lev. distances
    inds_to_rep <- which(
      min_part <= min_dist & min_part < nchar(tomatch) - 1)
    ranked_matches[inds_to_rep, ] <-  partial_matches[inds_to_rep, ]      
    min_dist[inds_to_rep] <- min_part[inds_to_rep]
    
    match_df <- data.frame(list(names_tomatch = tomatch, min = min_dist, ranked_matches))
  }
  return(match_df)
}



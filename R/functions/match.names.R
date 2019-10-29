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
#' The match_method comes from stringdistmatrix. Uses exact and then partial matching. 
#' 
#' @param data_names vector of names in data that need to be matched
#' @param data_nest vector of corresponding nesting factor for the names in the data 
#' @param match_names vector of potential matches 
#' @param match_nest vector of corresponding nesting factor for the potential matches
#' @param match_method  character string either "osa" "lv" or "dl. see Details. 
#' @param nested
#' 
#' @return Dataframe with minimum distance and best match for fixed and partial matching and 
#' the ranked fixed matches returned in columns (ranked by distance)
#' @section Dependencies: stringdist, dplyr

match.admin <- function(data_names, data_nest, match_names, match_nest, match_method, nested = TRUE) {
  
  require(stringdist)
  
  ## trouble shooting
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
    if (class(match_nest) != "character") match_nest = as.character(match_nest)
    
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
        
        ## also partial distances 
        partial <- adist(tomatch, matches, partial = TRUE)
        min_part <- apply(partial, 1, min)
        partial_matches <- t(apply(partial, 1, function(x) matches[order(x)]))
        
        ## make df
        df <- data.frame(list(names_tomatch = tomatch, nest = unique_nests[i],
                              fixed_best = ranked_matches[, 1], min_fixed = min_dist,
                              partial_best = partial_matches[, 1], min_partial = min_part, 
                              ranked_matches))
      }
      
      if(length(matches) == 1) {
        df <- data.frame(list(names_tomatch = tomatch, nest = unique_nests[i],
                              fixed_best = matches, min_fixed = 0,
                              partial_best = matches, min_partial = 0))
      }
      
      match_df <- bind_rows(match_df, df)
    }
  } else {
    
    tomatch <- unique(tolower(data_names))
    matches <- tolower(match_names)
    
    ## do simple lev. distances with stringdistmatrix
    distance <- stringdistmatrix(tomatch, matches, method = match_method)
    ranked_matches <- t(apply(distance, 1, function(x) matches[order(x)]))
    min_dist <- apply(distance, 1, min)
    
    ## then partial matching
    partial <- adist(tomatch, matches, partial = TRUE)
    min_part <- apply(partial, 1, min)
    partial_matches <- t(apply(partial, 1, function(x) matches[order(x)]))
  
    ## make df
    match_df <- data.frame(list(names_tomatch = tomatch, 
                          fixed_best = ranked_matches[, 1], min_fixed = min_dist,
                          partial_best = partial_matches[, 1], min_partial = min_part, 
                          ranked_matches))  
  }
  return(match_df)
}



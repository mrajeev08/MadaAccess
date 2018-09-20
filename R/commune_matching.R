# script for generating ranked name mismatches
# Still to do:
# match shapefile names (maybe just use one shapefile...or codes?)
# replace fr cardinal directions (i.e. sud, nord, etc.) w/ mg (i.e. atsimo, antsinanana, etc.) for both
# generate csv file to manually match for ones that can't be checked
# turn into a function?

## libraries
rm(list = ls())
setwd("~/Dropbox/MadaPEP")
library(tidyverse)
library(raster)
library(maptools)
library(maps)
library(GISTools)
library(rgdal)
library(sp)
library(rgdal)
library(gdistance)
library(glue) # for printing results
library(stringdist)

## commune level shapefile
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
ctar_data <- read.csv("data/SaisieRage_DATA_2018-06-28_0956.csv", encoding="UTF-8-MAC")

# need the following
head(ctar_data)
head(mada_communes@data)
mada_communes$match_dist <- substr(mada_communes$mdg_com_co, 1, 8)
filter <- unique(mada_communes@data$match_dist)
match.meth = "osa" # from stringdist-metrics
threshold = 4 # threshold for distance that's acceptable before partial matching
commune_match <- NULL
checks <- "district names to check:"
missing = 0

## need to do it this way because you have to get unique commune names within each district!
for (i in 1:length(filter)){
    loc_names <- unique(ctar_data$commune[which(ctar_data$district== filter[i])])
    loc_names <- tolower(str_replace_all(loc_names,"[^[:graph:]]", "/"))
    loc_names <- loc_names[!is.na(loc_names)]
    loc_matches <- mada_communes$commune[which(mada_communes$match_dist == filter[i])]
    loc_matches <- tolower(str_replace_all(loc_matches,"[^[:graph:]]", "/"))
    loc_matches <- loc_matches[!is.na(loc_matches)]
    
    if (length(loc_names) == 0) {
      print(glue("{filter[i]} is not in data"))
      checks <- glue("{checks} {filter[i]},")
      missing <- missing + 1
      next
    }
    
    else {
      print(glue("attempting to match {filter[i]}"))
      df <- as.data.frame (matrix(NA, nrow = length(loc_names), ncol = (length(loc_matches) + 1)))
      df[,1] <- loc_names
      colnames(df)[1] <- "name"
  
      
      for (j in 1:length(loc_matches)){
        colnames(df)[(j + 1)] <- glue("match{j}")
      }
      
      df$min_dist <- NA
      
      for (k in 1:length(loc_names)){
        
        ## do simple lev. distances with stringdistmatrix with a match threshold
        distance <- stringdistmatrix(loc_names[k], loc_matches, method = match.meth)
        
        if (min(distance, na.rm = TRUE) <= threshold) {
          df[k, c(2:(length(distance)+1))] <-  loc_matches[order(distance)]
          df$min_dist[k] <- min(distance, na.rm = TRUE)
        }
        
        ## else do partial matching
        # check both ways
        # also get min dist within this
        else{
          partial <- adist(loc_names[k], loc_matches, partial = TRUE)
          
          if (min(distance, na.rm = TRUE) < min(partial, na.rm = TRUE)){
            df[k, c(2:(length(distance)+1))] <-  loc_matches[order(distance)]
            df$min_dist[k] <- min(distance, na.rm = TRUE)
          }
          else{
            df[k, c(2:(length(partial)+1))] <-  loc_matches[order(partial)]
            df$min_dist[k] <- min(partial, na.rm = TRUE)
          }
        }
      }
      df$district <- filter[i]
      df <- df[,c(ncol(df), 1, (ncol(df)-1), 2:(ncol(df)-2))]
      commune_match <- bind_rows(commune_match, mutate_all(df, as.character))
    }
    print(glue("{i} out of {length(filter)} districts"))
}

print(checks)
missing
write.csv(commune_match, "commune_check.csv")

####################################################################################################
##' Step 4: Assigning catchment to each admin unit based on travel times
##' Details: Uses catchment matrixes generated in step 03 to select the catchment
##'   Does not need to be run in parallel
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

##' Libraries/source scripts 
##' ------------------------------------------------------------------------------------------------
library(geosphere)
library(raster)
library(rgdal)
library(tidyverse)

##' Read in files 
##' ------------------------------------------------------------------------------------------------
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
district_ttimes_weighted <- read.csv("data/processed/catchmats/baseline_district_ttimes_weighted.csv",
                                     row.names = 1)
district_distance_weighted <- read.csv("data/processed/catchmats/baseline_district_distance_weighted.csv",
                                       row.names = 1)
commune_ttimes_weighted <- read.csv("data/processed/catchmats/baseline_commune_ttimes_weighted.csv",
                                    row.names = 1)
commune_distance_weighted <- read.csv("data/processed/catchmats/baseline_commune_distance_weighted.csv",
                                      row.names = 1)
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")

##' Get travel times and catchments at district and commune level
##' ------------------------------------------------------------------------------------------------
##' Var names have to be <= 10 characters long for ESRI shapefile output
##' Quick function for getting catchments accounting for Inf returns
which.min.inf  <- function(x) {
  if(min(x) == Inf | is.na(min(x))) {
    return(NA)
  } else {
    return(which.min(x))
  }
}
##' Communes ttimes and distance
mada_communes$ttms_wtd <- apply(commune_ttimes_weighted, 1, min, na.rm = TRUE)
mada_communes$dist_wtd <- apply(commune_distance_weighted, 1, min, na.rm = TRUE)
mada_communes$ctch_ttwtd <- ctar_metadata$CTAR[unlist(apply(commune_ttimes_weighted, 
                                                                           1, which.min.inf))]
mada_communes$ctch_dswtd <- ctar_metadata$CTAR[unlist(apply(commune_distance_weighted, 
                                                                             1, which.min.inf))]
##' District
mada_districts$ttms_wtd <- apply(district_ttimes_weighted, 1, min, na.rm = TRUE)
mada_districts$dist_wtd <- apply(district_distance_weighted, 1, min, na.rm = TRUE)
mada_districts$ctch_ttwtd <- ctar_metadata$CTAR[unlist(apply(district_ttimes_weighted, 
                                                          1, which.min.inf))]
mada_districts$ctch_dswtd <- ctar_metadata$CTAR[unlist(apply(district_distance_weighted, 
                                                            1, which.min.inf))]

##' Get distance to closest CTAR based on centroids
##' ------------------------------------------------------------------------------------------------
##' Districts
mada_district_coords <- coordinates(mada_districts)
mada_districts$long <- mada_district_coords[, 1]
mada_districts$lat <- mada_district_coords[, 2]
dist_distance_mat <- distm(mada_district_coords, ctar_metadata[, c("LONGITUDE", "LATITUDE")])/1000
mada_districts$dist_cent <- apply(dist_distance_mat, 1, min, na.rm = TRUE)
mada_districts$ctch_dsct <- ctar_metadata$CTAR[unlist(apply(dist_distance_mat, 
                                                                 1, which.min.inf))]

##' Communes
mada_commune_coords <- coordinates(mada_communes)
mada_communes$long <- mada_commune_coords[, 1]
mada_communes$lat <- mada_commune_coords[, 2]
comm_distance_mat <- distm(mada_commune_coords, ctar_metadata[, c("LONGITUDE", "LATITUDE")])/1000
mada_communes$dist_cent <- apply(comm_distance_mat, 1, min, na.rm = TRUE)
mada_communes$ctch_dsct <- ctar_metadata$CTAR[unlist(apply(comm_distance_mat, 
                                                                         1, which.min.inf))]

##' Write out the shapefiles to processed/shapefiles/ (overwrite)
##' ------------------------------------------------------------------------------------------------
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)


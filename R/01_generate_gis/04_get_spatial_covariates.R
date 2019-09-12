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
extract <- raster::extract

##' Read in files 
##' ------------------------------------------------------------------------------------------------
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
dist_mat_masked <- read.csv("data/processed/catchmats/dist_mat_masked.csv")
dist_mat_unmasked <- read.csv("data/processed/catchmats/dist_mat_unmasked.csv")
comm_mat_masked <- read.csv("data/processed/catchmats/comm_mat_masked.csv")
comm_mat_unmasked <- read.csv("data/processed/catchmats/comm_mat_unmasked.csv")
ctar_points <- read.csv(file = "data/raw/ctar_metadata.csv")[, c("CTAR", "LATITUDE", "LONGITUDE")]

##' Format distcodes 
##' ------------------------------------------------------------------------------------------------
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

##' Extract Pop 1x1 km at district and commune level
##' ------------------------------------------------------------------------------------------------
mada_communes$pop <- extract(pop1x1, mada_communes, fun = sum, small = TRUE, na.rm = TRUE)[, 1]
mada_districts$pop <- extract(pop1x1, mada_districts, fun = sum, small = TRUE, na.rm = TRUE)[, 1]

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
##' Communes
mada_communes$ttms_wtd_m <- apply(comm_mat_masked, 1, min, na.rm = TRUE)
mada_communes$ttms_wtd_un <- apply(comm_mat_unmasked, 1, min, na.rm = TRUE)
mada_communes$ctch_wtd_m <- ctar_points$CTAR[unlist(apply(comm_mat_masked, 
                                                                           1, which.min.inf))]
mada_communes$ctch_wtd_un <- ctar_points$CTAR[unlist(apply(comm_mat_masked, 
                                                                             1, which.min.inf))]

##' District
mada_districts$ttms_wtd_m <- apply(dist_mat_masked, 1, min, na.rm = TRUE)
mada_districts$ttms_wtd_un <- apply(dist_mat_unmasked, 1, min, na.rm = TRUE)
mada_districts$ctch_wtd_m <- ctar_points$CTAR[unlist(apply(dist_mat_masked, 
                                                                            1, which.min.inf))]
mada_districts$ctch_wtd_un <- ctar_points$CTAR[unlist(apply(dist_mat_unmasked, 
                                                                           1, which.min.inf))]
 
##' Get distance to closest CTAR
##' ------------------------------------------------------------------------------------------------
##' Districts
mada_district_coords <- coordinates(mada_districts)
mada_districts$long <- mada_district_coords[, 1]
mada_districts$lat <- mada_district_coords[, 2]
dist_distance_mat <- distm(mada_district_coords, select(ctar_points, LONGITUDE, LATITUDE))/1000
mada_districts$mindist <- apply(dist_distance_mat, 1, min, na.rm = TRUE)
mada_districts$ctch_dist <- ctar_points$CTAR[unlist(apply(dist_distance_mat, 
                                                                 1, which.min.inf))]

##' Communes
mada_commune_coords <- coordinates(mada_communes)
mada_communes$long <- mada_commune_coords[, 1]
mada_communes$lat <- mada_commune_coords[, 2]
comm_distance_mat <- distm(mada_commune_coords, select(ctar_points, LONGITUDE, LATITUDE))/1000
mada_communes$mindist <- apply(comm_distance_mat, 1, min, na.rm = TRUE)
mada_communes$ctch_dist <- ctar_points$CTAR[unlist(apply(comm_distance_mat, 
                                                                         1, which.min.inf))]

##' Write out the shapefiles to processed/shapefiles/
##' ------------------------------------------------------------------------------------------------
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)


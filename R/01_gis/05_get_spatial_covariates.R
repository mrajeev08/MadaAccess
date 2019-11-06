####################################################################################################
##'Assigning catchment to each admin unit based on travel times
##' Details: Uses data frames generated in step 04
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
library(data.table)

##' Read in files 
##' ------------------------------------------------------------------------------------------------
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")
baseline_df <- fread("output/ttimes/baseline_grid.csv")

##' Get travel times and catchments at district and commune level
##' ------------------------------------------------------------------------------------------------
district_df <- fread("output/ttimes/baseline_district.csv")
district_df <- district_df[scenario == 31]
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], by = district_id]
## Fix so that is base catch is 0, then NA (this means that Inf returned for all clinics)
district_df[, weighted_times := ifelse(base_catches == 0, NA, weighted_times)]

## Match district_id with row number in mada_districts@data
setorder(district_df, district_id)
if (nrow(district_df) == nrow(mada_districts@data)) {
  mada_districts$ttimes <- district_df$weighted_times
  mada_districts$catch <- district_df$base_catches
  mada_districts$prop_pop <- district_df$prop_pop_catch
}

## Communes
commune_df <- fread("output/ttimes/baseline_commune.csv")
commune_df <- commune_df[scenario == 31]
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], by = commune_id]
## Fix so that is base catch is 0, then NA (this means that Inf returned for all clinics)
commune_df[, weighted_times := ifelse(base_catches == 0, NA, weighted_times)]

## Match commune_id with row number in mada_communes@data
setorder(commune_df, commune_id)
if (nrow(commune_df) == nrow(mada_communes@data)) {
  mada_communes$ttimes <- commune_df$weighted_times
  mada_communes$catch <- commune_df$base_catches
  mada_communes$prop_pop <- commune_df$prop_pop_catch
}

##' Get distance to closest CTAR based on centroids
##' ------------------------------------------------------------------------------------------------
##' Districts
mada_districts$long <- coordinates(mada_districts)[, 1]
mada_districts$lat <- coordinates(mada_districts)[, 2]

##' Communes
mada_communes$long <- coordinates(mada_communes)[, 1]
mada_communes$lat <-  coordinates(mada_communes)[, 2]

##' Clean up the shapefile attribute data
##' ------------------------------------------------------------------------------------------------
##' Var names have to be <= 10 characters long for ESRI shapefile output
##' Districts
mada_districts@data %>%
  dplyr::select(distcode, pop, long, lat, ttimes, catch, prop_pop) -> mada_districts@data

##' Communes
mada_communes@data %>%
  dplyr::select(distcode, district = ADM2_EN, commcode = ADM3_PCODE, commune = ADM3_EN, pop, 
                long, lat, ttimes, catch, prop_pop) -> mada_communes@data

##' Write out the shapefiles to processed/shapefiles/ (overwrite)
##' ------------------------------------------------------------------------------------------------
writeOGR(mada_communes, dsn = "data/output/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/output/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################################################################################################
##' Generating shapefiles with covariates
##' Author: Malavika Rajeev 
####################################################################################################
rm(list=ls())

##' Libraries and packages
library(rgeos)
library(dplyr)
library(raster)
library(rgdal)
library(data.table)

##' Shapefiles
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")

##' Dissolving districts
##' ------------------------------------------------------------------------------------------------
##' because these are new districts and we do not have data to the arrondisement level for 
##' Tana (i.e. Antananarivo Renivohitra)
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

districts_dissolved <- gUnaryUnion(mada_districts, id = mada_districts$distcode)
districts_df <- mada_districts@data[6:nrow(mada_districts@data), ]
districts_df$ADM2_EN <- recode(districts_df$ADM2_EN, `6e Arrondissement` = "Antananarivo Renivohitra")
row.names(districts_dissolved) <- as.character(1:nrow(districts_df))
row.names(districts_df) <- as.character(1:nrow(districts_df))
mada_districts <- SpatialPolygonsDataFrame(districts_dissolved, districts_df)

##' Get travel times and catchments at district and commune level
##' ------------------------------------------------------------------------------------------------
district_df <- fread("output/ttimes/baseline_district.csv")
district_df <- district_df[scenario == 31]
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], by = district_id]
## Fix so that if base catch is 0, then NA (this means that Inf returned for all clinics)
## Also match the clinic catchment name
district_df[, c('weighted_times', 'catchment') := .(ifelse(base_catches == 0, NA, weighted_times),
                                                    ifelse(base_catches == 0, NA, 
                                                           ctar_metadata$CTAR[base_catches]))]

## Match district_id with row number in mada_districts@data
setorder(district_df, district_id)
if (nrow(district_df) == nrow(mada_districts@data)) {
  mada_districts$ttimes <- district_df$weighted_times
  mada_districts$catch <- district_df$catchment
  mada_districts$prop_pop <- district_df$prop_pop_catch
  mada_districts$pop <- district_df$pop
}

## Communes
commune_df <- fread("output/ttimes/baseline_commune.csv")
commune_df <- commune_df[scenario == 31]
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], by = commune_id]
## Fix so that is base catch is 0, then NA (this means that Inf returned for all clinics)
commune_df[, c('weighted_times', 'catchment') := .(ifelse(base_catches == 0, NA, weighted_times),
                                                    ifelse(base_catches == 0, NA, 
                                                           ctar_metadata$CTAR[base_catches]))]

## Match commune_id with row number in mada_communes@data
setorder(commune_df, commune_id)
if (nrow(commune_df) == nrow(mada_communes@data)) {
  mada_communes$ttimes <- commune_df$weighted_times
  mada_communes$catch <- commune_df$catchment
  mada_communes$prop_pop <- commune_df$prop_pop_catch
  mada_communes$pop <- commune_df$pop
}

##' Get longitude and latitude
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
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)


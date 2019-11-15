####################################################################################################
##' Step 4: Generating shapefiles with covariates
##' Details: Adding pop, catchment, and travel time covariates to the shapefiles
##' Author: Malavika Rajeev 
####################################################################################################
rm(list=ls())

##' Packages
library(rgeos) # for dissolving the admin units
library(dplyr)
library(raster)
library(rgdal)
library(data.table)
source("R/functions/utils.R")

##' Shapefiles
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")

##' Dissolving districts
##' ------------------------------------------------------------------------------------------------
##' because we do not have data to the arrondisement level for 
##' Tana we want to dissolve these to be one district: Antananarivo Renivohitra
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7) # match distcodes
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

districts_dissolved <- gUnaryUnion(mada_districts, id = mada_districts$distcode)
districts_to_merge <- mada_districts@data[6:nrow(mada_districts@data), ]
districts_to_merge$ADM2_EN <- recode(districts_to_merge$ADM2_EN, `6e Arrondissement` = "Antananarivo Renivohitra")
row.names(districts_dissolved) <- as.character(1:nrow(districts_to_merge))
row.names(districts_to_merge) <- as.character(1:nrow(districts_to_merge))
mada_districts <- SpatialPolygonsDataFrame(districts_dissolved, districts_to_merge)

##' Get travel times and catchments at district and commune level
##' ------------------------------------------------------------------------------------------------
district_df <- fread("output/ttimes/baseline_district.csv")
##' Filter so that catchment is the one which is closest for the maximum proportion of the population
##' in that admin unit
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], by = district_id]
##' Match distcodes by district_id and base_catches (row ids in corresponding shapefile and ctar metadata)
district_df[, c('distcode', 'catchment', 'id_ctar') := .(mada_districts$distcode[district_id],
                                              ctar_metadata$CTAR[base_catches], 
                                              ctar_metadata$id_ctar[base_catches])]
##' Join the data
mada_districts@data <- district_df[mada_districts@data, on = "distcode"]

##' Do the same for communes
commune_df <- fread("output/ttimes/baseline_commune.csv")
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], by = commune_id]
commune_df[, c('commcode', 'catchment', 'id_ctar') := .(mada_communes$ADM3_PCODE[commune_id],
                                              ctar_metadata$CTAR[base_catches],
                                              ctar_metadata$id_ctar[base_catches])]
##' Join the data
mada_communes@data <- commune_df[mada_communes@data, on = c("commcode" = "ADM3_PCODE")]


##' Get longitude and latitude (for plotting)
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
  dplyr::select(distcode, district = ADM2_EN, pop, long, lat, ttimes_wtd = weighted_times, catchment,
                id_ctar, pop_catch = prop_pop_catch) -> mada_districts@data

##' Communes
mada_communes@data %>%
  dplyr::select(distcode, district = ADM2_EN, commcode, commune = ADM3_EN, pop, 
                long, lat, ttimes_wtd = weighted_times, catchment, id_ctar,
                pop_catch = prop_pop_catch) -> mada_communes@data

##' Write out the shapefiles to processed/shapefiles/ (overwrite)
##' ------------------------------------------------------------------------------------------------
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

##' Saving session info
out.session(path = "R/01_gis/04_make_shapefiles.R", filename = "sessionInfo.csv")

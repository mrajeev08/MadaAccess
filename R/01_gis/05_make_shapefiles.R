# ------------------------------------------------------------------------------------------------ #
#' Generating shapefiles with covariates: adding pop, catchment, and travel time covariates to the 
#' shapefiles at the commune and district level
# ------------------------------------------------------------------------------------------------ #

# Packages
library(rgeos) # for dissolving the admin units
library(dplyr)
library(raster)
library(rgdal)
library(data.table)
source("R/functions/out.session.R")

# Shapefiles
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")

# Get distcodes for both admin levels
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

# Dissolving districts for Tana to one district: Antananarivo Renivohitra -------------------------
districts_dissolved <- gUnaryUnion(mada_districts, id = mada_districts$distcode)
districts_to_merge <- mada_districts@data[6:nrow(mada_districts@data), ]
districts_to_merge$ADM2_EN <- recode(districts_to_merge$ADM2_EN, 
                                     `6e Arrondissement` = "Antananarivo Renivohitra")
row.names(districts_dissolved) <- as.character(1:nrow(districts_to_merge))
row.names(districts_to_merge) <- as.character(1:nrow(districts_to_merge))
mada_districts <- SpatialPolygonsDataFrame(districts_dissolved, districts_to_merge)

# Get travel times and catchments for district and commune levels ----------------------------------
# District level
district_df <- fread("output/ttimes/baseline_district.csv")

# Filter so that catchment is the one which is closest for the maximum proportion of the population
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = distcode]
district_df$id_ctar <- ctar_metadata$id_ctar[match(district_df$catchment, ctar_metadata$CTAR)]
mada_districts@data <- district_df[mada_districts@data, on = "distcode"]

# Do the same for commune level
commune_df <- fread("output/ttimes/baseline_commune.csv")
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                         by = commcode]
commune_df$id_ctar <- ctar_metadata$id_ctar[match(commune_df$catchment, ctar_metadata$CTAR)]
mada_communes@data <- commune_df[mada_communes@data, on = c("commcode" = "ADM3_PCODE")]

# Get centroid longitude and latitude (for plotting) --------------------------------------------
mada_districts$long_cent <- coordinates(mada_districts)[, 1]
mada_districts$lat_cent <- coordinates(mada_districts)[, 2]
mada_communes$long_cent <- coordinates(mada_communes)[, 1]
mada_communes$lat_cent <-  coordinates(mada_communes)[, 2]

# Clean up names -----------------------------------------------------------------------------------
# NOTE: var names have to be <= 10 characters long for ESRI shapefile output
mada_districts@data %>%
  dplyr::select(distcode, district = ADM2_EN, pop, long_cent, lat_cent, ttimes_wtd, catchment,
                id_ctar, pop_catch = prop_pop_catch) -> mada_districts@data

mada_communes@data %>%
  dplyr::select(distcode, district = ADM2_EN, commcode, commune = ADM3_EN, pop, 
                long_cent, lat_cent, ttimes_wtd, catchment, id_ctar,
                pop_catch = prop_pop_catch) -> mada_communes@data

# Write out the shapefiles (overwrite) ------------------------------------------------------------
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Saving session info
out.session(path = "R/01_gis/05_make_shapefiles.R", filename = "sessionInfo.csv")

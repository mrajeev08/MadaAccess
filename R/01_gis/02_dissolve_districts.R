####################################################################################################
##' Generating GIS files Step 1
##' Details: Getting masked and unmasked friction layers to input into travel time estimates 
##' Need to do this outside of the cluster because problem with connecting to external server
##' Author: Malavika Rajeev 
####################################################################################################

##' Libraries and packages
rm(list=ls())
library(rgdal)
library(rgeos)
library(dplyr)

##' Shapefiles
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")

##' Processing and writing out shapefiles
##' ------------------------------------------------------------------------------------------------
##' Last bit because these are new districts and we do not have data to the arrondisement level for 
##' Tana (i.e. Antananarivo Renivohitra)
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

districts_dissolved <- gUnaryUnion(mada_districts, id = mada_districts$distcode)
districts_df <- mada_districts@data[6:nrow(mada_districts@data), ]
districts_df$ADM2_EN <- recode(districts_df$ADM2_EN, `6e Arrondissement` = "Antananarivo Renivohitra")
row.names(districts_dissolved) <- as.character(1:nrow(districts_df))
row.names(districts_df) <- as.character(1:nrow(districts_df))
mada_districts <- SpatialPolygonsDataFrame(districts_dissolved, districts_df)

##' Write out the shapefiles to processed/shapefiles/ (overwrite)
##' ------------------------------------------------------------------------------------------------
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

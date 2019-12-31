##################################################################################################
##' Getting max ttimes (i.e. if all CSBS in Mada have a clinic)
##' Author: Malavika Rajeev
##################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
Sys.time()
rm(list = ls())

##' Libraries
library(rgdal)
library(raster)
library(foreach)
library(tidyverse)
library(gdistance)
library(iterators)
library(doParallel)
library(data.table)

##' Source
source("R/functions/utils.R")
source("R/functions/ttime_functions.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

## candidate points
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
## candidate points
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, X_COORD = ycoor, Y_COORD = xcoor) -> csbs

point_mat_all <- as.matrix(rbind(dplyr::select(csbs, Y_COORD, X_COORD),
                           dplyr::select(ctar_metadata, Y_COORD = LONGITUDE, X_COORD = LATITUDE)))

## Max raster
ttimes_max <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                          coords = point_mat_all, trans_matrix_exists = TRUE,
                          filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_max, "output/ttimes/max_ttimes.tif")

## Max at grid level to get district/comm dataframes
cand_mat <- fread("output/ttimes/candidate_matrix.gz") ## locally
cand_mat <- as.matrix(cand_mat)
max_catches <- apply(cand_mat, 1, which.min) + 31
max_times <- apply(cand_mat, 1, min, na.rm = TRUE)

## Get df with district and commune ids and aggregate accordingly
base_df <- fread("output/ttimes/baseline_grid.csv")
base_df[, c("base_times", "base_catches") := .(ifelse(is.infinite(max_times), NA, max_times),
                                             ifelse(is.infinite(max_times), NA, max_catches))]
fwrite(base_df, "output/ttimes/max_grid.csv")

district_df <-
  base_df[, .(weighted_times = sum(base_times * pop, na.rm = TRUE), 
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_dist[1], pop = pop_dist[1],
              scenario = "max"), 
          by = .(district_id, base_catches)]
district_df[, weighted_times := sum(weighted_times, na.rm = TRUE)/pop, by = district_id]
fwrite(district_df, "output/ttimes/max_district.csv")

commune_df <-
  base_df[, .(weighted_times = sum(base_times * pop, na.rm = TRUE),
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_comm[1], pop = pop_comm[1],
              scenario = "max"), 
          by = .(commune_id, base_catches)]
commune_df[, weighted_times := sum(weighted_times, na.rm = TRUE)/pop, by = commune_id]
fwrite(commune_df, "output/ttimes/max_commune.csv")


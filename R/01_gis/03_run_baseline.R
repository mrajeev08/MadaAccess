##################################################################################################
##' Step 3: Getting baseline travel time estimates
##' Details: Getting baseline travel time estimates and catchments for grid cells and also summarizing
##' to admin units with the 31 existing clinics
##' Author: Malavika Rajeev
##################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
Sys.time()
rm(list = ls())

##' Packages
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
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

##' Get candidate points as matrix
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
point_mat_base <- as.matrix(dplyr::select(ctar_metadata, Y_COORD = LONGITUDE, X_COORD = LATITUDE))

##' Do the baseline
##' For each clinic get the minimum travel times at the raster scale
cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  foreach(points = iter(point_mat_base, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
          } -> stacked_ttimes
})

stopCluster(cl) ## for doParallel
## 6 seconds per point

##' stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ] # to save memory filter out NAs

##' Ids for districts and communes (row # of shapefiles)
district_id <- getValues(rasterize(mada_districts, 
                                  friction_masked))[!is.na(getValues(friction_masked))]
commune_id <- getValues(rasterize(mada_communes, 
                                 friction_masked))[!is.na(getValues(friction_masked))]

##' Max at grid level to get district/comm dataframes
base_catches <- apply(stacked_ttimes, 1, which.min)
base_times <- apply(stacked_ttimes, 1, min, na.rm = TRUE)
base_times[is.infinite(base_times)] <- NA

##' Baseline dataframe at grid level
baseline_df <- data.table(district_id = district_id, commune_id = commune_id, 
                          pop = getValues(pop1x1)[!is.na(getValues(friction_masked))],
                          base_times, base_catches)
baseline_df[, prop_pop := pop/sum(pop, na.rm = TRUE)]
baseline_df[, pop_dist := sum(pop, na.rm = TRUE), by = district_id]
baseline_df[, pop_comm := sum(pop, na.rm = TRUE), by = commune_id]

fwrite(baseline_df, "output/ttimes/baseline_grid.csv")

##' use district and commune ids and aggregate accordingly
##' Weighted times is mean travel times weighted by the population in each grid cell for each admin
##' unit
##' Districts
district_df <-
  baseline_df[, .(weighted_times = sum(base_times * pop, na.rm = TRUE), 
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_dist[1], pop = pop_dist[1],
              scenario = 0), 
          by = .(district_id, base_catches)]
district_df[, weighted_times := sum(weighted_times, na.rm = TRUE)/pop, by = district_id]
fwrite(district_df, "output/ttimes/baseline_district.csv")

##' Communes
commune_df <-
  baseline_df[, .(weighted_times = sum(base_times * pop, na.rm = TRUE),
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_comm[1], pop = pop_comm[1],
              scenario = 0), 
          by = .(commune_id, base_catches)]
commune_df[, weighted_times := sum(weighted_times, na.rm = TRUE)/pop, by = commune_id]
fwrite(commune_df, "output/ttimes/baseline_commune.csv")

##' Quick comparison check
##' This should generate the same min estimates at the raster level as getting them one by one
##' Getting them one by one in order to establish which clinic is the closest (i.e. identify catcments)
ttimes_comp <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                          coords = point_mat_base, trans_matrix_exists = TRUE,
                          filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_comp, "output/ttimes/baseline_ttimes.tif") ##' write out for plotting
ttimes_comp <- getValues(ttimes_comp)[!is.na(getValues(friction_masked))]
sum(base_times - ttimes_comp, na.rm = TRUE) # should ~ 0

##' Saving session info
out.session(path = "R/01_gis/03_run_baseline.R", filename = "sessionInfo.csv")

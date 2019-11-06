##################################################################################################
##' Getting baseline travel time estimates and catchments for 31 baseline clinics
##' Can do this without parallelizing as well (takes 15 min with 3 cores vs. 1 hr with 1)
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
point_mat_base <- as.matrix(dplyr::select(ctar_metadata, Y_COORD = LONGITUDE, X_COORD = LATITUDE))

## Do the baseline
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

## stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ]

## Ids for districts and communes (row ids of shapefiles)
district_id <- getValues(rasterize(mada_districts, 
                                  friction_masked))[!is.na(getValues(friction_masked))]
commune_id <- getValues(rasterize(mada_communes, 
                                 friction_masked))[!is.na(getValues(friction_masked))]
## Prop pop
prop_pop <- getValues(pop1x1)/sum(getValues(pop1x1), na.rm = TRUE)
prop_pop <- prop_pop[!is.na(getValues(friction_masked))] ## Filter out masked cells

base_df <- data.table(district_id, commune_id, prop_pop, 
                      pop = getValues(pop1x1)[!is.na(getValues(friction_masked))],
                      base_times = rep(1e6, nrow(stacked_ttimes)), 
                      base_catches = rep(0, nrow(stacked_ttimes)))
base_df[, pop_dist := sum(pop, na.rm = TRUE), by = district_id]
base_df[, pop_comm := sum(pop, na.rm = TRUE), by = commune_id]

cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  ttimes_weighted <- add.armc(base_df = base_df, clinic_names = 1:31, 
                              clinic_catchmat = as.data.table(stacked_ttimes), 
                              max_clinics = ncol(stacked_ttimes),
                              thresh_ttimes = 0, thresh_prop = 0, 
                              dir_name = "output/ttimes/baseline_")
})

stopCluster(cl) ## for doParallel

base_times <- ttimes_weighted[["ttimes"]]
base_catches <- ttimes_weighted[["catches"]]

# ## Quick comparison check
ttimes_comp <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                          coords = point_mat_base, trans_matrix_exists = TRUE,
                          filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_comp, "output/ttimes/baseline_ttimes.tif")
ttimes_comp <- getValues(ttimes_comp)[!is.na(getValues(friction_masked))]
sum(base_times - ttimes_comp, na.rm = TRUE)

## Baseline dataframe
baseline_df <- data.table(district_id = district_id, commune_id = commune_id, 
                          pop = getValues(pop1x1)[!is.na(getValues(friction_masked))],
                          base_times, base_catches)
baseline_df[, prop_pop := pop/sum(pop, na.rm = TRUE)]
baseline_df[, pop_dist := sum(pop, na.rm = TRUE), by = district_id]
baseline_df[, pop_comm := sum(pop, na.rm = TRUE), by = commune_id]

fwrite(baseline_df, "output/ttimes/baseline_grid.csv")

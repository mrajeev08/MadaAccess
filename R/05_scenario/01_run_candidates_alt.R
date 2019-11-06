####################################################################################################
##' Testing catchment scripts
##' Details: Getting travel time estimates and catchments for all clinics
##'   Code must be run in parallel
##'   On the Della cluster at Princeton with NN cores, it takes approximately NN minutes
##' Author: Malavika Rajeev
####################################################################################################

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
source("R/functions/access_functions.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

## candidate points
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, X_COORD = ycoor, Y_COORD = xcoor) -> csbs

point_mat_base <- as.matrix(select(ctar_metadata, Y_COORD = LONGITUDE, X_COORD = LATITUDE))
point_mat_candidates <- as.matrix(select(csbs, Y_COORD, X_COORD))
candidate_ids <- 1:nrow(csbs) + 31 ## above the baseline 

## Do the baseline
cl <- makeCluster(4)
registerDoParallel(cl)

system.time ({
  foreach(points = iter(point_mat_base, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.access(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds",
                                 metric = "ttimes")
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
base_df[, prop_pop_dist := pop/sum(pop, na.rm = TRUE), by = district_id]
base_df[, prop_pop_comm := pop/sum(pop, na.rm = TRUE), by = commune_id]

cl <- makeCluster(4)
registerDoParallel(cl)
system.time ({
  ttimes_weighted <- add.armc(base_df = base_df, clinic_names = 1:31, 
                              clinic_catchmat = as.data.table(stacked_ttimes), 
                              max_clinics = ncol(stacked_ttimes),
                              threshold = 0, thresh_prop = 1e-4, 
                              dir_name = "output/baseline_")
})

stopCluster(cl) ## for doParallel

base_times <- ttimes_weighted[["ttimes"]]
base_catches <- ttimes_weighted[["catches"]]

# ## Quick comparison check
# ttimes_comp <- get.access(friction = friction_masked, shapefile = mada_districts,
#                           coords = point_mat_base, trans_matrix_exists = TRUE,
#                           filename_trans = "data/processed/rasters/trans_gc_masked.rds",
#                           metric = "ttimes")
# ttimes_comp <- getValues(ttimes_comp)[!is.na(getValues(friction_masked))]
# sum(base_times - ttimes_comp, na.rm = TRUE)

## Baseline dataframe
baseline_df <- data.table(district_id = district_id, commune_id = commune_id, 
                          pop = getValues(pop1x1)[!is.na(getValues(friction_masked))],
                          base_times, base_catches)
baseline_df[, prop_pop := pop/sum(pop, na.rm = TRUE)]
baseline_df[, pop_dist := sum(pop, na.rm = TRUE), by = district_id]
baseline_df[, pop_comm := sum(pop, na.rm = TRUE), by = commune_id]

fwrite(baseline_df, "output/baseline.csv")

## Do the candidates
cl <- makeCluster(4)
registerDoParallel(cl)

system.time ({
  foreach(points = iter(point_mat_candidates, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.access(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds",
                                 metric = "ttimes")
     } -> stacked_ttimes
})

stopCluster(cl) ## for doParallel
## 6 seconds per point

## stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ]

prop.lessthan <- function(x, prop_pop, base_metric, threshold) {
  ## Sum of the proportion of the population changed weighted by how much changed below threshold
  sum(prop_pop[which(x < base_metric & x > threshold)], na.rm = TRUE)
}

change_prop <- foreach(vals = iter(stacked_ttimes, by = "col"), 
                     .combine = c) %do% {
    prop.lessthan(vals, prop_pop = prop_pop, base_metric = ttimes_weighted, 
                                             threshold = 3*60)
}

stacked_ttimes <- stacked_ttimes[, -which(change_prop < 0.0001)]
candidate_ids <- candidate_ids[which(change_prop > 0.0001)]
  
fwrite(stacked_ttimes, "output/candidate_matrix.csv")
write.csv(candidate_ids, "output/candidate_ids.csv")

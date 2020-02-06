##################################################################################################
##' Getting baseline travel time estimates
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
mada_distreg <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")

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

## pop comparison
wp_2015 <- raster("data/processed/rasters/wp_2015_1x1.tif")
wp_2018 <- raster("data/processed/rasters/wp_2018_1x1.tif")
wp_2020 <- raster("data/processed/rasters/wp_2020_1x1.tif")
fb_2018 <- raster("data/processed/rasters/fb_2018_1x1.tif")

##' Baseline dataframe at grid level
baseline_df <- data.table(district_id = district_id, commune_id = commune_id, 
                          wp_2015 = getValues(wp_2015)[!is.na(getValues(friction_masked))],
                          wp_2018 = getValues(wp_2018)[!is.na(getValues(friction_masked))],
                          wp_2020 = getValues(wp_2020)[!is.na(getValues(friction_masked))],
                          fb_2018 = getValues(fb_2018)[!is.na(getValues(friction_masked))],
                          base_times, base_catches)
baseline_df[, c("wp_2015_dist", 
                "wp_2018_dist", 
                "wp_2020_dist", 
                "fb_2018_dist") := list(sum(wp_2015, na.rm = TRUE), 
                                        sum(wp_2018, na.rm = TRUE),
                                        sum(wp_2020, na.rm = TRUE),
                                        sum(fb_2018, na.rm = TRUE)), by = district_id]
baseline_df[, c("wp_2015_comm", 
                "wp_2018_comm", 
                "wp_2020_comm", 
                "fb_2018_comm") := list(sum(wp_2015, na.rm = TRUE), 
                                        sum(wp_2018, na.rm = TRUE),
                                        sum(wp_2020, na.rm = TRUE),
                                        sum(fb_2018, na.rm = TRUE)), by = commune_id]

## Comparing pops against census 2018
census_2018 <- read.csv("data/raw/census2018.csv")
sum(census_2018$pop_2018)
sum(getValues(wp_2015), na.rm = TRUE)
sum(getValues(wp_2018), na.rm = TRUE)
sum(getValues(wp_2020), na.rm = TRUE)
sum(getValues(fb_2018), na.rm = TRUE)

fb_check <- fb_2018
fb_check[is.na(fb_check)] <- 0
rast_diff <- wp_2018 - fb_check
rasterVis::gplot(rast_diff) + 
  geom_raster(aes(fill = value)) + 
  scale_fill_distiller(type = "seq", palette = "GnBu", direction = 1, na.value = "white",
                       name = expression("Log(population) \n per 1km"^2)) +
  theme_void()

rasterVis::gplot(fb_2018) + 
  geom_raster(aes(fill = log(value))) + 
  scale_fill_distiller(type = "seq", palette = "GnBu", direction = 1, na.value = "white",
                       name = expression("Log(population) \n per 1km"^2)) +
  theme_void()

rasterVis::gplot(wp_2018) + 
  geom_raster(aes(fill = log(value))) + 
  scale_fill_distiller(type = "seq", palette = "GnBu", direction = 1, na.value = "white",
                       name = expression("Log(population) \n per 1km"^2)) +
  theme_void()

## Comparing pops at commune/district level
dist_pops <- baseline_df[, .(wp_2015_dist = sum(wp_2015, na.rm = TRUE),
                             wp_2018_dist = sum(wp_2018, na.rm = TRUE),
                             wp_2020_dist = sum(wp_2020, na.rm = TRUE), 
                             fb_2018_dist = sum(fb_2018, na.rm = TRUE)),
                         by = district_id]
comm_pops <- baseline_df[, .(wp_2015_comm = sum(wp_2015, na.rm = TRUE),
                             wp_2018_comm = sum(wp_2018, na.rm = TRUE),
                             wp_2020_comm = sum(wp_2020, na.rm = TRUE), 
                             fb_2018_comm = sum(fb_2018, na.rm = TRUE)),
                         by = commune_id]
ggplot(data = comm_pops, aes(x = wp_2018_comm, y = fb_2018_comm)) +
  geom_point()

sum(comm_pops$fb_2018)
sum(getValues(fb_2018), na.rm = TRUE)
sum(dist_pops$fb_2018_dist)

1 - exp(log(sum(dist_pops$fb_2018_dist)/sum(census_2018$pop_2018))/2)

## Comparing weighted ttimes with fb vs. world pop (and unweighted)
##' use district and commune ids and aggregate accordingly
##' Weighted times is mean travel times weighted by the population in each grid cell for each admin
##' unit
##' Districts
district_unweighted <- baseline_df[, .(unweighted_ttimes = mean(base_times, na.rm = TRUE)), 
                                   by = district_id]

district_df <-
  baseline_df[, .(weighted_times_fb = sum(base_times * fb_2018, na.rm = TRUE), 
              prop_catch_fb = sum(fb_2018, na.rm = TRUE)/fb_2018_dist[1], 
              pop_fb = fb_2018_dist[1], weighted_times_wp = sum(base_times * wp_2015, na.rm = TRUE), 
              prop_catch_wp = sum(wp_2015, na.rm = TRUE)/wp_2015_dist[1], 
              pop_wp = wp_2015_dist[1],
              scenario = 0), 
          by = .(district_id, base_catches)]
district_df[, weighted_times_fb := sum(weighted_times_fb, na.rm = TRUE)/pop_fb, by = district_id]
district_df[, weighted_times_wp := sum(weighted_times_wp, na.rm = TRUE)/pop_wp, by = district_id]
district_df <- district_df[district_unweighted, on = "district_id"]

ggplot(data = district_df, aes(x = weighted_times_fb, y = weighted_times_wp)) + geom_point()
ggplot(data = district_df, aes(x = weighted_times_fb, y = unweighted_times)) + geom_point()
ggplot(data = district_df, aes(x = weighted_times_wp, y = unweighted_times)) + geom_point()

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

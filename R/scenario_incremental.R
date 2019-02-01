## Scenario analysis, incremental parallelized

## Init MPI Backend
rm(list = ls())
# args <- commandArgs(trailingOnly = TRUE)
# cores <- as.integer(args[9])
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

## libraries
library(raster)
library(tidyverse)
library(rgdal)
library(gdistance)
library(foreach)

## source travel times and add armc functions
source("R/get.ttimes.R")
source("R/add.ARMC.R")

## shapefiles
mada_district <- readOGR("data/MadaGIS/district_init.shp")
friction_masked <- raster("output/friction_mada_masked.tif")
friction_unmasked <- raster("output/friction_mada_unmasked.tif")
ttimes_base <- raster("output/ttimes_masked.tif")

## proportion of pop
pop10 <- raster("output/pop10.tif")
prop_pop <- pop10/sum(values(pop10), na.rm = TRUE)

## baseline proportion
threshold = 3 # threshold # of hours to calculate the proportion of pop at
base_prop <- sum(prop_pop[ttimes_base >= 60*threshold], na.rm = TRUE)

## Existing points
## Point locations
gps_locs <- read.csv(file = "data/ctar_metadata.csv", stringsAsFactors = FALSE)[,c(1, 3, 4)]
names(gps_locs) <- c ("CTAR", "X_COORD", "Y_COORD")

## candidate points
csbs <- read.csv("data/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, X_COORD = ycoor, Y_COORD = xcoor) -> csbs

## filter so that if within 5 km radius, you pick one with highest pop density
## need to use package biosphere
# dist_mat <- as.matrix(dist(cbind(csbs$xcoor, csbs$ycoor)))
# plot(csbs$xcoor, csbs$ycoor)

## check just with 10 
system.time({
  scenario <- add.armc(current_ARMC = gps_locs, candidate_ARMC = csbs, prop_pop, 
                  threshold = 3, delta_tt_min = 1e-20,
                  steps = nrow(csbs), base_prop, 
                  friction = friction_unmasked, shapefile = mada_district, 
                  filename_trans = "output/trans_gc_unmasked.rds")
})
write.csv(scenario, "output/scenario_incremental_prop3hrs_masked.csv")

### Then just close it out at the end
closeCluster(cl)
mpi.quit()

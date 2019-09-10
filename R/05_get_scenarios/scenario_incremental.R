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
library(data.table)

## source travel times and add armc functions
source("R/ttime_functions.R")

## shapefiles
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
mada_district <- readOGR("data/MadaGIS/district_init.shp")
friction_masked <- raster("output/friction_mada_masked.tif")
friction_unmasked <- raster("output/friction_mada_unmasked.tif")
ttimes_base <- raster("output/ttimes_unmasked.tif")

## proportion of pop
pop10 <- raster("output/pop10.tif")
prop_pop <- pop10/sum(values(pop10), na.rm = TRUE)

## getting pops
print(paste(Sys.time(), ": started extracting pop10"))
mada_district <- raster::extract(pop10, mada_district, fun = sum,
                                 na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
mada_communes <- raster::extract(pop10, mada_communes, fun = sum,
                                 na.rm = TRUE, df = TRUE, sp = TRUE, small = TRUE)
print(paste(Sys.time(), ": finished extracting pop10"))

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

## Run additional ARMC
system.time({
  scenario <- add.armc(current_ARMC = gps_locs, candidate_ARMC = csbs, prop_pop, 
                  threshold = 3, delta_tt_min = 1e-4,
                  steps = nrow(csbs), base_prop, 
                  friction = friction_unmasked, shapefile = mada_district, 
                  filename_trans = "output/trans_gc_unmasked.rds")
})

save(scenario, paste0("output/scenario_incremental_prop3hrs_", as.numeric(Sys.time()), ".RData"))

## Calc ttimes for each addtl ARMC
dist_mat <- run.scenario(current_ARMC = gps_locs, new_ARMC = scenario[["new_armc"]], friction = friction_unmasked, 
             shape = mada_district, pop_rast = pop10, pop_pol = mada_district$pop10,
             admin = "district", weighted = TRUE, filename_trans = "output/trans_gc_unmasked.rds")

comm_mat <- run.scenario(current_ARMC = gps_locs, new_ARMC = scenario[["new_armc"]], friction = friction_unmasked, 
             shape = mada_communes, pop_rast = pop10, pop_pol = mada_communes$pop10,
             admin = "commune", weighted = TRUE, filename_trans = "output/trans_gc_unmasked.rds")

### Then just close it out at the end
closeCluster(cl)
mpi.quit()

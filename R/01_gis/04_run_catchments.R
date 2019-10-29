####################################################################################################
##' Step 3: Generating catchment matrix
##' Details: Getting travel times and distance to each clinic for each admin unit 
##'   Recommended to run in parallel to limit compute time
##'   On the Della cluster at Princeton with 32 cores, it takes approximately 12 minutes
##' Author: Malavika Rajeev 
####################################################################################################

##' Set-up 
##' ------------------------------------------------------------------------------------------------
##' Cluster
rm(list = ls())
# args <- commandArgs(trailingOnly = TRUE)
# cores <- as.integer(args[9])
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

##' Libraries
library(rgdal)
library(raster)
library(malariaAtlas)
library(gdistance)
library(doRNG)
library(foreach)
library(dplyr)
library(geosphere)
select <- dplyr::select
source("R/functions/access_functions.R")

##' Shapefiles and CTAR point matrix
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
point_mat <- as.matrix(select(ctar_metadata, Y_COORD = LONGITUDE, X_COORD = LATITUDE))
friction_mada_unmasked <- raster("data/processed/rasters/friction_mada_unmasked.tif")
friction_mada_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

##' Quick function for getting catchments accounting for Inf returns
which.min.inf  <- function(x) {
  if(min(x) == Inf | is.na(min(x))) {
    return(NA)
  } else {
    return(which.min(x))
  }
}

##' Getting catchment matrix weighted and unweighted
##' If masked is.na then replace with unmasked (basically for the coastal and island admin units 
##' allows for travel over water)
##' ------------------------------------------------------------------------------------------------
##' Districts
print(paste(Sys.time(), ": started generating district catchmats"))

## Weighted travel times
dist_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  point_names = ctar_metadata$CTAR, admin_names = mada_districts$distcode,
                                  shape = mada_districts, pop_rast = pop1x1, 
                                  pop_pol = mada_districts$pop, 
                                  trans_mat = "data/processed/rasters/trans_gc_unmasked.rds",
                                  weighted = TRUE, met = "ttimes")
dist_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                point_names = ctar_metadata$CTAR, admin_names = mada_districts$distcode,
                                shape = mada_districts, pop_rast = pop1x1, 
                                pop_pol = mada_districts$pop, 
                                trans_mat = "data/processed/rasters/trans_gc_masked.rds",
                                weighted = TRUE, met = "ttimes")
dist_mat_masked[which(is.na(apply(dist_mat_masked, 1, which.min.inf))), ] <- 
  dist_mat_unmasked[which(is.na(apply(dist_mat_masked, 1, which.min.inf))), ]
write.csv(dist_mat_masked, "data/processed/catchmats/baseline_district_ttimes_weighted.csv")

## distance weighted
dist_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                point_names = ctar_metadata$CTAR, admin_names = mada_districts$distcode,
                                shape = mada_districts, pop_rast = pop1x1, 
                                pop_pol = mada_districts$pop, 
                                weighted = TRUE, met = "distance")
write.csv(dist_mat_masked, "data/processed/catchmats/baseline_district_distance_weighted.csv")
print(paste(Sys.time(), ": finished generating district catchmats"))

##' Communes
print(paste(Sys.time(), ": started generating commune catchmats"))

## travel times weighted
comm_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  point_names = ctar_metadata$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                  shape = mada_communes, pop_rast = pop1x1, 
                                  trans_mat = "data/processed/rasters/trans_gc_unmasked.rds",
                                  pop_pol = mada_communes$pop, weighted = TRUE,
                                  met = "ttimes")
comm_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_communes, pop_rast = pop1x1, 
                                point_names = ctar_metadata$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                trans_mat = "data/processed/rasters/trans_gc_masked.rds",
                                pop_pol = mada_communes$pop, weighted = TRUE,
                                met = "ttimes")
comm_mat_masked[which(is.na(apply(comm_mat_masked, 1, which.min.inf))), ] <- 
  comm_mat_unmasked[which(is.na(apply(comm_mat_masked, 1, which.min.inf))), ]
write.csv(comm_mat_masked, "data/processed/catchmats/baseline_commune_ttimes_weighted.csv")

## distance weighted
comm_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_communes, pop_rast = pop1x1, 
                                point_names = ctar_metadata$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                pop_pol = mada_communes$pop, weighted = TRUE,
                                met = "distance")
write.csv(comm_mat_masked, "data/processed/catchmats/baseline_commune_distance_weighted.csv")

print(paste(Sys.time(), ": finished generating commune catchmats"))


##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

####################################################################################################
##' Step 1: Generating catchment matrix for all possible additional clinics
##' Details: Getting travel times and distance to each clinic for each admin unit 
##'   Recommended to run in parallel to limit compute time
##'   On the Della cluster at Princeton with 32 cores, it takes approximately 6 minutes
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
library(gdistance)
library(doRNG)
library(foreach)
library(geosphere)
library(dplyr)
select <- dplyr::select
source("R/functions/access_functions.R")

##' Shapefiles and CTAR point matrix
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
friction_mada_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
friction_mada_unmasked <- raster("data/processed/rasters/friction_mada_unmasked.tif")

## candidate points
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, X_COORD = ycoor, Y_COORD = xcoor) -> csbs
point_mat <- as.matrix(select(csbs, Y_COORD, X_COORD))

## Weighted travel times
## District
dist_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  point_names = csbs$CTAR, admin_names = mada_districts$distcode,
                                  shape = mada_districts, pop_rast = pop1x1, 
                                  pop_pol = mada_districts$pop, 
                                  trans_mat = "data/processed/rasters/trans_gc_unmasked.rds",
                                  weighted = TRUE, type = "unmasked", met = "ttimes")
dist_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                point_names = csbs$CTAR, admin_names = mada_districts$distcode,
                                shape = mada_districts, pop_rast = pop1x1, 
                                pop_pol = mada_districts$pop, 
                                trans_mat = "data/processed/rasters/trans_gc_masked.rds",
                                weighted = TRUE, type = "masked", met = "ttimes")
dist_mat_masked[which(is.na(apply(dist_mat_masked, 1, which.min.inf))), ] <- 
  dist_mat_unmasked[which(is.na(apply(dist_mat_masked, 1, which.min.inf))), ]
write.csv(dist_mat_masked, "data/processed/catchmats/candidates_district_ttimes_weighted.csv")

## Commune
comm_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  point_names = csbs$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                  shape = mada_communes, pop_rast = pop1x1, 
                                  trans_mat = "data/processed/rasters/trans_gc_unmasked.rds",
                                  pop_pol = mada_communes$pop, weighted = TRUE, type = "unmasked",
                                  met = "ttimes")
comm_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_communes, pop_rast = pop1x1, 
                                point_names = csbs$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                trans_mat = "data/processed/rasters/trans_gc_masked.rds",
                                pop_pol = mada_communes$pop, weighted = TRUE, type = "masked",
                                met = "ttimes")
comm_mat_masked[which(is.na(apply(comm_mat_masked, 1, which.min.inf))), ] <- 
  comm_mat_unmasked[which(is.na(apply(comm_mat_masked, 1, which.min.inf))), ]
write.csv(comm_mat_masked, "data/processed/catchmats/candidates_commune_ttimes_weighted.csv")

## Weighted distance
## District
dist_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  point_names = csbs$CTAR, admin_names = mada_districts$distcode,
                                  shape = mada_districts, pop_rast = pop1x1, 
                                  pop_pol = mada_districts$pop, 
                                  trans_mat = "data/processed/rasters/trans_gc_unmasked.rds",
                                  weighted = TRUE, type = "unmasked", met = "distance")
dist_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                point_names = csbs$CTAR, admin_names = mada_districts$distcode,
                                shape = mada_districts, pop_rast = pop1x1, 
                                pop_pol = mada_districts$pop, 
                                trans_mat = "data/processed/rasters/trans_gc_masked.rds",
                                weighted = TRUE, type = "masked", met = "distance")
dist_mat_masked[which(is.na(apply(dist_mat_masked, 1, which.min.inf))), ] <- 
  dist_mat_unmasked[which(is.na(apply(dist_mat_masked, 1, which.min.inf))), ]
write.csv(dist_mat_masked, "data/processed/catchmats/candidates_district_distance_weighted.csv")

## Commune
comm_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  point_names = csbs$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                  shape = mada_communes, pop_rast = pop1x1, 
                                  trans_mat = "data/processed/rasters/trans_gc_unmasked.rds",
                                  pop_pol = mada_communes$pop, weighted = TRUE, type = "unmasked",
                                  met = "distance")
comm_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_communes, pop_rast = pop1x1, 
                                point_names = csbs$CTAR, admin_names = mada_communes$ADM3_PCODE,
                                trans_mat = "data/processed/rasters/trans_gc_masked.rds",
                                pop_pol = mada_communes$pop, weighted = TRUE, type = "masked",
                                met = "distance")
comm_mat_masked[which(is.na(apply(comm_mat_masked, 1, which.min.inf))), ] <- 
  comm_mat_unmasked[which(is.na(apply(comm_mat_masked, 1, which.min.inf))), ]
write.csv(comm_mat_masked, "data/processed/catchmats/candidates_commune_distance_weighted.csv")

##' Distance Centroid
##' Communes
mada_commune_coords <- coordinates(mada_communes)
comm_distance_mat <- distm(mada_commune_coords, point_mat)/1000
write.csv(comm_distance_mat, "data/processed/catchmats/candidates_commune_distance_centroid.csv")

##' Districts
mada_district_coords <- coordinates(mada_districts)
dist_distance_mat <- distm(mada_district_coords, point_mat)/1000
write.csv(dist_distance_mat, "data/processed/catchmats/candidates_district_distance_centroid.csv")

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

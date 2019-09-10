####################################################################################################
##' Step 3: Generating catchment matrix
##' Details: Getting travel times to each clinic for each admin unit 
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
library(malariaAtlas)
library(gdistance)
library(doRNG)
library(foreach)
source("R/functions/ttime_functions.R")

##' Shapefiles from Malaria Atlas and CTAR point matrix
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
point_mat <- read.csv("data/processed/point_mat_CTAR.csv")
friction_mada_unmasked <- raster("data/processed/rasters/friction_mada_unmasked.tif")
friction_mada_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

##' Getting catchment matrix masked and unmasked
##' This function generates travel times to each CTAR for each admin unit (district or commune)
##' ------------------------------------------------------------------------------------------------
##' Districts
print(paste(Sys.time(), ": started generating district catchmats"))
dist_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                         shape = mada_districts, admin = "district", pop_rast = pop1x1, 
                         pop_pol = mada_districts$pop, weighted = TRUE, type = "unmasked")
dist_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_districts, admin = "district", pop_rast = pop1x1, 
                                pop_pol = mada_districts$pop, weighted = TRUE, type = "masked")
print(paste(Sys.time(), ": finished generating district catchmats"))

##' Communes
print(paste(Sys.time(), ": started generating commune catchmats"))
comm_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  shape = mada_communes, admin = "commune", pop_rast = pop1x1, 
                                  pop_pol = mada_communes$pop, weighted = TRUE, type = "unmasked")
comm_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_communes, admin = "commune", pop_rast = pop1x1, 
                                pop_pol = mada_communes$pop, weighted = TRUE, type = "masked")
print(paste(Sys.time(), ": finished generating commune catchmats"))

## Get unweighted vals too?!

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

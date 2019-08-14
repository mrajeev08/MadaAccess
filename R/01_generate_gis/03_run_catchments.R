####################################################################################################
##' Step 3: Generating catchment matrix
##' Details: Getting travel times to each clinic for each admin unit 
##'   Needs(!) to be run in parallel to limit compute time
##'   On the Della cluster at Princeton with NN cores, it takes approximately XX hrs
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
source("R/ttime_functions.R")

##' Shapefiles from Malaria Atlas and CTAR point matrix
mada_communes <- readOGR("data/shapefiles/communes.shp")
mada_districts <- readOGR("data/shapefiles/districts.shp")
pop1x1<- raster("output/worldpop2015adj_mada_1x1km.tif")
point_mat <- read.csv("output/point_mat_CTAR.csv")
friction_mada_unmasked <- raster("output/friction_mada_unmasked.tif")
friction_mada_masked <- raster("output/friction_mada_masked.tif")

##' Getting catchment matrix masked and unmasked
##' This function generates travel times to each CTAR for each admin unit (district or commune)
##' ------------------------------------------------------------------------------------------------
##' Districts
print(paste(Sys.time(), ": started generating district catchmats"))
dist_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                         shape = mada_districts, admin = "district", pop_rast = pop1x1, 
                         pop_pol = mada_district$pop1x1, weighted = TRUE, type = "unmasked")
dist_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_districts, admin = "district", pop_rast = pop1x1, 
                                pop_pol = mada_district$pop1x1, weighted = TRUE, type = "masked")
print(paste(Sys.time(), ": finished generating district catchmats"))

##' Communes
print(paste(Sys.time(), ": started generating commune catchmats"))
comm_mat_unmasked <- get.catchmat(point_mat = point_mat, fric = friction_mada_unmasked, 
                                  shape = mada_communes, admin = "commune", pop_rast = pop1x1, 
                                  pop_pol = mada_communes$pop1x1, weighted = TRUE, type = "unmasked")
comm_mat_masked <- get.catchmat(point_mat = point_mat, fric = friction_mada_masked, 
                                shape = mada_communes, admin = "commune", pop_rast = pop1x1, 
                                pop_pol = mada_communes$pop1x1, weighted = TRUE, type = "masked")
print(paste(Sys.time(), ": finished generating commune catchmats"))

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

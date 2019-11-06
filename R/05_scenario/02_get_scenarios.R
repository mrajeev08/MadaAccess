####################################################################################################
##' Testing catchment scripts
##' Details: Getting travel time estimates and catchments for all clinics
##'   Code must be run in parallel
##'   On the Della cluster at Princeton with NN cores, it takes approximately NN minutes
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
##' SINGLE NODE
rm(list = ls())
args <- commandArgs(trailingOnly = TRUE)
cores <- as.integer(args[1])
# cores <- 3

# ##' Init MPI Backend
# Sys.time()
# rm(list = ls())
# library(doMPI)
# cl <- startMPIcluster()
# clusterSize(cl) # this just tells you how many you've got
# registerDoMPI(cl)

##' Libraries
library(foreach)
library(tidyverse)
library(iterators)
library(data.table)
library(doParallel)

##' Source
source("R/functions/utils.R")
source("R/functions/ttime_functions.R")

## Pull in candidates
# cand_mat <- fread("output/candidate_matrix.gz") ## locally
cand_mat <- fread("/scratch/gpfs/mrajeev/candidate_matrix.gz")

# cand_mat <- as.matrix(cand_mat)
candidate_ids <- fread("output/candidate_ids.csv")$x

## Baseline df
base_df <- fread("output/baseline.csv")

## Do the candidates
## WITH SINGLE NODE
cl <- makeCluster(cores)
registerDoParallel(cl)

system.time ({
  add.armc(base_df = base_df, clinic_names = candidate_ids, clinic_catchmat = cand_mat, 
           max_clinics = ncol(cand_mat), threshold = 3*60, thresh_prop = 1e-4, 
           dir_name = "/scratch/gpfs/mrajeev/output/scenario_")
})

##' WITH SINGLE NODE TO CLOSE
stopCluster(cl)
print("Done :)")
Sys.time()

# ##' Close out cluster
# closeCluster(cl)
# mpi.quit()
# print("Done :)")
# Sys.time()
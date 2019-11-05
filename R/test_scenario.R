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
args <- commandArgs(trailingOnly = TRUE)
cores <- as.integer(args[1])
# cores <- 4

##' Libraries
library(foreach)
library(tidyverse)
library(iterators)
library(doParallel)
library(data.table)

##' Source
source("R/functions/utils.R")
source("R/functions/access_functions.R")

## Pull in candidates
cand_mat <- fread("/scratch/gpfs/mrajeev/candidate_matrix.gz")

# cand_mat <- as.matrix(cand_mat)
candidate_ids <- fread("output/candidate_ids.csv")$x

## Baseline df
base_df <- fread("output/baseline.csv")

## Do the candidates
## Estimation
cl <- makeCluster(cores)
registerDoParallel(cl)

system.time ({
  add.armc(base_df = base_df, clinic_names = candidate_ids, clinic_catchmat = cand_mat, 
           max_clinics = ncol(cand_mat), threshold = 3*60, thresh_prop = 1e-4, 
           dir_name = "/scratch/gpfs/mrajeev/output/scenario_")
})

##' Close out cluster
stopCluster(cl)
print("Done :)")
Sys.time()
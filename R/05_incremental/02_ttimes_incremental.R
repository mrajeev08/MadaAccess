####################################################################################################
##' Incrementally adding clinics based on travel times
##' Details: Getting travel time estimates and catchments for districts/communes as clinics are added
##'   Code should be run in parallel with shared memory if large input cand_mat
##'   On the Della cluster at Princeton with 18 cores, it takes approximately 1 hr 10 minutes; 
##'   On MacOS with 16 GB 1867 MHz DDR3 and 2.9 GHz Intel Core i5 with three cores, 
##'   it takes approximately 10 hours
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
##' SINGLE NODE
rm(list = ls())
args <- commandArgs(trailingOnly = TRUE)
cores <- as.integer(args[1])
# cores <- 3

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
# cand_mat <- fread("output/ttimes/candidate_matrix.gz") ## locally
cand_mat <- fread("/scratch/gpfs/mrajeev/output/ttimes/candidate_matrix.gz") ## this is a huge file!

# cand_mat <- as.matrix(cand_mat)
candidate_ids <- fread("output/ttimes/candidate_ids.csv")$x

## Baseline df
base_df <- fread("output/ttimes/baseline_grid.csv")

## Do the candidates
## WITH SINGLE NODE
cl <- makeCluster(cores)
registerDoParallel(cl)

system.time ({
  add.armc(base_df = base_df, clinic_names = candidate_ids, clinic_catchmat = cand_mat, 
           max_clinics = ncol(cand_mat), thresh_ttimes = 3*60, thresh_prop = 1e-4, 
           dir_name = "/scratch/gpfs/mrajeev/output/ttimes/incremental_")
})

##' WITH SINGLE NODE TO CLOSE
stopCluster(cl)
print("Done :)")
Sys.time()

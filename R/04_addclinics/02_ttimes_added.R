# ------------------------------------------------------------------------------------------------ #
#' Incrementally adding clinics based on travel times
#'  Getting travel time estimates and catchments for districts/communes as clinics are added
#'   Code should be run in parallel with shared memory if large input cand_mat
#'   On the Della cluster at Princeton with 18 cores, it takes approximately 1 hr 10 minutes; 
#'   On MacOS with 16 GB 1867 MHz DDR3 and 2.9 GHz Intel Core i5 with three cores, 
#'   it takes approximately 10 hours
# ------------------------------------------------------------------------------------------------ #

# set up cluster on single node with do Parallel
library(doParallel) 
cl <- makeCluster(18)
registerDoParallel(cl)
getDoParWorkers()
Sys.time()

# Libraries
library(foreach)
library(tidyverse)
library(iterators)
library(data.table)

# Source
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# Pull in candidates
cand_mat <- fread("/scratch/gpfs/mrajeev/output/ttimes/candidate_matrix.gz") # this is a huge file!
csb2 <- fread("data/processed/clinics/csb2.csv")

## Baseline df
base_df <- fread("output/ttimes/baseline_grid.gz")

## Do the candidates
system.time ({
  add.armc(base_df = base_df, clinic_names = csb2$clinic_id, clinic_catchmat = cand_mat, 
           max_clinics = ncol(cand_mat), thresh_ttimes = 3*60, thresh_prop = 1e-4, 
           dir_name = "/scratch/gpfs/mrajeev/output/ttimes/addclinics_")
})

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/output/ttimes/addclinics*"

# Close out
file_path <- "R/04_addclinics/02_ttimes_added.R"
out.session(path = file_path, filename = "log_cluster.csv")
print("Done remotely:)")
stopCluster(cl)
Sys.time()

# ------------------------------------------------------------------------------------------------ #
#' Incrementally adding clinics based on travel times
#'  Getting travel time estimates and catchments for districts/communes as clinics are added
#'   Code should be run in parallel with shared memory if large input cand_mat
#'   On the Della cluster at Princeton with 18 cores, it takes approximately 1 hr 10 minutes; 
#'   On MacOS with 16 GB 1867 MHz DDR3 and 2.9 GHz Intel Core i5 with three cores, 
#'   it takes approximately 10 hours
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-sn -t 12 -n 10 -sp "./R/04_addclinics/02_ttimes_added.R" -jn addclinics -wt 5m -n@
  
# set up cluster on single node with do Parallel
library(doParallel)
cl <- makeCluster(15)
registerDoParallel(cl)
start <- Sys.time()

# Libraries
library(foreach)
library(tidyverse)
library(iterators)
library(data.table)
library(raster)

# Source
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# baseline rasters 
base_df <- fread("output/ttimes/base_df.gz")

# Pull in candidates & figure out which files they're in
csb2 <- fread("data/processed/clinics/csb2.csv")
brick_dt <- get.bricks(brick_dir = "/scratch/gpfs/mrajeev/output/ttimes/candidates")
csb2 <- brick_dt[csb2, on = "clinic_id"]
csb2[, band := (clinic_id - min + 1)]

# Do the candidates
system.time({
  add.armc(base_df = base_df, cand_df = csb2, max_clinics = nrow(csb2), 
           thresh_ttimes = 3*60, thresh_prop = 1e-4, 
           dir_name = "/scratch/gpfs/mrajeev/output/ttimes/addclinics_")
})

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/output/ttimes/addclinics*"

# Close out
file_path <- "R/04_addclinics/02_ttimes_added.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)
stopCluster(cl)
print("Done remotely:)")

# ------------------------------------------------------------------------------
#' Incrementally adding clinics based on travel times
#'  Getting travel time estimates and catchments for districts/communes
#'  as clinics are added
#'  Code should be run in parallel if possible
# ------------------------------------------------------------------------------

# sub_cmd=-sn -t 12 -n 10 -jn addclinics -wt 5m

# Set it up
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = FALSE)

if (!set_up$slurm) fp <- here::here else fp <- cl_safe

cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

# Libraries
library(foreach)
library(tidyverse)
library(iterators)
library(data.table)
library(raster)

# Source
source(here_safe("R/ttime_functions.R"))

# baseline rasters
base_df <- fread(fp("analysis/out/ttimes/base/grid_df.gz"))

# Add one per district first
clin_per_dist <- fread(here_safe("data-raw/out/clinics/clinic_per_dist.csv"))
clin_per_dist[, candfile := fp(paste0("analysis/out/ttimes/candidates/clinic_",
                                      clinic_id, ".tif"))]

base_df <- add.armc(
  base_df = base_df, cand_df = clin_per_dist,
  max_clinics = nrow(clin_per_dist),
  rank_metric = prop, thresh_met = 1e-4,
  thresh_ttimes = 3 * 60, dir_name = fp("analysis/out/ttimes/addclinics"),
  base_scenario = 0, overwrite = TRUE, random = FALSE
)

# Then add one per commune
clin_per_comm <- fread(here_safe("data-raw/out/clinics/clinic_per_comm.csv"))
clin_per_comm[, candfile := fp(paste0("analysis/out/ttimes/candidates/clinic_",
                                      clinic_id, ".tif"))]

base_df <- add.armc(
  base_df = base_df, cand_df = clin_per_comm,
  max_clinics = nrow(clin_per_comm),
  rank_metric = prop,
  thresh_ttimes = 3 * 60, thresh_met = 1e-4,
  dir_name = fp("analysis/out/ttimes/addclinics"),
  base_scenario = nrow(clin_per_dist), overwrite = FALSE, # so will append to previous run
  random = FALSE
)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/ttimes/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/ttimes/addclinics"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

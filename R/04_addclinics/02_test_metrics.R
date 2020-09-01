# ------------------------------------------------------------------------------------------------ #
#' Test metrics for ranking clinics
# ------------------------------------------------------------------------------------------------ #

# sub_cmd=-sn -t 12 -n 10 -sp "./R/04_addclinics/02_test_metrics.R" -jn testmets -wt 5m -n@

start <- Sys.time()
slurm <- Sys.getenv("SLURM_JOB_ID") != ""
args <- commandArgs(trailingOnly = TRUE)

library(doParallel)

if (!slurm) {
  if (args[1] == "local") {
    ntasks <- detectCores() - 1
  } else {
    print("Running serially (not in parallel!)")
    # Look for the name of the script and how long it will take in the log and warn accordingly
  }
  out_log <- "output/log_local.csv"
  brick_path <- "output/ttimes/candidates"
} else {
  ntasks <- as.numeric(Sys.getenv("SLURM_NTASKS"))
  out_log <- "log_cluster.csv"
  brick_path <- "/scratch/gpfs/mrajeev/output/ttimes/candidates"
}

cl <- makeCluster(ntasks)
registerDoParallel(cl)

# Libraries
library(foreach)
library(tidyverse)
library(iterators)
library(data.table)
library(raster)
library(doRNG)

# Source
source("R/functions/ttime_functions.R")
source("R/functions/batch_functions.R")
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")

# baseline rasters
brick_dt <- get.bricks(brick_dir = "/scratch/gpfs/mrajeev/output/ttimes/candidates")
brick_dt <- get.bricks(brick_dir = "output/ttimes/candidates")
base_df <- fread("output/ttimes/base/grid_df.gz")

# Add one per district first
clin_per_dist <- fread("data/processed/clinics/clinic_per_dist.csv")
clin_per_dist <- brick_dt[clin_per_dist, on = "clinic_id"]

# Candidate functions here
prop_wtd <- function(base_df, cand_ttimes, thresh_ttimes) {
  inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes
  & !is.na(base_df$prop_pop))
  sum(base_df$prop_pop[inds] * # pop affected
    ((base_df$ttimes[inds] - cand_ttimes[inds]) / base_df$ttimes[inds]),
  na.rm = TRUE
  ) # weighted by reduction
}

prop <- function(base_df, cand_ttimes, thresh_ttimes) {
  inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes
  & !is.na(base_df$prop_pop))
  sum(base_df$prop_pop[inds], na.rm = TRUE) # prop pop only
}

mean_tt <- function(base_df, cand_ttimes, thresh_ttimes) {
  inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes
  & !is.na(base_df$prop_pop))
  mean(base_df$ttimes[inds] - cand_ttimes[inds], na.rm = TRUE) # prop pop only
}

metrics <- c("prop", "mean_tt", "prop_wtd", paste0("random", 1:10))

foreach(i = iter(metrics), .combine = rbind) %do% {
  rands <- ifelse(grepl("random", i), TRUE, FALSE)
  dir <- paste0("output/ttimes/tests/", i)
  rank_metric <- get(i)

  # Rank clinics (need to check base_df not modified!)
  add.armc(
    base_df = base_df, cand_df = clin_per_dist, max_clinics = nrow(clin_per_dist),
    rank_metric = rank_metric,
    thresh_ttimes = 3 * 60, random = rands,
    dir_name = dir,
    base_scenario = 0, overwrite = TRUE
  )

  # Post process these (will write out to same folder)
  process_ttimes(
    dir_name = dir, include_base = TRUE,
    base_dir = "output/ttimes/base"
  )

  # Do burden preds
  scenario_loop <- unique(fread(paste0(dir, "/commpreds_max.csv"))$lookup)

  # make df with the lookup + mod pars (reverse vec so big ones at end don't slow things down)
  lookup <- expand.grid(
    loop = rev(scenario_loop), scale = c("Commune", "District"),
    pop_predict = "flatPop", intercept = "fixed", data_source = "National",
    OD = TRUE, p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
    exp_min = 15 / 1e5, exp_max = 76 / 1e5, p_death = 0.16
  )

  all_preds <- run_scenarios(
    lookup = lookup, pred_type = "burden",
    par_type = "posterior", scaled = FALSE,
    directory = dir,
    colnames_max = colnames(fread(paste0(dir, "/commpreds_max.csv"))),
    colnames_all = colnames(fread(paste0(dir, "/commpreds_all.csv"))),
    colnames_j = c("scale", "data_source", "OD"),
    admin_to_keep = NULL, # keep all admin ones for burden
    multicomb = function(x, ...) {
      mapply(rbind, x, ..., SIMPLIFY = FALSE)
    },
    rng_seed = 23481, sims = 1000
  )
  out <- all_preds$natl_preds
  out$metric <- i
  out
} -> test_preds

fwrite(test_preds, "output/ttimes/tests/test_preds.csv")

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/tests/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/ttimes/tests/*"

# Session Info
out.session(
  path = "02_test_metrics.R", filename = "output/log_local.csv",
  start = start
)

stopCluster(cl)
print("Done :)")

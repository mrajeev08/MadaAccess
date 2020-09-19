# ------------------------------------------------------------------------------
#' Test metrics for ranking clinics
# ------------------------------------------------------------------------------

# sub_cmd=-sn -t 12 -n 10 -jn testmets -wt 5m

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
library(doRNG)

# Source
source(here_safe("R/ttime_functions.R"))
source(here_safe("R/batch_functions.R"))
source(here_safe("R/predict_functions.R"))
base_df <- fread(fp("analysis/out/ttimes/base/grid_df.gz"))
clin_per_dist <- fread("data-raw/out/clinics/clinic_per_dist.csv")

# Add one per district first
clin_per_dist[, candfile := fp(paste0("analysis/out/ttimes/candidates/clinic_",
                                      clinic_id, ".tif"))]

metrics <- c("prop", "mean_tt", "prop_wtd", paste0("random", 1:10))

foreach(i = iter(metrics), .combine = rbind) %do% {
  if (grepl("random", i)) {
    rands <- TRUE
    rank_metric <- NULL
  } else {
    rands <- FALSE
    rank_metric <- get(i)
  }

  dir <- fp(paste0("analysis/out/ttimes/tests/", i))

  # Rank clinics (need to check base_df not modified!)
  add_armc(
    base_df = base_df, cand_df = clin_per_dist,
    max_clinics = nrow(clin_per_dist),
    rank_metric = rank_metric,
    thresh_ttimes = 3 * 60, random = rands,
    dir_name = dir,
    base_scenario = 0, overwrite = TRUE
  )

  # Post process these (will write out to same folder)
  process_ttimes(
    dir_name = dir, include_base = TRUE,
    base_dir = fp("analysis/out/ttimes/base")
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
    ttimes_dir = dir,
    mod_dir = fp("analysis/out/mods/samps/"),
    colnames_max = colnames(fread(paste0(dir, "/commpreds_max.csv"))),
    colnames_all = colnames(fread(paste0(dir, "/commpreds_all.csv"))),
    colnames_j = c("scale", "data_source", "OD"),
    admin_to_keep = NULL, # keep all admin ones for burden
    rng_seed = 23481, sims = 1000
  )

  out <- all_preds$natl_preds
  out$metric <- i
  out
} -> test_preds

write_create(
  test_preds, fp("analysis/out/ttimes/tests/test_preds.csv"),
  fwrite
)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/ttimes/tests/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/ttimes/tests/*"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

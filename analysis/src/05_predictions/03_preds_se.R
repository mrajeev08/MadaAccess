# ------------------------------------------------------------------------------
#' Getting sensitivity to parameter assumptions for burden &
#' impact of expanding access
# ------------------------------------------------------------------------------

# sub_cmd=-t 12 -n 31 -jn preds_se -wt 5m

# Set it up
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = TRUE)
if(!set_up$slurm) fp <- here::here else fp <- cl_safe
cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

# pkgs
library(data.table)
library(foreach)
library(doRNG)
library(iterators)
library(tidyr)
library(dplyr)
library(triangle)
source(here_safe("R/predict_functions.R"))
source(here_safe("R/batch_functions.R"))
select <- dplyr::select

# Set-up these two vectors for reading in data using cmd line args
scenario_loop <- unique(fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))$lookup)
se_pars <- fread(fp("analysis/out/sensitivity/se_pars.csv"))

# reverse so bigger dfs don't slow everything down
lookup <- expand_grid(loop = rev(scenario_loop), se_pars)

# Do burden -----------------------------------------------------------------
all_preds <- run_scenarios(
  lookup = lookup, pred_type = "burden",
  par_type = "point_est", scaled = FALSE,
  ttimes_dir = fp("analysis/out/ttimes/addclinics"),
  mod_dir = fp("analysis/out/mods/samps/"),
  colnames_max = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))),
  colnames_all = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_all.csv"))),
  colnames_j = c("scale", "data_source", "vary", "direction"),
  admin_to_keep = "scenario_0",
  rng_seed = 23492, sims = 1000
)

# Write out data
write_create(all_preds$admin_preds,
             fp("analysis/out/sensitivity/burden_baseline_se.gz"),
             fwrite)
write_create(all_preds$natl_preds,
             fp("analysis/out/sensitivity/burden_addclinics_se.gz"),
             fwrite)

# Do vials -----------------------------------------------------------------
lookup <- filter(lookup, grepl("beta|sigma", vary))

all_preds <- run_scenarios(
  lookup = lookup, pred_type = "vials",
  par_type = "point_est", scaled = FALSE, catch_keep = FALSE,
  ttimes_dir = fp("analysis/out/ttimes/addclinics/"),
  mod_dir = fp("analysis/out/mods/samps/"),
  colnames_max = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))),
  colnames_all = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_all.csv"))),
  colnames_j = c("scale", "data_source", "vary", "direction"),
  admin_to_keep = NULL,
  rng_seed = 2342, sims = 1000
)
write_create(all_preds$natl_preds,
             fp("analysis/out/sensitivity/vials_se.gz"),
             fwrite)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/sensitivity/*"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

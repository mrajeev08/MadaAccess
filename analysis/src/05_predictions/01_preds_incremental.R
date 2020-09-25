# ------------------------------------------------------------------------------
#' Getting incremental estimates of burden
#' Pulling in district and commune estimates of travel times as clinics are added
# ------------------------------------------------------------------------------

# sub_cmd=-t 12 -n 30 -jn preds -wt 5m

# Set it up
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = TRUE)

if(!set_up$slurm) fp <- here::here else fp <- cl_safe

cl <- make_cl(set_up$ncores)
register_cl(cl)

print(paste("Cluster size:", cl_size(cl)))

# pkgs
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
library(triangle)
library(glue)
source(here_safe("R/predict_functions.R"))
source(here_safe("R/batch_functions.R"))
select <- dplyr::select

# Preds given commune/district ttimes -------------------------------------------------------
scenario_loop <- unique(
  fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))$lookup
)

# make df with the lookup + mod pars (reverse vec so big ones at end don't slow things down)
lookup <- expand.grid(
  loop = rev(scenario_loop), scale = c("Commune", "District"),
  pop_predict = "flatPop", intercept = "fixed", data_source = "National",
  OD = TRUE, p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
  exp_min = 15 / 1e5, exp_max = 76 / 1e5, p_death = 0.16
)

all_preds <- run_scenarios(
  lookup = lookup, pred_type = c("vials", "burden"),
  par_type = "posterior", scaled = FALSE,
  ttimes_dir = fp("analysis/out/ttimes/addclinics/"),
  mod_dir = fp("analysis/out/mods/samps/"),
  colnames_max = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))),
  colnames_all = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_all.csv"))),
  colnames_j = c("scale", "data_source", "OD"),
  admin_to_keep = lookup$loop, # keep all admin ones for burden
  rng_seed = 23481, sims = 1000
)

write_create(all_preds$admin_preds, fp("analysis/out/preds/admin_preds.gz"),
             fwrite)
write_create(all_preds$admin_preds[scenario == 0], fp("analysis/out/preds/admin_preds_base.gz"),
             fwrite)
write_create(all_preds$natl_preds, fp("analysis/out/preds/natl_preds.gz"),
             fwrite)
write_create(all_preds$catch_preds, fp("analysis/out/preds/catch_preds.gz"),
             fwrite)
write_create(all_preds$prop_preds, fp("analysis/out/preds/prop_preds.gz"),
             fwrite)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/preds/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/preds/*"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

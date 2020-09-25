# ------------------------------------------------------------------------------------------------ #
#' Scaling of incidence and how this might drive burden
# ------------------------------------------------------------------------------------------------ #

# sub_cmd=-t 12 -n 30 -jn scaling_se -wt 2m -md "gdal"

# Set it up
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = TRUE)

if(!set_up$slurm) fp <- here::here else fp <- cl_safe

cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

# pkgs
library(doRNG)
library(glue)
library(sf)
library(data.table)
library(tidyverse)
library(foreach)
library(iterators)
library(triangle)
source(here_safe("R/predict_functions.R"))
source(here_safe("R/batch_functions.R"))

# Scaling factors ----------------------------------------------------------------------------
mada_communes <- st_read(fp("analysis/out/shapefiles/mada_communes.shp"))
mada_districts <- st_read(fp("analysis/out/shapefiles/mada_districts.shp"))

# Communes
pop <- mada_communes$pop - min(mada_communes$pop)
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 76 / 1e5
incidence_min <- 15 / 1e5
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ 0 + pop, offset = rep(15 / 1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset = rep(76 / 1e5, length(pop)))
neg_comm <- data.table(
  scaling = "neg", sfactor = neg$coefficients[1],
  scale = "Commune", type = "---", trans = min(mada_communes$pop)
)
pos_comm <- data.table(
  scaling = "pos", sfactor = pos$coefficients[1],
  scale = "Commune", type = "+++", trans = min(mada_communes$pop)
)

# Districts
pop <- mada_districts$pop - min(mada_districts$pop)
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ 0 + pop, offset = rep(15 / 1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset = rep(76 / 1e5, length(pop)))
neg_dist <- data.table(
  scaling = "neg", sfactor = neg$coefficients[1],
  scale = "District", type = "---", trans = min(mada_districts$pop)
)
pos_dist <- data.table(
  scaling = "pos", sfactor = pos$coefficients[1],
  scale = "District", type = "+++", trans = min(mada_districts$pop)
)
scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)
write_create(scaling_df, fp("analysis/out/sensitivity/scaling.csv"),
             write.csv, row.names = FALSE)

# Scaled preds --------------------------------------------------------------------------------
scenario_loop <- unique(fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))$lookup)

lookup <- expand_grid(
  loop = rev(scenario_loop), scaling_df, pop_predict = "flatPop",
  data_source = "National", intercept = "fixed", OD = TRUE,
  p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
  exp_min = 15 / 1e5, exp_max = 76 / 1e5, p_death = 0.16
)

all_preds <- run_scenarios(
  lookup = lookup, pred_type = "burden",
  par_type = "posterior", scaled = TRUE,
  ttimes_dir = fp("analysis/out/ttimes/addclinics"),
  mod_dir = fp("analysis/out/mods/samps/"),
  colnames_max = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_max.csv"))),
  colnames_all = colnames(fread(fp("analysis/out/ttimes/addclinics/commpreds_all.csv"))),
  colnames_j = c("scale", "data_source", "scaling"),
  admin_to_keep = "scenario_0",
  rng_seed = 23472, sims = 1000
)

# Write out data
write_create(all_preds$admin_preds, fp("analysis/out/sensitivity/burden_baseline_scaled.gz"),
             fwrite)
write_create(all_preds$natl_preds, fp("analysis/out/sensitivity/burden_addclinics_scaled.gz"),
             fwrite)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/sensitivity/*"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

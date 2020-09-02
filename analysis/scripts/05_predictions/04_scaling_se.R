# ------------------------------------------------------------------------------------------------ #
#' Scaling of incidence and how this might drive burden                          
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 4000 -sp "./R/05_predictions/04_scaling_se.R" -jn scaling_se -wt 5m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# libraries
library(doRNG)
library(glue)
library(sf)
library(data.table)
library(tidyverse)
library(foreach)
library(iterators)
library(triangle)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")
source("R/functions/batch_functions.R")

# Scaling factors ----------------------------------------------------------------------------
mada_communes <- st_read("data/processed/shapefiles/mada_communes.shp")
mada_districts <- st_read("data/processed/shapefiles/mada_districts.shp")

# Communes
pop <- mada_communes$pop - min(mada_communes$pop)
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 76/1e5
incidence_min <- 15/1e5
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ 0 + pop, offset=rep(15/1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset=rep(76/1e5, length(pop)))
neg_comm <- data.table(scaling = "neg", sfactor = neg$coefficients[1], 
                       scale = "Commune", type = "---", trans = min(mada_communes$pop))
pos_comm <- data.table(scaling = "pos", sfactor = pos$coefficients[1], 
                       scale = "Commune", type = "+++", trans = min(mada_communes$pop))

# Districts
pop <- mada_districts$pop - min(mada_districts$pop)
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ 0 + pop, offset=rep(15/1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset=rep(76/1e5, length(pop)))
neg_dist <- data.table(scaling = "neg", sfactor = neg$coefficients[1], 
                       scale = "District", type = "---", trans = min(mada_districts$pop))
pos_dist <- data.table(scaling = "pos", sfactor = pos$coefficients[1], 
                       scale = "District", type = "+++", trans = min(mada_districts$pop))
scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)
write.csv(scaling_df, "output/sensitivity/scaling.csv", row.names = FALSE)

# Scaled preds --------------------------------------------------------------------------------
scenario_loop <- unique(fread("output/ttimes/addclinics/commpreds_max.csv")$lookup)

lookup <- expand_grid(loop = rev(scenario_loop), scaling_df, pop_predict = "flatPop", 
                      data_source = "National", intercept = "fixed", OD = TRUE,
                      p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, 
                      exp_min = 15/1e5, exp_max = 76/1e5, p_death = 0.16)

all_preds <- run_scenarios(lookup = lookup, pred_type = "burden", 
                           par_type = "posterior", scaled = TRUE, directory = "output/ttimes/addclinics",
                           colnames_max = colnames(fread("output/ttimes/addclinics/commpreds_max.csv")), 
                           colnames_all = colnames(fread("output/ttimes/addclinics/commpreds_all.csv")),
                           colnames_j = c("scale", "data_source", "scaling"), 
                           admin_to_keep = "scenario_0", 
                           multicomb = function(x, ...) { mapply(rbind, x, ..., SIMPLIFY = FALSE)}, 
                           rng_seed = 23472, sims = 1000)

# Write out data
fwrite(all_preds$admin_preds, "output/sensitivity/burden_baseline_scaled.gz") 
fwrite(all_preds$natl_preds, "output/sensitivity/burden_addclinics_scaled.gz") 

# Close out
file_path <- "R/05_predictions/04_scaling_se.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")

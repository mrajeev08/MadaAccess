# ------------------------------------------------------------------------------------------------ #
#' Getting incremental estimates of burden  
#' Details: Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 4000 -sp "./R/05_predictions/01_burden_incremental.R" -jn burden -wt 5m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# Set up ------------------------------------------------------------------------------------
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
library(triangle)
library(glue)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")
source("R/functions/batch_functions.R")
select <- dplyr::select

# Set-up these two vectors for reading in data using cmd line args
scenario_loop <- unique(fread("output/ttimes/commune_maxcatch.csv")$lookup)
colnames <- colnames(fread("output/ttimes/commune_maxcatch.csv"))

# Preds given commune/district ttimes -------------------------------------------------------
# make df with the lookup + mod pars (reverse vec so big ones at end don't slow things down)
lookup <- expand.grid(loop = rev(scenario_loop), scale = c("Commune", "District"), 
                      pop_predict = "flatPop", intercept = "fixed", data_source = "National", 
                      OD = TRUE, p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, 
                      exp_min = 15/1e5, exp_max = 76/1e5, p_death = 0.16)

all_preds <- run_scenarios(lookup = lookup, pred_type = c("vials", "burden"), 
                           par_type = "posterior", scaled = FALSE, directory = "output/ttimes/",
                           colnames_max = colnames(fread("output/ttimes/commune_maxcatch.csv")), 
                           colnames_all = colnames(fread("output/ttimes/commune_allcatch.csv")),
                           colnames_j = c("scale", "data_source", "OD"), 
                           admin_to_keep = lookup$loop, # keep all admin ones for burden
                           multicomb = function(x, ...) { mapply(rbind, x, ..., SIMPLIFY = FALSE)}, 
                           rng_seed = 23481, sims = 1000)

fwrite(all_preds$admin_preds, "output/preds/admin_preds.gz")
fwrite(all_preds$natl_preds, "output/preds/natl_preds.gz")
fwrite(all_preds$catch_preds, "output/preds/catch_preds.gz")
fwrite(all_preds$prop_preds, "output/preds/prop_preds.gz")

# Saving session info
out.session(path = "R/05_predictions/01_preds_incremental.R", filename = "output/log_cluster.csv",
            start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/preds/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/preds/*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


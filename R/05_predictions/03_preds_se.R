# ------------------------------------------------------------------------------------------------ #
#' Getting sensitivity to parameter assumptions for burden & impact of expanding access                          
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 4000 -sp "./R/05_predictions/03_preds_se.R" -jn preds_se -wt 5m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# Set up ------------------------------------------------------------------------------------
library(data.table)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
library(triangle)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")
source("R/functions/batch_functions.R")
select <- dplyr::select

# Set-up these two vectors for reading in data using cmd line args
scenario_loop <- unique(fread("output/ttimes/commune_maxcatch.csv")$lookup)
se_pars <- fread("output/sensitivity/se_pars.csv")

# reverse so bigger dfs don't slow everything down
lookup <- expand_grid(loop = rev(scenario_loop), se_pars)

# Do burden -----------------------------------------------------------------
all_preds <- run_scenarios(lookup = lookup, pred_type = "burden", 
                           par_type = "point_est", scaled = FALSE, directory = "output/ttimes/",
                           colnames_max = colnames(fread("output/ttimes/commune_maxcatch.csv")), 
                           colnames_all = colnames(fread("output/ttimes/commune_allcatch.csv")),
                           colnames_j = c("scale", "data_source", "vary", "direction"), 
                           admin_to_keep = "scenario_0", 
                           multicomb = function(x, ...) { mapply(rbind, x, ..., SIMPLIFY = FALSE)}, 
                           rng_seed = 2342, sims = 1000)

# Write out data
fwrite(all_preds$admin_preds, "output/sensitivity/burden_baseline_se.gz") 
fwrite(all_preds$natl_preds, "output/sensitivity/burden_addclinics_se.gz") 

# Do vials -----------------------------------------------------------------
lookup <- filter(lookup, grepl("beta|sigma", vary))
vial_se_pars <- fread("output/sensitivity/se_pars.csv")[grep("beta|sigma", vary)]

all_preds <- run_scenarios(lookup = lookup, pred_type = "vials", 
                           par_type = "point_est", scaled = FALSE, catch_keep = FALSE,
                           directory = "output/ttimes/",
                           colnames_max = colnames(fread("output/ttimes/commune_maxcatch.csv")), 
                           colnames_all = colnames(fread("output/ttimes/commune_allcatch.csv")),
                           colnames_j = c("scale", "data_source", "vary", "direction"), 
                           admin_to_keep = NULL, 
                           multicomb = function(x, ...) { mapply(rbind, x, ..., SIMPLIFY = FALSE)}, 
                           rng_seed = 2342, sims = 1000)
fwrite(all_preds$natl_preds, "output/sensitivity/vials_se.gz")

# Close out
file_path <- "R/05_predictions/03_preds_se.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/burden*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")

####################################################################################################
##' Getting incremental estimates of burden 
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
#' Init MPI Backend
Sys.time()
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

#' #' SINGLE NODE
# rm(list = ls())
# library(doParallel)
# # args <- commandArgs(trailingOnly = TRUE)
# # cores <- as.integer(args[1])
# cores <- 3

##' Libraries and scripts
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(boot)
source("R/functions/predict_bites.R")

## Read in data
commune_master <- fread("output/ttimes/master_commune.csv")
setorder(commune_master, commune_id, scenario)
commune_master[, diff_comm := weighted_times - shift(weighted_times, 1), by = commune_id]
commune_master[, diff_dist := weighted_times_dist - shift(weighted_times_dist, 1), by = commune_id]
comm_run <- commune_master[diff_comm != 0 | scenario == 0]
dist_run <- commune_master[diff_dist != 0 | scenario == 0]
rm(commune_master) ## cleaning up memory!
gc()
library(tidyverse)

##' Get model means for commune and district models
model_ests <- read.csv("output/mods/bitemod_results.csv")
model_ests %>%
  dplyr::select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", intercept == "random") -> model_means

# # WITH SINGLE NODE
# cl <- makeCluster(cores)
# registerDoParallel(cl)
## Commune
params <- model_means[model_means$scale == "Commune", ]
predict.all(ttimes = comm_run$weighted_times/60, pop = comm_run$pop, catch = comm_run$base_catches, 
                names = comm_run$commune_id, beta_ttimes = params$beta_access, beta_0 = params$beta_0, 
                beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                pop_predict = params$pop_predict, intercept = params$intercept, 
                data_source = params$data_source, scale = params$scale, trans = 1e5, known_catch = FALSE, 
                p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, max_HDR = 25, min_HDR = 5, 
                dog_rabies_inc = 0.01, human_exp_rate = 0.39, prob_death = 0.16, nsims = 1000,
                outputs = c("bites", "deaths", "vials"), scenario = comm_run$scenario,
                seed = 678, max_scenario = 472) -> all_preds_comm
print("commune done")

## District
params <- model_means[model_means$scale == "District", ]
predict.all(ttimes = dist_run$weighted_times/60, pop = dist_run$pop, 
            catch = dist_run$base_catches, 
            names = dist_run$commune_id, beta_ttimes = params$beta_access, beta_0 = params$beta_0, 
            beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
            pop_predict = params$pop_predict, intercept = params$intercept, 
            data_source = params$data_source, scale = params$scale, trans = 1e5, known_catch = FALSE, 
            p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, max_HDR = 25, min_HDR = 5, 
            dog_rabies_inc = 0.01, human_exp_rate = 0.39, prob_death = 0.16, nsims = 1000,
            outputs = c("bites", "deaths", "vials"), scenario = dist_run$scenario,
            seed = 567, max_scenario = 472) -> all_preds_dist

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

all_preds <- multicomb(all_preds_comm, all_preds_dist)

fwrite(all_preds[["admin_dt"]], "output/preds/admin_preds_partial.csv")
fwrite(all_preds[["catch_dt"]], "output/preds/catch_preds_partial.csv")

# ##' WITH SINGLE NODE TO CLOSE
# stopCluster(cl)
# print("Done :)")
# Sys.time()

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()
####################################################################################################
##' Getting incremental estimates of burden 
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
##' Init MPI Backend
Sys.time()
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

##' Libraries and scripts
library(data.table)
library(foreach)
library(doRNG)
library(iterators)
library(boot)
source("R/functions/predict_bites.R")

## Read in data
commune_master <- fread("output/ttimes/master_commune.csv")
model_ests <- read.csv("output/mods/bitemod_results.csv")

##' Get model means for commune and district models
model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop", data_source == "National", intercept == "random") -> model_means

## This should for each model apply the respective params and get out all the preds for bites/deaths/vials
commune_master <- commune_master[scenario < 5]
comm_by_scenario <- split(commune_master, by = "scenario")

registerDoRNG(456) ## Reproducible within the loops?
foreach(params = iter(model_means, by = 'row'), .combine = multicomb) %do% {
  foreach(dt = iter(comm_by_scenario), .combine = multicomb) %do% {
    if(params$scale == "District") {
      dt$weighted_times <- dt$weighted_times_dist
    }
    predict.all(ttimes = dt$weighted_times/60, pop = dt$pop, catch = dt$catch_numeric, 
                names = dt$commune_id, beta_ttimes = params$beta_access, beta_0 = params$beta_0, 
                beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                pop_predict = params$pop_predict, intercept = params$intercept, 
                data_source = params$data_source, scale = params$scale, trans = 1e5, known_catch = FALSE, 
                p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, max_HDR = 25, min_HDR = 5, 
                dog_rabies_inc = 0.01, human_exp_rate = 0.39, prob_death = 0.16, nsims = 1000,
                outputs = c("bites", "deaths", "vials"), scenario = dt$scenario) -> predictions
  } -> scale_preds 
} -> all_preds_1

registerDoRNG(456) ## Reproducible within the loops?
foreach(params = iter(model_means, by = 'row'), .combine = multicomb) %do% {
  foreach(dt = iter(comm_by_scenario), .combine = multicomb) %do% {
    if(params$scale == "District") {
      dt$weighted_times <- dt$weighted_times_dist
    }
    predict.all(ttimes = dt$weighted_times/60, pop = dt$pop, catch = dt$catch_numeric, 
                names = dt$commune_id, beta_ttimes = params$beta_access, beta_0 = params$beta_0, 
                beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                pop_predict = params$pop_predict, intercept = params$intercept, 
                data_source = params$data_source, scale = params$scale, trans = 1e5, known_catch = FALSE, 
                p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, max_HDR = 25, min_HDR = 5, 
                dog_rabies_inc = 0.01, human_exp_rate = 0.39, prob_death = 0.16, nsims = 1000,
                outputs = c("bites", "deaths", "vials"), scenario = dt$scenario) -> predictions
  } -> scale_preds 
} -> all_preds_2

registerDoRNG(567) ## Reproducible within the loops?
foreach(params = iter(model_means, by = 'row'), .combine = multicomb) %do% {
  foreach(dt = iter(comm_by_scenario), .combine = multicomb) %do% {
    if(params$scale == "District") {
      dt$weighted_times <- dt$weighted_times_dist
    }
    predict.all(ttimes = dt$weighted_times/60, pop = dt$pop, catch = dt$catch_numeric, 
                names = dt$commune_id, beta_ttimes = params$beta_access, beta_0 = params$beta_0, 
                beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                pop_predict = params$pop_predict, intercept = params$intercept, 
                data_source = params$data_source, scale = params$scale, trans = 1e5, known_catch = FALSE, 
                p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98, max_HDR = 25, min_HDR = 5, 
                dog_rabies_inc = 0.01, human_exp_rate = 0.39, prob_death = 0.16, nsims = 1000,
                outputs = c("bites", "deaths", "vials"), scenario = dt$scenario) -> predictions
  } -> scale_preds 
} -> all_preds_3

identical(all_preds_1, all_preds_2) ## Should be TRUE
identical(all_preds_1, all_preds_3) ## Should be FALSE
identical(all_preds_2, all_preds_3) ## Should be FALSE

fwrite(all_preds_1, "output/burden/check.csv")

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()
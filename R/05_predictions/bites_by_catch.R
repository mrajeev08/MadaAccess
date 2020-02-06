####################################################################################################
##' Simulate bites and sum by catchment 
##' Details: deets 
##' Author: author 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
##' Init MPI Backend
Sys.time()
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

##' Libraries and scripts
library(doParallel)
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
source("R/functions/predict_functions.R")
source("R/functions/utils.R")
select <- dplyr::select

##' Pull in travel time estimates (for max catchments and proportional catchments)
##' ------------------------------------------------------------------------------------------------
commune_allcatch <- fread("output/ttimes/commune_allcatch.csv")
commune_maxcatch <- fread("output/ttimes/commune_maxcatch.csv")

##' Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "random") -> model_means
params <- model_means[model_means$scale == "Commune", ]

##' Commune predictions 
##' ------------------------------------------------------------------------------------------------
bite_mat <- predict.bites(ttimes = commune_maxcatch$weighted_times/60, pop = commune_maxcatch$pop, 
                          catch = commune_maxcatch$base_catches, names = commune_maxcatch$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
bites <- data.table(commune_id = commune_maxcatch$commune_id, scenario = commune_maxcatch$scenario,
                    bite_mat)
check <- commune_maxcatch[bites, on = c("commune_id", "scenario")]
check <- mutate_at(check, vars(result.1:result.1000), function(x) rpois(length(x), x*check$prop_pop_catch))
check %>%
  group_by(base_catches, scenario) %>%
  summarize_at(vars(result.1:result.1000), sum, na.rm = TRUE) -> bites_by_catch
fwrite(bites_by_catch, "output/preds/complete/bites_by_catch_comm.csv")

##' District predictions 
##' ------------------------------------------------------------------------------------------------
bite_mat <- predict.bites(ttimes = commune_maxcatch$weighted_times_dist/60, pop = commune_maxcatch$pop, 
                          catch = commune_maxcatch$base_catches, names = commune_maxcatch$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
bites <- data.table(commune_id = commune_maxcatch$commune_id, scenario = commune_maxcatch$scenario,
                    bite_mat)
check <- commune_maxcatch[bites, on = c("commune_id", "scenario")]
check <- mutate_at(check, vars(result.1:result.1000), function(x) x*check$prop_pop_catch)
check %>%
  group_by(base_catches, scenario) %>%
  summarize_at(vars(result.1:result.1000), sum, na.rm = TRUE) -> bites_by_catch
fwrite(bites_by_catch, "output/preds/complete/bites_by_catch_dist.csv")

##' Saving session info
out.session(path = "R/05_predictions/02_vials_incremental.R", filename = "sessionInfo.csv")
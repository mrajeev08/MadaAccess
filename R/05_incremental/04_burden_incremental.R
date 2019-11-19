####################################################################################################
##' Getting incremental estimates of burden 
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
rm(list = ls())

##' Libraries and scripts
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
source("R/functions/predict_bites.R")
select <- dplyr::select

## Read in data
commune_master <- fread("output/ttimes/master_commune.csv")
setorder(commune_master, commune_id, scenario)
commune_master[, diff_comm := weighted_times - shift(weighted_times, 1), by = commune_id]
commune_master[, diff_dist := weighted_times_dist - shift(weighted_times_dist, 1), by = commune_id]
comm_run <- commune_master[diff_comm != 0 | scenario %in% c(0, 1648)]
dist_run <- commune_master[diff_dist != 0 | scenario %in% c(0, 1648)]
rm(commune_master) ## cleaning up memory!
gc()

##' Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "random") -> model_means

##' Commune predictions 
##' ------------------------------------------------------------------------------------------------
params <- model_means[model_means$scale == "Commune", ]
bite_mat <- predict.bites(ttimes = comm_run$weighted_times/60, pop = comm_run$pop, 
                          catch = comm_run$base_catches, names = comm_run$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
all_mats <-  predict.deaths(bite_mat, pop = comm_run$pop,
                           p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
                           max_HDR = 35, min_HDR = 7, dog_rabies_inc = 0.01, 
                           human_exp_rate = 0.39, 
                           prob_death = 0.16)

all_mats <- c(list(bites = bite_mat), all_mats)

foreach(i = 1:length(all_mats), .combine = 'cbind', .packages = "data.table") %do% {
    mat <- all_mats[[i]]
    labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
    mean <- rowMeans(mat, na.rm = TRUE) ## mean for each row = admin unit
    sd <- apply(mat, 1, sd, na.rm = TRUE)
    upper <- mean + 1.96*sd/sqrt(ncol(mat))
    lower <- mean - 1.96*sd/sqrt(ncol(mat))
    out <- data.table(mean, upper, lower)
    names(out) <- labels
    out
  } -> admin_comm

admin_comm <- data.table(names = comm_run$commune_id,
                         ttimes = comm_run$weighted_times/60, pop = comm_run$pop, 
                         catch = comm_run$base_catches, scenario = comm_run$scenario, 
                         scale = "Commune", admin_comm)

  
##' For simulating vials and doing scenario analysis
catch_comm <- data.table(bite_mat, catch = comm_run$base_catches, scenario = comm_run$scenario, 
                       names = comm_run$commune_id)
catch_comm %>%
  complete(scenario = 0:472, names) %>%
  group_by(names) %>%
  arrange(scenario) %>%
  fill(3:ncol(catch_comm), .direction = "down") -> catch_comm
catch_comm <- as.data.table(catch_comm)[, names := NULL]
catch_comm <- catch_comm[, lapply(.SD, sum, na.rm = TRUE), by = c("catch", "scenario")]
catch_mat <- as.matrix(catch_comm[, -c("catch", "scenario"), with = FALSE])

## Vials
catch_comm[, check := rowSums(catch_mat)]
setorder(catch_comm, catch, scenario)
catch_comm[, diff := check - shift(check, 1), by = c("catch", "scenario")]
catch_comm <- catch_comm[diff != 0 | scenario %in% c(0, 1648)]
catch_mat <- as.matrix(catch_comm[, -c("catch", "scenario", "check", "diff"), with = FALSE])

vials <- apply(catch_mat, c(1, 2), get.vials)
mean <- rowMeans(vials, na.rm = TRUE) ## mean for each row = admin unit
sd <- apply(vials, 1, sd, na.rm = TRUE)
upper <- mean + 1.96*sd/sqrt(ncol(vials))
lower <- mean - 1.96*sd/sqrt(ncol(vials))
vials_comm <- data.table(vials_mean = mean, vials_upper = upper, vials_lower = lower,
                          catch = catch_comm$catch, scenario = catch_comm$scenario, 
                          scale = "Commune")

##' District preds 
##' ------------------------------------------------------------------------------------------------
params <- model_means[model_means$scale == "District", ]
bite_mat <- predict.bites(ttimes = dist_run$weighted_times/60, pop = dist_run$pop, 
                          catch = dist_run$base_catches, names = dist_run$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
all_mats <-  predict.deaths(bite_mat, pop = dist_run$pop,
                            p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
                            max_HDR = 35, min_HDR = 7, dog_rabies_inc = 0.01, 
                            human_exp_rate = 0.39, 
                            prob_death = 0.16)

all_mats <- c(list(bites = bite_mat), all_mats)

foreach(i = 1:length(all_mats), .combine = 'cbind', .packages = "data.table") %do% {
  mat <- all_mats[[i]]
  labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
  mean <- rowMeans(mat, na.rm = TRUE) ## mean for each row = admin unit
  sd <- apply(mat, 1, sd, na.rm = TRUE)
  upper <- mean + 1.96*sd/sqrt(ncol(mat))
  lower <- mean - 1.96*sd/sqrt(ncol(mat))
  out <- data.table(mean, upper, lower)
  names(out) <- labels
  out
} -> admin_dist

admin_dist <- data.table(names = dist_run$commune_id,
                         ttimes = dist_run$weighted_times/60, pop = dist_run$pop, 
                         catch = dist_run$base_catches, scenario = dist_run$scenario, 
                         scale = "District", admin_dist)

##' For simulating vials and doing scenario analysis
catch_dist <- data.table(bite_mat, catch = dist_run$base_catches, scenario = dist_run$scenario, 
                         names = dist_run$commune_id)
catch_dist %>%
  complete(scenario = 0:472, names) %>%
  group_by(names) %>%
  arrange(scenario) %>%
  fill(3:ncol(catch_dist), .direction = "down") -> catch_dist
catch_dist <- as.data.table(catch_dist)[, names := NULL]
catch_dist <- catch_dist[, lapply(.SD, sum, na.rm = TRUE), by = c("catch", "scenario")]
catch_mat <- as.matrix(catch_dist[, -c("catch", "scenario"), with = FALSE])

## Vials
catch_dist[, check := rowSums(catch_mat)]
setorder(catch_dist, catch, scenario)
catch_dist[, diff := check - shift(check, 1), by = c("catch", "scenario")]
catch_dist <- catch_dist[diff != 0 | scenario %in% c(0, 1648)]
catch_mat <- as.matrix(catch_dist[, -c("catch", "scenario", "check", "diff"), with = FALSE])

vials <- apply(catch_mat, c(1, 2), get.vials)
mean <- rowMeans(vials, na.rm = TRUE) ## mean for each row = admin unit
sd <- apply(vials, 1, sd, na.rm = TRUE)
upper <- mean + 1.96*sd/sqrt(ncol(vials))
lower <- mean - 1.96*sd/sqrt(ncol(vials))
vials_dist <- data.table(vials_mean = mean, vials_upper = upper, vials_lower = lower,
                          catch = catch_dist$catch, scenario = catch_dist$scenario, 
                          scale = "District")

##' Output results 
##' ------------------------------------------------------------------------------------------------
vials_bycatch_incremental <- rbind(vials_dist, vials_comm)
fwrite(vials_bycatch_incremental, "output/preds/partial/vials_bycatch_partial.csv")
results_incremental <- rbind(admin_dist, admin_comm)
fwrite(results_incremental, "output/preds/partial/burden_partial.csv")

# ------------------------------------------------------------------------------------------------ #
#' Getting incremental estimates of burden  
#' Details: Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

# Set up ------------------------------------------------------------------------------------
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
library(triangle)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")
select <- dplyr::select

# Filter to only do ones for which travel times have changed
commune_master <- fread("output/ttimes/commune_maxcatch.gz")
setorder(commune_master, commcode, scenario)
commune_master[, diff_comm := ttimes_wtd - shift(ttimes_wtd, 1), by = commcode]
commune_master[, diff_dist := ttimes_wtd_dist - shift(ttimes_wtd_dist, 1), by = commcode]
comm_run <- commune_master[diff_comm != 0 | scenario %in% c(0, 1648)]
dist_run <- commune_master[diff_dist != 0 | scenario %in% c(0, 1648)]
rm(commune_master) # cleaning up memory!
gc()

# Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "random") -> model_means

# Commune predictions -----------------------------------------------------------------------
params <- model_means[model_means$scale == "Commune", ]

bite_mat <- predict.bites(ttimes = comm_run$ttimes_wtd/60, pop = comm_run$pop, 
                          catch = comm_run$catchment, names = comm_run$commcode, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")
all_mats <-  predict.deaths(bite_mat, pop = comm_run$pop,
                           p_rab_min = 0.2, p_rab_max = 0.6,
                           rho_max = 0.98, exp_min = 15/1e5, exp_max = 76/1e5,
                           prob_death = 0.16, dist = "uniform")

all_mats <- c(list(bites = bite_mat), all_mats)

foreach(i = 1:length(all_mats), .combine = 'cbind', .packages = "data.table") %do% {
    mat <- all_mats[[i]]
    labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
    mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
    sd <- apply(mat, 1, sd, na.rm = TRUE)
    upper <- mean + 1.96*sd/sqrt(ncol(mat))
    lower <- mean - 1.96*sd/sqrt(ncol(mat))
    out <- data.table(mean, upper, lower)
    names(out) <- labels
    out
  } -> admin_comm

admin_comm <- data.table(names = comm_run$commcode,
                         ttimes = comm_run$ttimes_wtd/60, pop = comm_run$pop, 
                         catch = comm_run$catchment, scenario = comm_run$scenario, 
                         scale = "Commune", admin_comm)

# District predictions -----------------------------------------------------------------------
params <- model_means[model_means$scale == "District", ]
bite_mat <- predict.bites(ttimes = dist_run$ttimes_wtd/60, pop = dist_run$pop, 
                          catch = dist_run$catchment, names = dist_run$commcode, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")
all_mats <-  predict.deaths(bite_mat, pop = dist_run$pop,
                            p_rab_min = 0.2, p_rab_max = 0.6,
                            rho_max = 0.98, exp_min = 15/1e5, exp_max = 76/1e5, 
                            prob_death = 0.16, dist = "uniform")

all_mats <- c(list(bites = bite_mat), all_mats)

foreach(i = 1:length(all_mats), .combine = 'cbind', .packages = "data.table") %do% {
  mat <- all_mats[[i]]
  labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
  mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
  sd <- apply(mat, 1, sd, na.rm = TRUE)
  upper <- mean + 1.96*sd/sqrt(ncol(mat))
  lower <- mean - 1.96*sd/sqrt(ncol(mat))
  out <- data.table(mean, upper, lower)
  names(out) <- labels
  out
} -> admin_dist

admin_dist <- data.table(names = dist_run$commcode,
                         ttimes = dist_run$ttimes_wtd/60, pop = dist_run$pop, 
                         catch = dist_run$catchment, scenario = dist_run$scenario, 
                         scale = "District", admin_dist)

# Complete results
# ------------------------------------------------------------------------------------------------
burden_partial <- rbind(admin_dist, admin_comm)
max_clinics <- max(comm_run$scenario[comm_run$scenario != 1648])

burden_partial %>%
  complete(scenario = 1:max_clinics, names, scale) %>% # from 1 to last
  group_by(names, scale) %>%
  arrange(scenario) %>%
  fill(4:ncol(burden_partial), .direction = "down") -> burden_complete

## This should be true!
burden_complete %>%
  group_by(scenario, scale) %>%
  summarize(n_admin = n()) %>%
  group_by(scale) %>%
  summarize(all(n_admin == 1579)) # should be true for all of them!

fwrite(burden_complete, "output/preds/burden.gz")

##' Saving session info
out.session(path = "R/05_predictions/01_burden.R", filename = "output/log_local.csv")


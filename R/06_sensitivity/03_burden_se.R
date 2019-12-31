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
source("R/functions/predict_functions.R")
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

##' Scaling factors 
##' ------------------------------------------------------------------------------------------------
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

## Communes
pop <- mada_communes$pop
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 0.01*0.39/7
incidence_min <- 0.01*0.39/25
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ pop) ## use these and constrain
neg <- lm(neg_scale ~ pop) ## use these and constrain
neg_comm <- data.table(scaling = "neg", sfactor = neg$coefficients[2], intercept = incidence_max,
                       scale = "Commune", type = "---")
pos_comm <- data.table(scaling = "pos", sfactor = pos$coefficients[2], intercept = incidence_min,
                       scale = "Commune", type = "+++")
max_comm <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_max,
                       scale = "Commune", type = "max")
min_comm <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_min,
                       scale = "Commune", type = "min")
## Districts
pop <- mada_districts$pop
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ pop) ## use these and constrain
neg <- lm(neg_scale ~ pop) ## use these and constrain
neg_dist <- data.table(scaling = "neg", sfactor = neg$coefficients[2], intercept = incidence_max, 
                       scale = "District", type = "---")
pos_dist <- data.table(scaling = "pos", sfactor = pos$coefficients[2], intercept = incidence_min,
                       scale = "District", type = "+++")
max_dist <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_max,
                       scale = "District", type = "max")
min_dist <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_min,
                       scale = "District", type = "min")

scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)
flat_df <- rbind(max_comm, max_dist, min_comm, min_dist)
pop_plot <- seq(0, 1e6, by = 1000)
constrained_inc <- function(slope, intercept, pop, max, min){
  inc <- slope*pop + intercept
  inc[inc >= max] <- max
  inc[inc <= min] <- min
  return(inc)
}

foreach(vals = iter(scaling_df, by = "row"), .combine = rbind) %do% {
  preds <- constrained_inc(vals$sfactor, vals$intercept, pop_plot, incidence_max,
                           incidence_min)*1e5
  data.table(vals, preds, pop_plot)
} -> predicted_inc

write.csv(predicted_inc, "output/sensitivity/scaling.csv", row.names = FALSE)
incidence_df <- rbind(flat_df, scaling_df)

##' Commune predictions 
##' ------------------------------------------------------------------------------------------------
params <- model_means[model_means$scale == "Commune", ]
bite_mat_comm <- predict.bites(ttimes = comm_run$weighted_times/60, pop = comm_run$pop, 
                          catch = comm_run$base_catches, names = comm_run$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
params <- model_means[model_means$scale == "District", ]
bite_mat_dist <- predict.bites(ttimes = dist_run$weighted_times/60, pop = dist_run$pop, 
                               catch = dist_run$base_catches, names = dist_run$commune_id, 
                               beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                               beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                               pop_predict = params$pop_predict, intercept = params$intercept, 
                               trans = 1e5, known_catch = FALSE, nsims = 1000)

## scale vals + min/max HDR
## p_rabid min/max fixed
## rho_max min/max fixed
rho_max_vals <- c(0.85, 0.99)
p_rabid_vals <- c(0.2, 0.6)

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(i = 1:length(p_rabid_vals), .combine = multicomb) %:%
foreach(k = iter(incidence_df, by = "row"), .combine = multicomb) %:%
foreach(j = 1:length(rho_max_vals), .combine = multicomb) %do% {
  ## With max + min scaling!  
  if(k$scale == "District") {
    bite_mat <- bite_mat_dist
    admin_df <- dist_run
  } else {
    bite_mat <- bite_mat_comm
    admin_df <- comm_run
  }
  death_mat <- get.burden.fixed(bite_mat, pop = admin_df$pop, hdr = 25, 
                               incidence = 0.01, exp_rate = 0.39, p_rabid = p_rabid_vals[i],
                               rho_max = rho_max_vals[j], prob_death = 0.16, scale = TRUE, 
                               slope = k$sfactor, intercept = k$intercept, 
                               inc_max = incidence_max, 
                               inc_min = incidence_min)
  mean <- rowMeans(death_mat, na.rm = TRUE) ## mean for each row = admin unit
  sd <- apply(death_mat, 1, sd, na.rm = TRUE)
  upper <- mean + 1.96*sd/sqrt(ncol(death_mat))
  lower <- mean - 1.96*sd/sqrt(ncol(death_mat))
  out <- data.table(deaths_mean = mean, deaths_upper = upper, deaths_lower = lower,
                    scenario = admin_df$scenario, pop = admin_df$pop,
                    names = admin_df$commune_id, ttimes = admin_df$weighted_times)
  out_base <- data.table(scaling = k$sfactor, intercept = k$intercept, rho_max = rho_max_vals[j], 
                         p_rabid = p_rabid_vals[i], type = k$type, scale = k$scale,
                         out[scenario == 0])
  
  ## Fill down with predictions
  out %>%
    complete(scenario = 0:472, names) %>%
    group_by(names) %>%
    arrange(scenario) %>%
    fill(starts_with("deaths"), .direction = "down") %>%
    ungroup() %>%
    group_by(scenario) %>%
    summarize_at(vars(starts_with("deaths")), sum, na.rm = TRUE) -> out
  out <- data.table(scaling = k$sfactor, intercept = k$intercept, rho_max = rho_max_vals[j], 
                    p_rabid = p_rabid_vals[i], type = k$type, scale = k$scale, out)
  list(base = out_base, incremental = out)
} -> sensitivity

base_se <- sensitivity[["base"]]
incremental_se <- sensitivity[["incremental"]]
incremental_se$scenario[incremental_se$scenario 
                                 == max(incremental_se$scenario)] <- 600
incremental_se %>%
  group_by(p_rabid, rho_max, scale, type) %>%
  mutate(deaths_base = deaths_mean[scenario == 0],
         deaths_upper_base = deaths_upper[scenario == 0], 
         deaths_lower_base = deaths_lower[scenario == 0]) %>%
  complete(scenario = 472:600) -> incremental_se
                                      
fwrite(base_se, "output/sensitivity/baseline_se.csv")
fwrite(incremental_se, "output/sensitivity/incremental_se.csv")

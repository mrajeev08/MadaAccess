####################################################################################################
##' Getting incremental estimates of burden 
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list=ls())

##' Libraries and packages
library(data.table)
library(tidyverse)
library(rgdal)
library(foreach)
library(iterators)
library(boot)
library(ggridges)
source("R/functions/predict_bites.R")

## District travel times
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")
commune_df <- fread("output/ttimes/incremental_commune.csv")
district_df <- fread("output/ttimes/incremental_district.csv")
model_ests <- read.csv("output/mods/bitemod_results.csv")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")

## Get model means for commune and district models
model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop", data_source == "National", intercept == "random") -> model_means

## Filter to a single catchment
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(district_id, scenario)]
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(commune_id, scenario)]

## Fix so that catches are numeric within the scenarios
district_df[, catch_numeric := as.numeric(as.factor(base_catches)), by = scenario]
commune_df[, catch_numeric := as.numeric(as.factor(base_catches)), by = scenario]

## Match district ttimes to commune ids to get 
mada_communes$match_id <- 1:nrow(mada_communes@data)
commune_df$distcode <- mada_communes$distcode[match(commune_df$commune_id, mada_communes$match_id)]
mada_districts$match_id <- 1:nrow(mada_districts@data)
district_df$distcode <- mada_districts$distcode[match(district_df$district_id, mada_districts$match_id)]
district_df_merge <- district_df[, 
                                 c("distcode", "weighted_times", "scenario"), 
                                 with = FALSE][, setnames(.SD, "weighted_times", "weighted_times_dist")]
commune_df <- commune_df[district_df_merge, on = c("scenario", "distcode")]
comm_by_scenario <- split(commune_df, by = "scenario")

## This should for each model apply the respective params and get out all the preds for bites/deaths/vials
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
}-> all_preds

## Single catchment
commune_max <- fread("output/ttimes/max_commune.csv")
commune_max <- commune_max[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                             by = .(commune_id, scenario)]

## Fix so that catches are numeric within the scenarios
commune_max[, catch_numeric := as.numeric(as.factor(max_catches))]

predict.deaths(access = commune_max$weighted_times/60, ctar_in = 0, 
               pop = commune_max$pop, catch = commune_max$catch_numeric, 
               names = commune_max$commune_id, 
               group_name = commune_max$commune_id, 
               beta_access = model_means$beta_access[model_means$scale == "Commune"], 
               beta_ctar = 0, beta_0 = model_means$beta_0[model_means$scale == "Commune"], beta_pop = 0, 
               sigma_0 = model_means$sigma_0[model_means$scale == "Commune"], 
               known_alphas = NA, 
               covar_name = "ttimes", pop_predict = "flatPop", intercept = "random",
               summed = FALSE, ctar_bump = FALSE, data_source = "National", 
               scale = "Commune",
               trans = 1e5, known_catch = FALSE, 
               p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
               max_HDR = 25, min_HDR = 12, 
               dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
               prob_death = 0.16, nsims = 1000, scenario = commune_max$scenario) -> comm_max
sum(comm_max$deaths_mean, na.rm = TRUE)

## Hypotheticals
comm_model <- model_means[model_means$scale == "District", ]
ttimes <- seq(0, 15, by = 0.2)
pop_plot <- 1e5
int_upper95 <- comm_model$beta_0 + 1.96*comm_model$sigma_0
int_lower95 <- comm_model$beta_0 - 1.96*comm_model$sigma_0
exp_bites_max <- exp(int_upper95 + comm_model$beta_access*ttimes)*pop_plot
exp_bites_min <- exp(int_lower95 + comm_model$beta_access*ttimes)*pop_plot

reporting_min <- ifelse(0.2*exp_bites_min/78 > 1, 0.98, 0.2*exp_bites_min/78)
reporting_max <- ifelse(0.2*exp_bites_max/78 > 1, 0.98, 0.2*exp_bites_max/78)

plot(ttimes, reporting_min, ylim = c(0, 1))
points(ttimes, reporting_max, col = "blue")

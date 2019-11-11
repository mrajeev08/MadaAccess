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
source("R/functions/predict_bites.R")

## District travel times
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")
district_df <- fread("output/ttimes/incremental_district.csv")
commune_df <- fread("output/ttimes/incremental_commune.csv")
model_ests <- read.csv("output/mods/bitemod_results.csv")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

## Get model means for commune and district models
model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop", data_source == "National", intercept == "random") -> model_means

## Single catchment
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(district_id, scenario)]
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(commune_id, scenario)]

## Fix so that catches are numeric within the scenarios
district_df[, catch_numeric := as.numeric(as.factor(base_catches)), by = scenario]
commune_df[, catch_numeric := as.numeric(as.factor(base_catches)), by = scenario]

## Burden for district model
predict.all(ttimes = district_df$weighted_times/60, 
               pop = district_df$pop, catch = district_df$catch_numeric, 
               names = district_df$district_id, 
               beta_ttimes = model_means$beta_access[model_means$scale == "District"], 
               beta_0 = model_means$beta_0[model_means$scale == "District"], beta_pop = 0, 
               sigma_0 = model_means$sigma_0[model_means$scale == "District"], 
               known_alphas = NA, pop_predict = "flatPop", intercept = "random",
               data_source = "National", 
               scale = "District",
               trans = 1e5, known_catch = FALSE, 
               p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
               max_HDR = 25, min_HDR = 5, 
               dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
               prob_death = 0.16, nsims = 1000, scenario = district_df$scenario) -> district_deaths


district_deaths <- data.table(district_deaths)
district_grouped <- district_deaths[, .(deaths_mean = sum(deaths_mean)), by = scenario]
ggplot(data = district_grouped, aes(x = scenario, y = deaths_mean)) + geom_line()


## Single catchment
district_max <- fread("output/ttimes/max_district.csv")
district_max <- district_max[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(district_id, scenario)]

## Fix so that catches are numeric within the scenarios
district_max[, catch_numeric := as.numeric(as.factor(max_catches))]

predict.deaths(access = district_max$weighted_times/60, ctar_in = 0, 
               pop = district_max$pop, catch = district_max$catch_numeric, 
               names = district_max$district_id, 
               group_name = district_max$district_id, 
               beta_access = model_means$beta_access[model_means$scale == "District"], 
               beta_ctar = 0, beta_0 = model_means$beta_0[model_means$scale == "District"], beta_pop = 0, 
               sigma_0 = model_means$sigma_0[model_means$scale == "District"], 
               known_alphas = NA, 
               covar_name = "ttimes", pop_predict = "flatPop", intercept = "random",
               summed = FALSE, ctar_bump = FALSE, data_source = "National", 
               scale = "District",
               trans = 1e5, known_catch = FALSE, 
               p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
               max_HDR = 25, min_HDR = 5, 
               dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
               prob_death = 0.16, nsims = 1000, scenario = district_max$scenario) -> district_max
sum(district_max$deaths_mean, na.rm = TRUE)

## Burden for commune model
predict.deaths(access = commune_df$weighted_times/60, ctar_in = 0, 
               pop = commune_df$pop, catch = commune_df$catch_numeric, 
               names = commune_df$commune_id, 
               group_name = commune_df$commune_id, 
               beta_access = model_means$beta_access[model_means$scale == "Commune"], 
               beta_ctar = 0, beta_0 = model_means$beta_0[model_means$scale == "Commune"], beta_pop = 0, 
               sigma_0 = model_means$sigma_0[model_means$scale == "Commune"], 
               known_alphas = NA, 
               covar_name = "ttimes", pop_predict = "flatPop", intercept = "random",
               summed = FALSE, ctar_bump = FALSE, data_source = "National", 
               scale = "Commune",
               trans = 1e5, known_catch = FALSE, 
               p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
               max_HDR = 25, min_HDR = 5, 
               dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
               prob_death = 0.16, nsims = 1000, scenario = commune_df$scenario) -> commune_deaths


commune_deaths <- data.table(commune_deaths)
commune_grouped <- commune_deaths[, .(deaths_mean = sum(deaths_mean)), by = scenario]
ggplot(data = commune_grouped, aes(x = scenario, y = deaths_mean)) + geom_line() + expand_limits(y = 0)


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

## Hypothetica
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

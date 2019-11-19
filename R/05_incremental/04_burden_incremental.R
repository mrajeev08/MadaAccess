####################################################################################################
##' Getting incremental estimates of burden 
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
#' #' Init MPI Backend
#' Sys.time()
#' rm(list = ls())
#' library(doMPI)
#' cl <- startMPIcluster()
#' clusterSize(cl) # this just tells you how many you've got
#' registerDoMPI(cl)

#' #' SINGLE NODE
rm(list = ls())
library(doParallel)
# args <- commandArgs(trailingOnly = TRUE)
# cores <- as.integer(args[1])
cores <- 3

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
commune_master <- commune_master[scenario %in% c(0, 1, 10, 100, 200, 400, 472, 1648)]

# setorder(commune_master, commune_id, scenario)
# commune_master[, diff_comm := weighted_times - shift(weighted_times, 1), by = commune_id]
# commune_master[, diff_dist := weighted_times_dist - shift(weighted_times_dist, 1), by = commune_id]
# comm_run <- commune_master[diff_comm != 0 | scenario == 0]
# dist_run <- commune_master[diff_dist != 0 | scenario == 0]
# comm_run <- commune_master
# rm(commune_master) ## cleaning up memory!
# gc()
library(tidyverse)

##' Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "fixed") -> model_means

# # WITH SINGLE NODE
cl <- makeCluster(cores)
registerDoParallel(cl)

## Commune
params <- model_means[model_means$scale == "Commune", ]
comm_run <- commune_master
bite_mat <- predict.bites(ttimes = comm_run$weighted_times/60, pop = comm_run$pop, 
                          catch = comm_run$base_catches, names = comm_run$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
deaths <-  predict.deaths(bite_mat, pop = comm_run$pop,
                           p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.99,
                           max_HDR = 25, min_HDR = 5, dog_rabies_inc = 0.01, 
                           human_exp_rate = 0.39, 
                           prob_death = 0.16)

all_mats <- c(list(bites = bite_mat), deaths)

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
  } -> admin_dt

admin_dt %>%
  mutate(scenario = comm_run$scenario) %>%
  group_by(scenario) %>%
  summarize_all(sum, na.rm = TRUE) -> natl_dt

admin_dt <- data.table(names = comm_run$commune_id,
                       ttimes = comm_run$weighted_times/60, pop = comm_run$pop, 
                       catch = comm_run$base_catches, scenario = comm_run$scenario, admin_dt)


## Get national 
natl_mats <- all_mats[c("deaths", "averted", "bites")]
foreach(i = 1:length(natl_mats), .combine = 'cbind', .export = c('get.boots.dt', 'boots_mean'),
        .packages = c('boot', 'data.table')) %do% {
          mat <- natl_mats[[i]]
          natl_dt <- data.table(mat, scenario = comm_run$scenario)
          natl_dt %>% 
            group_by(scenario) %>%
            summarize_all(sum, na.rm = TRUE) -> natl_dt
          labels <-  paste0(names(natl_mats)[i], "_", c("mean", "upper", "lower"))
          mat <- as.matrix(natl_dt[, 2:ncol(natl_dt)])
          out <- apply(mat, 1, get.boots.dt, names = labels, type_CI = "basic", nsims = 1000)
          out <- data.table(scenario = natl_dt$scenario, rbindlist(out))
  } -> natl_dt

natl_dt <- natl_dt[, -c(5, 9), with = FALSE]
ggplot(data = natl_dt, aes(x = scenario, y = deaths_mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = deaths_lower, 
                  ymax = deaths_upper), alpha = 0.5, fill = "blue")


## District
params <- model_means[model_means$scale == "District", ]
comm_run <- commune_master
bite_mat <- predict.bites(ttimes = comm_run$weighted_times_dist/60, pop = comm_run$pop, 
                          catch = comm_run$base_catches, names = comm_run$commune_id, 
                          beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                          beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                          pop_predict = params$pop_predict, intercept = params$intercept, 
                          trans = 1e5, known_catch = FALSE, nsims = 1000)
deaths <-  predict.deaths(bite_mat, pop = comm_run$pop,
                          p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.99,
                          max_HDR = 25, min_HDR = 5, dog_rabies_inc = 0.01, 
                          human_exp_rate = 0.39, 
                          prob_death = 0.16)

all_mats <- c(list(bites = bite_mat), deaths)

foreach(i = 1:length(all_mats), .combine = 'cbind', .packages = "data.table") %dopar% {
  mat <- all_mats[[i]]
  labels <- paste0(names(all_mats)[i], "_", c("mean", "sd"))
  mean <- rowMeans(mat, na.rm = TRUE) ## mean for each row = admin unit
  sd <- apply(mat, 1, sd, na.rm = TRUE)
  out <- data.table(mean, sd)
  names(out) <- labels
  out
} -> admin_dt

admin_dt <- data.table(names = comm_run$commune_id,
                       ttimes = comm_run$weighted_times/60, pop = comm_run$pop, 
                       catch = comm_run$base_catches, scenario = comm_run$scenario, admin_dt)
admin_dt %>%
  group_by(scenario) %>%
  summarize_at(vars(contains("deaths")), sum, na.rm = TRUE) -> natl_deaths

ggplot(data = natl_deaths, aes(x = scenario, y = deaths_mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = deaths_mean - 2*deaths_sd/sqrt(1000), 
                  ymax = deaths_mean + 2*deaths_sd/sqrt(1000)), alpha = 0.5, fill = "blue")

## Get national 
natl_mats <- all_mats[c("deaths", "averted", "bites")]
foreach(i = 1:length(natl_mats), .combine = 'cbind', .export = c('get.boots.dt', 'boots_mean'),
        .packages = c('boot', 'data.table')) %do% {
          mat <- natl_mats[[i]]
          natl_dt <- data.table(mat, scenario = comm_run$scenario)
          natl_dt %>% 
            group_by(scenario) %>%
            summarize_all(sum, na.rm = TRUE) -> natl_dt
          labels <-  paste0(names(natl_mats)[i], "_", c("mean", "upper", "lower"))
          mat <- as.matrix(natl_dt[, 2:ncol(natl_dt)])
          out <- apply(mat, 1, get.boots.dt, names = labels)
          out <- data.table(scenario = natl_dt$scenario, rbindlist(out))
        } -> natl_dt

natl_dt <- natl_dt[, -c(5, 9), with = FALSE]
ggplot(data = natl_dt, aes(x = scenario, y = deaths_mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = deaths_lower, 
                  ymax = deaths_upper), alpha = 0.5, fill = "blue")


##' For simulating vials!
foreach(i = 1:length(natl_mats), .combine = 'cbind', .export = c('get.boots.dt', 'boots_mean'),
        .packages = c('boot', 'data.table')) %do% {
          
  mat <- natl_mats[[i]]
  natl_dt <- data.table(mat, scenario = comm_run$scenario, 
                        names = comm_run$commune_id)
  natl_dt %>%
    complete(scenario = 0:472, names) %>%
    group_by(names) %>%
    arrange(scenario) %>%
    fill(3:ncol(natl_dt), .direction = "down") -> natl_dt
  natl_dt %>% 
    ungroup() %>%
    select(-names) %>%
    group_by(scenario) %>%
    summarize_all(sum, na.rm = TRUE) -> natl_dt
  labels <-  paste0(names(natl_mats)[i], "_", c("mean", "upper", "lower"))
  natl_mat <- as.matrix(natl_dt[, 2:ncol(natl_dt)])
  out <- apply(natl_mat, 1, get.boots.dt, names = labels)
  out <- data.table(scenario = natl_dt$scenario, rbindlist(out))
} -> natl_dt

natl_dt <- natl_dt[, -c(5, 9), with = FALSE]
natl_dt %>%
  complete(scenario = 1:1648) -> natl_dt

ggplot(data = natl_dt, aes(x = scenario, y = bites_mean)) + 
  geom_line() +
  scale_x_continuous(breaks = c(0, 10, 100, 200, 400, 472, 1648), 
                    labels = c(0, 10, 100, 200, 400, 472, "max (1648)"),
                    trans = "sqrt") +
  geom_ribbon(aes(ymin = bites_lower, ymax = bites_upper), alpha = 0.5, fill = "blue") +
  ggforce::geom_link(aes(x = 472, y = 475.256, xend = 1648, 
                         yend = 382.927), color = "blue", alpha = 0.5, linetype = 2) +
  theme(panel.grid.minor.x = element_blank())

catch_dt <- data.table(bite_mat, catch, scenario, names)
catch_dt %>%
  complete(scenario = 0:max_scenario, names) %>%
  group_by(names) %>%
  arrange(scenario) %>%
  fill(3:ncol(catch_dt), .direction = "down") -> catch_dt
catch_dt <- as.data.table(catch_dt)[, names := NULL]
catch_dt <- catch_dt[, lapply(.SD, sum, na.rm = TRUE), by = c("catch", "scenario")]
catch_dt <- unique(catch_dt, by = 3:ncol(catch_dt))
catches <- catch_dt$catch
scenarios <- catch_dt$scenario
catch_mat <- as.matrix(catch_dt[, c("catch", "scenario") := NULL])

## Simulate vials 

## Aggregate

## Complete...and summarize @ natl level...


print("commune done")

## District
params <- model_means[model_means$scale == "District", ]
predict.all(ttimes = dist_run$weighted_times_dist/60, pop = dist_run$pop, 
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

## Hmmmm
ggplot() +
  ggridges::geom_density_ridges(data = commune_master, 
                      aes(x = weighted_times/60, y = as.factor(scenario)), 
                      alpha = 0.5) 


ggplot() +
  geom_hline(aes(yintercept = sum(admin_dt$deaths_mean)/sum(admin_dt$pop)*1e5), linetype = 1, 
             color = "#004b49", alpha = 0.75, size = 1.2) +
  geom_point(data = admin_dt, 
             aes(x = as.factor(order(names, ttimes)), y = deaths_mean/pop*1e5, 
                 fill = ttimes, stroke = 1.1)) +
  scale_fill_viridis_c(option = "viridis", direction = 1,
                       name = "Travel times \n (hrs)", limits=c(0, 15), oob = scales::squish)  +
  scale_shape_manual(values = c(22, 23), name = "Model scale") +
  scale_alpha_manual(values = c(0.85, 1), name = "Model scale") +
  scale_size_manual(values = c(2.5, 3.5), name = "Model scale") +
  scale_color_manual(values = c("darkgrey", "black"), name = "Model scale") +
  labs(x = "Districts (ordered by \n increasing travel times)", 
       y = "Predicted incidence of \n deaths per 100k", tag = "A") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), text = element_text(size=20)) +
  coord_flip(clip = "off")
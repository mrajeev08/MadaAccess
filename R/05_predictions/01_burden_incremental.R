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
model_ests <- read.csv("output/mods/estimates_adj_OD.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_means
model_means <- bind_rows(filter(model_means, data_source == "National", scale == "District",
                                 intercept == "random"), 
                         filter(model_means, data_source == "Moramanga"))
model_ests %>%
  select(params, sd_adj, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = sd_adj, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_sds_adj
model_sds_adj <- bind_rows(filter(model_sds_adj, data_source == "National", scale == "District",
                                intercept == "random"), filter(model_sds_adj, 
                                                               data_source == "Moramanga"))

# All predictions -----------------------------------------------------------------------
foreach(par = iter(model_means, by = "row"), 
        sds = iter(model_sds_adj, by = "row"), .combine = rbind, .options.RNG = 1434) %dorng% {
          
          if(par$data_source == "Moramanga"){
            admin <- comm_run # these are data.tables so hopefully will not copy only point!
            ttimes <- admin$ttimes_wtd/60
            OD_dist <- TRUE # accounting for overdispersion
          }
          
          if(par$scale == "District"){
            admin <- dist_run
            ttimes <- admin$ttimes_wtd_dist/60
            OD_dist <- FALSE # not accounting for overdispersion
          }

          bite_mat <- predict.bites(ttimes = ttimes, pop = admin$pop, 
                                    catch = admin$catchment, names = admin$commcode, 
                                    beta_ttimes = par$beta_ttimes, beta_ttimes_sd = sds$beta_ttimes,
                                    beta_0 = par$beta_0, beta_0_sd = sds$beta_0, 
                                    beta_pop = 0, beta_pop_sd = sds$beta_pop, 
                                    sigma_0 = par$sigma_0, known_alphas = NA, 
                                    pop_predict = "flatPop", intercept = "random", dist = OD_dist,
                                    trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")
          
          all_mats <-  predict.deaths(bite_mat, pop = admin$pop,
                                      p_rab_min = 0.2, p_rab_max = 0.6,
                                      rho_max = 0.98, exp_min = 15/1e5, exp_max = 76/1e5,
                                      prob_death = 0.16, dist = "triangle")
          
          all_mats <- c(list(bites = bite_mat), all_mats)
          
          foreach(i = 1:length(all_mats), .combine = 'cbind') %do% {
            mat <- all_mats[[i]]
            labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
            mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
            sd <- apply(mat, 1, sd, na.rm = TRUE)
            upper <- apply(mat, 1, quantile, prob = 0.975)
            lower <- apply(mat, 1, quantile, prob = 0.025)
            out <- data.table(mean, upper, lower)
            names(out) <- labels
            out
          } -> admin_comm
          
          admin_comm <- data.table(names = admin$commcode,
                                   ttimes = ttimes, pop = admin$pop, 
                                   catch = admin$catchment, scenario = admin$scenario, 
                                   scale = par$scale, data_source = par$data_source,
                                   admin_comm)
          
          max_clinics <- max(admin$scenario[admin$scenario != 1648])
          
          admin_comm %>%
            complete(scenario = 1:max_clinics, names) %>% # from 1 to last
            group_by(names) %>%
            arrange(scenario) %>%
            fill(3:ncol(admin_comm), .direction = "down") -> admin_comm
          
          ## This should be true!
          admin_comm %>%
            group_by(scenario) %>%
            summarize(n_admin = n()) %>%
            summarize(all(n_admin == 1579)) # should be true for all of them!
          
          admin_comm
          
  } -> burden_complete

fwrite(burden_complete, "output/preds/burden_all.gz")
burden_base <- filter(burden_complete, scenario == 0)
fwrite(burden_base, "output/preds/burden_base.gz")

##' Saving session info
out.session(path = "R/05_predictions/01_burden.R", filename = "output/log_local.csv")


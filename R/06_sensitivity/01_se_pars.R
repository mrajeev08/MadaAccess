# ------------------------------------------------------------------------------------------------ #
#' Parameter ranges for univariate sensitivity analyses          
# ------------------------------------------------------------------------------------------------ #

# Set up 
library(data.table)
library(tidyverse)
library(foreach)
library(iterators)
select <- dplyr::select
source("R/functions/out.session.R")


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
model_ests %>%
  select(params, SD, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = SD, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_sds_unadj

model_sds_adj <- bind_rows(filter(model_sds_unadj, data_source == "National", scale == "District",
                                  intercept == "random"), filter(model_sds_adj, 
                                                                 data_source == "Moramanga"))

# Univariate sensitivity ---------------------------------------------------------------------
foreach(means = iter(model_means, by = "row"), sds = iter(model_sds_adj, by = "row"), 
        .combine = rbind) %do% {
          
  base <- data.table(exp_min = 15/1e5, exp_max = 76/1e5, p_rab_min = 0.2, p_rab_max = 0.6,
                     rho_max = 0.98, p_death = 0.16, 
                     sigma_0 = means$sigma_0,
                     beta_ttimes = means$beta_ttimes, 
                     beta_0 = means$beta_0)
  
  vary <-  c("human_exp", "p_rabid", "rho_max", "p_death", "sigma_0", "beta_ttimes", "beta_0")
  direction <- c("min", "max")
  se_pars <- expand_grid(base, vary, direction)
  
  se_pars %>%
    mutate(exp_max = case_when(vary == "human_exp" & direction == "min" ~ 15/1e5, 
                               vary == "human_exp" & direction == "max" ~ 110/1e5,
                               vary != "human_exp" ~ exp_max),
           exp_min = case_when(vary == "human_exp" & direction == "min" ~ 15/1e5, 
                               vary == "human_exp" & direction == "max" ~ 110/1e5,
                               vary != "human_exp" ~ exp_min),
           p_rab_max = ifelse(vary == "p_rabid" & direction == "min", p_rab_min, p_rab_max),
           p_rab_min = ifelse(vary == "p_rabid" & direction == "max", p_rab_max, p_rab_min),
           rho_max = case_when(vary == "rho_max" & direction == "min" ~ 0.8,
                               vary == "rho_max" & direction == "max" ~ 1.0,
                               vary != "rho_max" ~ rho_max),
           p_death = case_when(vary == "p_death" & direction == "min" ~ 0.13, # from gavi paper
                               vary == "p_death" & direction == "max" ~ 0.20, 
                               vary != "p_death" ~ p_death),
           beta_0 = case_when(vary == "beta_0" & 
                                direction == "min" ~ means$beta_0 - 1.96*sds$beta_0, 
                               vary == "beta_0" & 
                                direction == "max" ~ means$beta_0 + 1.96*sds$beta_0, 
                               vary != "beta_0" ~ beta_0),
           beta_ttimes = case_when(vary == "beta_ttimes" & 
                                direction == "min" ~ means$beta_0 - 1.96*sds$beta_0, 
                              vary == "beta_ttimes" & 
                                direction == "max" ~ means$beta_0 + 1.96*sds$beta_0, 
                              vary != "beta_ttimes" ~ beta_ttimes), 
           sigma_0 = case_when(vary == "sigma_0" & direction == "min" ~ means$sigma_0 - 1.96*sds$sigma_0,
                               vary == "sigma_0" & direction == "max" ~ means$sigma_0 + 1.96*sds$sigma_0,
                               vary != "sigma_0" ~ sigma_0),
           scale = scale[i])-> se_pars
} -> se_pars

write.csv(se_pars, "output/sensitivity/se_pars.csv", row.names = FALSE)

##' Saving session info
out.session(path = "R/06_sensitivity/03_burden_se_pars.R", filename = "output/log_local.csv")



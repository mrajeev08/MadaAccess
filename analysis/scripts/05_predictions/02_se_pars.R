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
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  filter(pop_predict == "flatPop", OD == TRUE, 
         intercept == "fixed", data_source == "National") %>%
  mutate(mod_id = as.character(interaction(pop_predict, OD, intercept, data_source, scale))) -> model_ests

# Univariate sensitivity ---------------------------------------------------------------------
foreach(pars = split(model_ests, model_ests$mod_id), .combine = rbind) %do% {

  base <- data.table(exp_min = 15/1e5, exp_max = 76/1e5, p_rab_min = 0.2, p_rab_max = 0.6,
                     rho_max = 0.98, p_death = 0.16, 
                     sigma_e = pars$Mean[pars$params == "sigma_e"],
                     beta_ttimes = pars$Mean[pars$params == "beta_ttimes"], 
                     beta_0 = pars$Mean[pars$params == "beta_0"])
  
  vary <-  c("human_exp", "p_rabid", "rho_max", "p_death", "sigma_e", "beta_ttimes", "beta_0")
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
                                direction == "min" ~ pars$quant_2.5[pars$params == "beta_0"], 
                               vary == "beta_0" & 
                                direction == "max" ~ pars$quant_97.5[pars$params == "beta_0"], 
                               vary != "beta_0" ~ pars$Mean[pars$params == "beta_0"]),
           beta_ttimes = case_when(vary == "beta_ttimes" & 
                                direction == "min" ~ pars$quant_2.5[pars$params == "beta_ttimes"], 
                              vary == "beta_ttimes" & 
                                direction == "max" ~ pars$quant_97.5[pars$params == "beta_ttimes"], 
                              vary != "beta_ttimes" ~ pars$Mean[pars$params == "beta_ttimes"]), 
           sigma_e = case_when(vary == "sigma_e" & direction == "min" ~ pars$quant_2.5[pars$params == "sigma_e"],
                               vary == "sigma_e" & direction == "max" ~ pars$quant_97.5[pars$params == "sigma_e"],
                               vary != "sigma_e" ~ pars$Mean[pars$params == "sigma_e"]),
           scale = pars$scale[1], data_source = pars$data_source[1], intercept = pars$intercept[1], 
           pop_predict = pars$pop_predict[1], OD = pars$OD[1]) -> se_pars
} -> se_pars

write.csv(se_pars, "output/sensitivity/se_pars.csv", row.names = FALSE)

# Saving session info
out.session(path = "R/05_predictions/02_se_pars.R", filename = "output/log_local.csv")



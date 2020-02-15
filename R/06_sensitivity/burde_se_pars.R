# ------------------------------------------------------------------------------------------------ #
#' Parameter ranges for univariate sensitivity analyses          
# ------------------------------------------------------------------------------------------------ #

# Set up 
library(data.table)
library(tidyverse)
select <- dplyr::select

# Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  filter(pop_predict == "flatPop", data_source == "National", intercept == "random",
         params %in% c("beta_0", "beta_ttimes", "sigma_0"))-> ests

# Univariate sensitivity ---------------------------------------------------------------------
scale <- c("Commune", "District")

foreach(i = 1:length(scale), .combine = rbind) %do% {
  params <- data.table(ests[ests$scale == scale[i], ])
  base <- data.table(exp_min = 15/1e5, exp_max = 76/1e5, p_rab_min = 0.2, p_rab_max = 0.6,
                     rho_max = 0.98, p_death = 0.16, sigma_0 = params[params == "sigma_0"]$Mean,
                     beta_ttimes = params[params == "beta_ttimes"]$Mean, 
                     beta_0 = params[params == "beta_0"]$Mean)
  vary <-  c("human_exp", "p_rabid", "rho_max", "p_death", "sigma_0", "beta_ttimes", "beta_0")
  direction <- c("min", "max")
  se_pars <- expand_grid(base, vary, direction)
  se_pars %>%
    mutate(exp_max = case_when(vary == "human_exp" & direction == "min" ~ 10/1e5, 
                               vary == "human_exp" & direction == "max" ~ 110/1e5,
                               vary != "human_exp" ~ exp_max),
           exp_min = case_when(vary == "human_exp" & direction == "min" ~ 10/1e5, 
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
           sigma_0 = case_when(vary == "sigma_0" & direction == "min" ~ params[params == "sigma_0"]$quant_97.5, 
                               vary == "sigma_0" & direction == "max" ~ params[params == "sigma_0"]$quant_2.5, 
                               vary != "sigma_0" ~ sigma_0),
           beta_0 = case_when(vary == "beta_0" & 
                                direction == "min" ~ params[params == "beta_0"]$quant_97.5, 
                               vary == "beta_0" & 
                                direction == "max" ~ params[params == "beta_0"]$quant_2.5, 
                               vary != "beta_0" ~ beta_0),
           beta_ttimes = case_when(vary == "beta_ttimes" & 
                                direction == "min" ~ params[params == "beta_ttimes"]$quant_97.5, 
                              vary == "beta_ttimes" & 
                                direction == "max" ~ params[params == "beta_ttimes"]$quant_2.5, 
                              vary != "beta_ttimes" ~ beta_ttimes), 
           scale = scale[i])-> se_pars
} -> se_pars

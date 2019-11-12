####################################################################################################
##' Plotting scenario shifts with ggridges
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
district_df$scale <- "District"
commune_df$scale <- "Commune"
scenario_to_plot <- rbind(district_df, commune_df, fill = TRUE)

## ggridges to show shifts in distribution of max catchments
ggplot() +
  geom_density_ridges(data = scenario_to_plot[scenario %in% c(1, 100, 200, 300, 472)], 
                      aes(x = prop_pop_catch, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5) +
  xlim(c(0, 1)) +
  scale_fill_manual(values = c("#cc7722", "#004b49"), name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, "max")) +
  labs(y = "Number of clinics added", x = "Maximum proportion \n of population in clinic catchment")

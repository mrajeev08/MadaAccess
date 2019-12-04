####################################################################################################
##' Post processing predictions 
##' Details: Filling in data frames by missing scenarios and admin units (or catchment depending) 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up 
##' ------------------------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
admin_preds_part <- fread("output/preds/partial/burden_partial.csv")
vials_preds_part <- fread("output/preds/partial/vials_bycatch_partial.csv")

## Fill down with predictions
admin_preds_part %>%
  complete(scenario = 0:472, names, scale) %>%
  group_by(names, scale) %>%
  arrange(scenario) %>%
  fill(4:ncol(admin_preds_part), .direction = "down") -> admin_filled

## This should be true!
nrow(admin_filled) == 1579*474*2
fwrite(admin_filled, "output/preds/complete/burden_filled.csv")

## Fix max scenario so that it is 473
commune_master <- fread("output/ttimes/master_commune.csv")
commune_master %>%
  select(catch = base_catches, scenario) %>%
  group_by(catch, scenario) %>%
  summarize(count = n()) -> match_df
match_df$scale <- "Commune"
match_df_dist <- match_df
match_df_dist$scale <- "District"
match_df <- bind_rows(match_df, match_df_dist)

## Fill down with predictions
catch_filled <- left_join(match_df, vials_preds_part)
catch_filled %>%
  group_by(catch, scale) %>%
  arrange(scenario) %>%
  fill(starts_with("vials"), .direction = "down") -> catch_filled
fwrite(catch_filled, "output/preds/complete/vials_filled.csv")




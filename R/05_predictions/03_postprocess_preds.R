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
select <- dplyr::select
admin_preds_part <- fread("output/preds/partial/burden_partial.csv")

## Fill down with predictions
admin_preds_part %>%
  complete(scenario = 0:472, names, scale) %>%
  group_by(names, scale) %>%
  arrange(scenario) %>%
  fill(4:ncol(admin_preds_part), .direction = "down") -> admin_filled

## This should be true!
nrow(admin_filled) == 1579*474*2
fwrite(admin_filled, "output/preds/complete/burden_filled.csv")


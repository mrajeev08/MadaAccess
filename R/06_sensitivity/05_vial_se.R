# ------------------------------------------------------------------------------------------------ #
#' Vial sensitivity (i.e. what is an upper limit on the # of vials you might need)
#' May also be worth thinking about doing an se analyses with the bite data params
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
commune_master <- fread("output/ttimes/commune_allcatch.gz")
comm_run <- commune_master[scenario == max(scenario)]
rm(commune_master) # cleaning up memory!
gc()

# max incidence from moramanga = 800/1e5
comm_run %>%
  mutate(bites = 1000/1e5*comm_run$pop, 
         bites_by_catch = bites*prop_pop_catch) %>%
  group_by(catchment) %>%
  summarize(bites_by_catch = rpois(1, sum(bites_by_catch, na.rm = TRUE))) -> max_bites

vial_preds <- sapply(max_bites$bites_by_catch, get.vials)
comm_run$vials <- unlist(vial_preds["vials", ])
comm_run$throughput <- unlist(vial_preds["throughput", ])

fwrite(comm_run, "output/sensitivity/vials_max.csv")

# rep this?

# Also look at sensitivity to bite estimates? 

# Speed up by aggregating bites first (in matrix form!)

# If not changed then only do the vials


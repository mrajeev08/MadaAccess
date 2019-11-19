####################################################################################################
##' Estimating vials
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
rm(list = ls())

##' Libraries and scripts
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
source("R/functions/predict_bites.R")
select <- dplyr::select

## Commune
##' For simulating vials and doing scenario analysis
catch_comm <- fread("output/preds/partial/bites_bycatch_comm.csv")

## Filter to only the catchments that have changed
catch_comm[, check := rowSums(catch_mat)]
setorder(catch_comm, catch, scenario)
catch_comm[, diff := (check - shift(check, 1)), by = catch]
catch_comm <- catch_comm[diff != 0 | scenario %in% c(0, 1648)]
catch_mat <- as.matrix(catch_comm[, -c("catch", "scenario", "check", "diff"), with = FALSE])

## Getting vials
## Simulate vials at admin level
foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, .export = 'get.vials', 
        .packages = 'data.table') %dopar% {
          vials <- unlist(sapply(bycatch, get.vials))
          mean <- mean(vials, na.rm = TRUE)
          sd <- sd(vials, na.rm = TRUE)
          upper <- mean + 1.96*sd/sqrt(ncol(vials))
          lower <- mean - 1.96*sd/sqrt(ncol(vials))
          out <- data.table(vials_mean = mean, vials_upper = upper, vials_lower = lower)
 } -> vials_comm

vials_comm <- data.table(catch = catch_comm$catch, scenario = catch_comm$scenario,
                         scale = "Commune", vials_comm)


vials <- apply(catch_mat, c(1, 2), get.vials)
mean <- rowMeans(vials, na.rm = TRUE) ## mean for each row = admin unit
sd <- apply(vials, 1, sd, na.rm = TRUE)
upper <- mean + 1.96*sd/sqrt(ncol(vials))
lower <- mean - 1.96*sd/sqrt(ncol(vials))
vials_comm <- data.table(vials_mean = mean, vials_upper = upper, vials_lower = lower,
                         catch = catch_comm$catch, scenario = catch_comm$scenario, 
                         scale = "Commune")

## District
catch_dist <- fread("output/preds/partial/bites_bycatch_dist.csv")
catch_mat <- as.matrix(catch_dist[, -c("catch", "scenario"), with = FALSE])

## Filter to only the catchments that have changed
catch_dist[, check := rowSums(catch_mat)]
setorder(catch_dist, catch, scenario)
catch_dist[, diff := (check - shift(check, 1)), by = catch]
catch_dist <- catch_dist[diff != 0 | scenario %in% c(0, 1648)]
catch_mat <- as.matrix(catch_dist[, -c("catch", "scenario", "check", "diff"), with = FALSE])

vials <- apply(catch_mat, c(1, 2), get.vials)
mean <- rowMeans(vials, na.rm = TRUE) ## mean for each row = admin unit
sd <- apply(vials, 1, sd, na.rm = TRUE)
upper <- mean + 1.96*sd/sqrt(ncol(vials))
lower <- mean - 1.96*sd/sqrt(ncol(vials))
vials_dist <- data.table(vials_mean = mean, vials_upper = upper, vials_lower = lower,
                         catch = catch_dist$catch, scenario = catch_dist$scenario, 
                         scale = "District")

##' Output results 
##' ------------------------------------------------------------------------------------------------
vials_bycatch_incremental <- rbind(vials_dist, vials_comm)
fwrite(vials_bycatch_incremental, "output/preds/partial/vials_bycatch_partial.csv")
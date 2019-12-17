####################################################################################################
##' Estimating vials
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
rm(list = ls())

##' Init MPI Backend
Sys.time()
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

##' Libraries and scripts
library(doParallel)
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
source("R/functions/predict_bites.R")
source("R/functions/utils.R")
select <- dplyr::select

## Commune
##' For simulating vials and doing scenario analysis
catch_comm <- fread("output/preds/partial/bites_bycatch_comm.csv")
catch_mat <- as.matrix(catch_comm[, -c("catch", "scenario", "check", "diff"), with = FALSE])

# ## Parallelize
# cores <- 3
# cl <- makeCluster(cores)
# registerDoParallel(cl)

## Simulate vials at admin level
foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, .export = 'get.vials', 
        .packages = 'data.table') %dopar% {
          vial_preds <- sapply(bycatch, get.vials)
          vials <- unlist(vial_preds["vials", ])
          throughput <- unlist(vial_preds["throughput", ])
          vials_mean <- mean(vials, na.rm = TRUE)
          vials_sd <- sd(vials, na.rm = TRUE)
          vials_upper <- vials_mean + 1.96*vials_sd/sqrt(length(vials))
          vials_lower <- vials_mean - 1.96*vials_sd/sqrt(length(vials))
          
          throughput_mean <- mean(throughput, na.rm = TRUE)
          throughput_sd <- sd(throughput, na.rm = TRUE)
          throughput_upper <- throughput_mean + 1.96*throughput_sd/sqrt(length(throughput))
          throughput_lower <- throughput_mean - 1.96*throughput_sd/sqrt(length(throughput))
          
          out <- data.table(vials_mean, vials_upper, vials_lower, 
                            throughput_mean, throughput_sd, throughput_upper, throughput_lower)
 } -> vials_comm

# stopCluster(cl)

vials_comm <- data.table(catch = catch_comm$catch, scenario = catch_comm$scenario,
                         scale = "Commune", vials_comm)

## District
catch_dist <- fread("output/preds/partial/bites_bycatch_dist.csv")
catch_mat <- as.matrix(catch_dist[, -c("catch", "scenario", "check", "diff"), with = FALSE])

# ## Parallelize
# cores <- 3
# cl <- makeCluster(cores)
# registerDoParallel(cl)

## Simulate vials at admin level
foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, .export = 'get.vials', 
        .packages = 'data.table') %dopar% {
          vial_preds <- sapply(bycatch, get.vials)
          vials <- unlist(vial_preds["vials", ])
          throughput <- unlist(vial_preds["throughput", ])
          vials_mean <- mean(vials, na.rm = TRUE)
          vials_sd <- sd(vials, na.rm = TRUE)
          vials_upper <- vials_mean + 1.96*vials_sd/sqrt(length(vials))
          vials_lower <- vials_mean - 1.96*vials_sd/sqrt(length(vials))
          
          throughput_mean <- mean(throughput, na.rm = TRUE)
          throughput_sd <- sd(throughput, na.rm = TRUE)
          throughput_upper <- throughput_mean + 1.96*throughput_sd/sqrt(length(throughput))
          throughput_lower <- throughput_mean - 1.96*throughput_sd/sqrt(length(throughput))
          
          out <- data.table(vials_mean, vials_upper, vials_lower, 
                            throughput_mean, throughput_sd, throughput_upper, throughput_lower)
        } -> vials_dist

# stopCluster(cl)

vials_dist <- data.table(catch = catch_dist$catch, scenario = catch_dist$scenario,
                         scale = "District", vials_dist)

##' Output results 
##' ------------------------------------------------------------------------------------------------
vials_bycatch_incremental <- rbind(vials_dist, vials_comm)
fwrite(vials_bycatch_incremental, "output/preds/partial/vials_bycatch_partial.csv")

##' Saving session info
out.session(path = "R/05_vials_incremental.R", filename = "sessionInfo.csv")

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

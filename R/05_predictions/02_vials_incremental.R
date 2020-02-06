####################################################################################################
##' Estimating vials
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------

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
library(dplyr)
source("R/functions/predict_functions.R")
source("R/functions/utils.R")
select <- dplyr::select

##' Vials given commune ttimes
##' ------------------------------------------------------------------------------------------------
bites_by_catch <- fread("output/preds/complete/bites_by_catch_comm.csv")
bites_by_catch %>%
  ungroup() %>%
  select(result.1:result.1000) %>%
  as.matrix(.) -> catch_mat

foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, .export = 'get.vials', 
        .packages = 'data.table', .options.RNG = 8887) %dorng% {
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

fwrite(vials_comm, "output/preds/complete/vials_comm.csv")

##' Vials given district ttimes
##' ------------------------------------------------------------------------------------------------
bites_by_catch <- fread("output/preds/complete/bites_by_catch_dist.csv")
bites_by_catch %>%
  ungroup() %>%
  select(result.1:result.1000) %>%
  as.matrix(.) -> catch_mat

foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, .export = 'get.vials', 
        .packages = 'data.table', .options.RNG = 8888) %dorng% {
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

fwrite(vials_dist, "output/preds/complete/vials_dist.csv")

##' Saving session info
out.session(path = "R/05_predictions/02_vials_incremental.R", filename = "sessionInfo.csv")

##' Close out cluster
##' 
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()

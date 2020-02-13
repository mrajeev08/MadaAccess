# ------------------------------------------------------------------------------------------------ #
#' Run models of bite incidence using spatial covariates
#' Details: Models include travel times in addition to population 
# ------------------------------------------------------------------------------------------------ #

# Pull in command line arguments on how to run this script
args <- commandArgs(trailingOnly = TRUE)
type <- args[1]

if(type == "local") {
  library(doParallel)
  cl <- makeCluster(detectCores() - 1, type = "FORK")
  registerDoParallel(cl)
} 

if(type == "remote") {
  # Init MPI Backend
  Sys.time()
  library(doMPI)
  cl <- startMPIcluster()
  clusterSize(cl) # this just tells you how many you've got
  registerDoMPI(cl)
}

if(type == "serial") {
  print("Warning: will run without parallel backend")
}

# libraries
library(data.table)
library(dplyr)
library(foreach)
library(iterators)
library(rjags)

# source bite ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")
source("R/functions/estimate_pars.R")
source("R/functions/out.session.R")

# Run all Mada models -----------------------------------------------------------------------
# Mada data
# These all should have same index letter
covars_list <- list(district_bites, comm_covars)
scale <- c("District", "Commune")
sum_it <- c(FALSE, TRUE)

# The other index letters
intercept_type <- c("random", "fixed")
pop_predict <- c("addPop", "onlyPop", "flatPop")

# For each of those opts
mods_mada <- 
  foreach(i = 1:length(covars_list), .combine = "rbind") %:%
  foreach(k = 1:length(pop_predict), .combine = "rbind") %:%
  foreach(l = 1:length(intercept_type), .combine = "rbind", 
          .packages = 'rjags') %dopar% {
    covar_df <- covars_list[[i]]
    ttimes <- covar_df$ttimes_wtd/60
    
    out <- estimate.pars(bites = round(district_bites$avg_bites), 
                           ttimes = ttimes, pop = covar_df$pop, 
                           start = district_bites$start, end = district_bites$end,
                           ncovars = nrow(covar_df), 
                           nlocs = nrow(district_bites), catch = covar_df$catch, 
                           ncatches = max(district_bites$catch), pop_predict =  pop_predict[k], 
                           intercept = intercept_type[l], summed = sum_it[i], 
                           data_source = "National",
                           scale = scale[i], trans = 1e5, 
                           chains = 4, adapt = 1000, iter = 15000, thinning = 2,
                           dic = TRUE, save = TRUE)
    
    samps <- out[["samps"]]
    dic <- out[["dic"]]
    
    dic_est <- mean(dic$deviance) + mean(dic$penalty)
    
    samp_summ <- summary(samps)
    params <- rownames(samp_summ$statistics)
    diag <- gelman.diag(samps)
    
    samp_df <- as.data.frame(list(params = params, samp_summ$statistics, 
                                  neff = effectiveSize(samps),
                                  quant_2.5 = samp_summ$quantiles[, 5], 
                                  quant_97.5 = samp_summ$quantiles[, 1],
                                  psrf_est = diag$psrf[, 1], 
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf,  
                                  pop_predict = pop_predict[k], intercept = intercept_type[l], 
                                  summed = sum_it[i], 
                                  data_source = "National", 
                                  scale = scale[i], dic = dic_est))
  }
    

# Moramanga models -------------------------------------------------------------------------------
# These all should have same index letter
scale <- "Commune"
sum_it <- FALSE
intercept_type <- "fixed"

# The other index letters
pop_predict <- c("addPop", "onlyPop", "flatPop")

mods_mora <- 
  foreach(k = 1:length(pop_predict), .combine = "rbind") %dopar% {
    
    ttimes <- mora_bites$ttimes_wtd/60
    
    out <- estimate.pars(bites = round(mora_bites$avg_bites), 
                         ttimes = ttimes, pop = mora_bites$pop, 
                         start = mora_bites$start, end = mora_bites$end,
                         ncovars = nrow(mora_bites), 
                         nlocs = nrow(mora_bites), catch = mora_bites$catch, 
                         ncatches = max(mora_bites$catch), pop_predict =  pop_predict[k], 
                         intercept = intercept_type, summed = sum_it, 
                         data_source = "Moramanga",
                         scale = scale, trans = 1e5, 
                         chains = 4, adapt = 1000, iter = 15000, thinning = 2,
                         dic = TRUE, save = TRUE)
    
    samps <- out[["samps"]]
    dic <- out[["dic"]]
    
    dic_est <- mean(dic$deviance) + mean(dic$penalty)
    
    samp_summ <- summary(samps)
    params <- rownames(samp_summ$statistics)
    diag <- gelman.diag(samps)
    
    samp_df <- as.data.frame(list(params = params, samp_summ$statistics, 
                                  neff = effectiveSize(samps),
                                  quant_2.5 = samp_summ$quantiles[, 5], 
                                  quant_97.5 = samp_summ$quantiles[, 1],
                                  psrf_est = diag$psrf[, 1], 
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf, 
                                  pop_predict = pop_predict[k], intercept = intercept_type,
                                  summed = sum_it, data_source = "Moramanga", 
                                  scale = scale, dic = dic_est))
  }

mods_all <- bind_rows(mods_mada, mods_mora)
write.csv(mods_all, "output/mods/estimates.csv", row.names = FALSE)

# Close out 
file_path <- "R/03_bitemodels/01_run_bitemods.R"

if(type == "serial") {
  print("Done serially:)")
  fname <- "output/log_local.csv"
} 

if(type == "local") {
  stopCluster(cl)
  print("Done locally:)")
  fname <- "output/log_local.csv"
} 

if (type == "remote") {
  closeCluster(cl)
  mpi.quit()
  print("Done remotely:)")
  Sys.time()
  fname <- "output/log_cluster.csv"
}
out.session(path = file_path, filename = fname)

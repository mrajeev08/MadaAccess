####################################################################################################
##' Run models of bite incidence using spatial covariates 
##' Details: Models include travel times and distance as access metrics, in addition to population 
##' Author: Malavika Rajeev 
####################################################################################################
##' Init MPI Backend
Sys.time()
rm(list = ls())
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)

## source bite ests
source("R/02_bitedata/03_estimate_biteinc.R") # either source this or output bite data
# probs want to output bite data in order to pull into other things
source("R/functions/estimate_pars.R")

## libraries
library(foreach)
library(iterators)
library(rjags)

##' Run all models
##' ----------------------------------------------------------------------------------------
##' Mada data
## These all should have same index letter
bite_df <- bites_by_ttimes
covars_list <- list(bites_by_ttimes, commcovars_by_ttimes)
scale <- c("District", "Commune")
sum_it <- c(FALSE, TRUE)

## The other index letters
intercept_type <- c("random", "fixed")
pop_predict <- c("addPop", "onlyPop", "flatPop")

mods_mada <- 
  foreach(i = 1:length(covars_list), .combine = "rbind") %:%
  foreach(k = 1:length(pop_predict), .combine = "rbind") %:%
  foreach(l = 1:length(intercept_type), .combine = "rbind", 
          .packages = 'rjags') %dopar% {
    
    covar_df <- covars_list[[i]]
    access <- covar_df$covar/60
    
    out <- estimate.pars(bites = round(bite_df$avg_bites), 
                           access = access, ctar_in = covar_df$ctar_in_district, pop = covar_df$pop, 
                           start = bite_df$start, end = bite_df$end, ncovars = nrow(covar_df), 
                           nlocs = nrow(bite_df), catch = covar_df$catch, ncatches = max(bite_df$catch), 
                           covar_name = covar_df$covar_name[1], pop_predict =  pop_predict[k], 
                           intercept = intercept_type[l], summed = sum_it[i], 
                           ctar_bump = FALSE, data_source = "National",
                           scale = scale[i], trans = 1e5, 
                           chains = 3, adapt = 500, iter = 10000, thinning = 2,
                           dic = TRUE, save = TRUE)
    
    samps <- out[["samps"]]
    dic <- out[["dic"]]
    
    dic_est <- mean(dic$deviance) + mean(dic$penalty)
    
    samp_summ <- summary(samps)
    params <- rownames(samp_summ$statistics)
    diag <- gelman.diag(samps)
    
    samp_df <- as.data.frame(list(params = params, samp_summ$statistics, 
                                  quant_2.5 = samp_summ$quantiles[, 5], 
                                  quant_97.5 = samp_summ$quantiles[, 1],
                                  psrf_est = diag$psrf[, 1], 
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf, 
                                  covar_name = covar_df$covar_name[1], 
                                  pop_predict = pop_predict[k], intercept = intercept_type[l], 
                                  ctar_bump = FALSE, summed = sum_it[i], 
                                  data_source = "National", 
                                  scale = scale[i], dic = dic_est))
  }
    

##' Moramanga models 
##' ------------------------------------------------------------------------------------------------
## These all should have same index letter
bite_df <- covar_df <- morabites_by_ttimes
scale <- "Commune"
sum_it <- FALSE
intercept_type <- "fixed"

## The other index letters
pop_predict <- c("addPop", "onlyPop", "flatPop")

mods_mora <- 
  foreach(k = 1:length(pop_predict), .combine = "rbind") %dopar% {
    access <- covar_df$covar/60
    
    out <- estimate.pars(bites = round(bite_df$avg_bites), 
                         access = access, ctar_in = covar_df$ctar_in_district, pop = covar_df$pop, 
                         start = bite_df$start, end = bite_df$end, ncovars = nrow(covar_df), 
                         nlocs = nrow(bite_df), catch = covar_df$catch, ncatches = max(bite_df$catch), 
                         covar_name = covar_df$covar_name[1], pop_predict =  pop_predict[k], 
                         intercept = intercept_type, summed = sum_it, 
                         ctar_bump = FALSE, data_source = "Moramanga",
                         scale = scale, trans = 1e5, 
                         chains = 3, adapt = 500, iter = 10000, thinning = 2,
                         dic = TRUE, save = TRUE)
    
    samps <- out[["samps"]]
    dic <- out[["dic"]]
    
    dic_est <- mean(dic$deviance) + mean(dic$penalty)
    
    samp_summ <- summary(samps)
    params <- rownames(samp_summ$statistics)
    diag <- gelman.diag(samps)
    
    samp_df <- as.data.frame(list(params = params, samp_summ$statistics, 
                                  quant_2.5 = samp_summ$quantiles[, 5], 
                                  quant_97.5 = samp_summ$quantiles[, 1],
                                  psrf_est = diag$psrf[, 1], 
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf, 
                                  covar_name = covar_df$covar_name[1], 
                                  pop_predict = pop_predict[k], intercept = intercept_type, 
                                  ctar_bump = FALSE, summed = sum_it, 
                                  data_source = "Moramanga", 
                                  scale = scale, dic = dic_est))
  }

mods_all <- bind_rows(mods_mada, mods_mora)
write.csv(mods_all, "output/bitemod_results.csv", row.names = FALSE)

##' Close out cluster
closeCluster(cl)
mpi.quit()
print("Done :)")
Sys.time()
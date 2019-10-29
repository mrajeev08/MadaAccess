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
##' ------------------------------------------------------------------------------------------------
##' Mada data
## These all should have same index letter
bites_list <- list(bites_by_distcent, bites_by_distcent,
                 bites_by_ttimes, bites_by_ttimes,
                 bites_by_distwtd, bites_by_distwtd)
covars_list <- list(bites_by_distcent, commcovars_by_distcent, 
                 bites_by_ttimes, commcovars_by_ttimes, 
                 bites_by_distwtd, commcovars_by_distwtd)
scale <- c("District", "Commune", "District", "Commune", "District", "Commune")
sum_it <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)

## The other index letters
ctar_bump <- c(TRUE, FALSE)
intercept_type <- c("random", "fixed")
pop_predict <- c("addPop", "onlyPop", "flatPop")

mods_mada <- 
  foreach(i = 1:length(bites_list), .combine = "rbind") %:%
  foreach(j = 1:length(ctar_bump), .combine = "rbind") %:%
  foreach(k = 1:length(pop_predict), .combine = "rbind") %:%
  foreach(l = 1:length(intercept_type), .combine = "rbind", 
          .packages = 'rjags') %dopar% {
    
    print(c(i, j, k, l))
    
    bite_df <- bites_list[[i]]
    covar_df <- covars_list[[i]]
    
    if(covar_df$covar_name[1] == "ttimes") {
      access <- covar_df$covar/60
    } else {
      access <- covar_df$covar
    }
    
    out <- estimate.pars(bites = round(bite_df$avg_bites), 
                           access = access, ctar_in = covar_df$ctar_in_district, pop = covar_df$pop, 
                           start = bite_df$start, end = bite_df$end, ncovars = nrow(covar_df), 
                           nlocs = nrow(bite_df), catch = covar_df$catch, ncatches = max(bite_df$catch), 
                           covar_name = covar_df$covar_name[1], pop_predict =  pop_predict[k], 
                           intercept = intercept_type[l], summed = sum_it[i], 
                           ctar_bump = ctar_bump[j], data_source = "National",
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
                                  ctar_bump = ctar_bump[j], summed = sum_it[i], 
                                  data_source = "National", 
                                  scale = scale[i], dic = dic_est))
  }
    

##' Moramanga models 
##' ------------------------------------------------------------------------------------------------
## These all should have same index letter
bites_list <- covars_list <- list(morabites_by_distcent, morabites_by_ttimes, morabites_by_distwtd)
scale <- "Commune"
sum_it <- FALSE
intercept_type <- "fixed"

## The other index letters
ctar_bump <- c(TRUE, FALSE)
pop_predict <- c("addPop", "onlyPop", "flatPop")

mods_mora <- 
  foreach(i = 1:length(bites_list), .combine = "rbind") %:%
  foreach(j = 1:length(ctar_bump), .combine = "rbind") %:%
  foreach(k = 1:length(pop_predict), .combine = "rbind") %dopar% {
    
    print(c(i, j, k))
    
    bite_df <- bites_list[[i]]
    covar_df <- covars_list[[i]]
    
    if(covar_df$covar_name[1] == "ttimes") {
      access <- covar_df$covar/60
    } else {
      access <- covar_df$covar
    }
    
    out <- estimate.pars(bites = round(bite_df$avg_bites), 
                         access = access, ctar_in = covar_df$ctar_in_district, pop = covar_df$pop, 
                         start = bite_df$start, end = bite_df$end, ncovars = nrow(covar_df), 
                         nlocs = nrow(bite_df), catch = covar_df$catch, ncatches = max(bite_df$catch), 
                         covar_name = covar_df$covar_name[1], pop_predict =  pop_predict[k], 
                         intercept = intercept_type, summed = sum_it, 
                         ctar_bump = ctar_bump[j], data_source = "Moramanga",
                         scale = scale, trans = 1e5, 
                         chains = 3, adapt = 500, iter = 10000, thinning = 2,
                         dic = TRUE, save = FALSE)
    
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
                                  ctar_bump = ctar_bump[j], summed = sum_it, 
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
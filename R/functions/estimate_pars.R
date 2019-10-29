####################################################################################################
##' Estimate parameters using bayesian (and/or hierarchical) models in JAGS 
##' Details: function for estimating parameters from different candidate models using RJAGS
##' Author: Malavika Rajeev 
####################################################################################################

estimate.pars <- function(bites, access, ctar_in, pop, ncovars, nlocs, catch, ncatches, start, end,
                          covar_name = "ttimes_weighted", pop_predict = "addPop", intercept = "random",
                          summed = TRUE, ctar_bump = FALSE, data_source = "national", scale = "Commune",
                          trans = 1e5, chains = 3, adapt = 500, iter = 10000, thinning = 5, 
                          dic = TRUE, save = FALSE) {
  
  # ##' Testing
  # i = 2; j = 1; k = 1; l = 1;
  # bite_df <- bites_list[[i]]
  # covar_df <- covars_list[[i]]
  # bites = round(bite_df$avg_bites);
  # if(covar_df$covar_name[1] == "ttimes") {
  #   access <- covar_df$covar/60
  # } else {
  #   access <- covar_df$covar
  # }
  # ctar_in = covar_df$ctar_in_district; pop = covar_df$pop;
  # start = bite_df$start; end = bite_df$end; ncovars = nrow(covar_df);
  # nlocs = nrow(bite_df); catch = covar_df$catch; ncatches = max(covar_df$catch);
  # covar_name = covar_df$covar_name[1]; pop_predict =  pop_predict[k];
  # intercept = intercept_type[l]; summed = sum_it[i];
  # ctar_bump = ctar_bump[j]; data_source = "National";
  # scale = scale[i]; trans = 1e5;
  # chains = 2; adapt = 50; iter = 100; thinning = 1; dic = TRUE; save = FALSE;

  
  if(ctar_bump == TRUE) ctar_par = "bump" else ctar_par = "nobump"
  if(summed == TRUE) sumit = "summed" else sumit = "nosum"
  
  mod_name <- paste(intercept, pop_predict, sumit, ctar_par, scale, sep = "_")
  
  model_script <- paste0("R/functions/candidate_mods/", intercept, "_intercept.R")
  source(model_script, local = TRUE)
  
  jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                         n.chains = chains, n.adapt = adapt)
  
  samps <- coda.samples(jags_mod, params, n.iter = iter, thin = thinning)
  
  id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if(dic == TRUE) {
    dic_est <- dic.samples(jags_mod, n.iter = iter, thin = thinning, type = "pD")
    samps <- list(samps = samps, dic = dic_est)
  }
  
  if(save == TRUE) {
    ## Directory to output results
    dir_name <- paste(data_source, covar_name, sep = "_") ## Think about how you want to organize results
    dir_name <- paste0("output/samps/", dir_name)
    
    if (!dir.exists(dir_name)) {
      dir.create(dir_name)
    }
    
    saveRDS(samps, paste0(dir_name, "/", mod_name, "_", id, ".rds")) 
    if(dic == TRUE) {
      saveRDS(dic_est, paste0(dir_name, "/", mod_name, "_", "dic", "_", id, ".rds"))
    }
  }
  
  return(samps)
}

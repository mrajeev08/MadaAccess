#' Estimate parameters using bayesian models in JAGS 
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions
#'   
  
estimate.pars <- function(bites, ttimes, pop, ncovars, nlocs, catch, ncatches, start, end,
                          pop_predict = "addPop", intercept = "random",
                          summed = TRUE, OD = TRUE, data_source = "national", scale = "Commune",
                          trans = 1e5, chains = 3, adapt = 500, burn = 100, 
                          iter = 10000, thinning = 5, 
                          dic = TRUE, save = FALSE, pass_priors = NULL, ...) {
  
  mod_name <- paste(scale, intercept, pop_predict, sep = "_")
  
  model_script <- paste0("R/functions/candidate_mods/", intercept, "_intercept.R")
  source(model_script, local = TRUE)
  
  if(!is.null(pass_priors)) {
    model <- pass.priors(prior_list = pass_priors, uninformed = "dnorm(0, 10^-3)", model)
  }
  
  cat(model)
  
  jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                         n.chains = chains, n.adapt = adapt)
  update(jags_mod, burn)
  samps <- coda.samples(jags_mod, params, n.iter = iter, thin = thinning)

  if(dic == TRUE) {
    dic_est <- dic.samples(jags_mod, n.iter = iter, thin = thinning, type = "pD")
    samps <- list(samps = samps, dic = dic_est)
  }
  
  if(save == TRUE) {
    ## Directory to output results
    dir_name <- paste0("output/mods/samps/", data_source)
    
    if (!dir.exists(dir_name)) {
      dir.create(dir_name)
    }
    saveRDS(samps, paste0(dir_name, "/", mod_name, ".rds")) 
  }
  return(samps)
}


#' Pass priors from list
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions
#'     
pass.priors <- function(prior_list, uninformed = "dnorm(0, 10^-3)", model) {
  for(j in 1:length(prior_list)) {
    prior_lookup <- glue("{names(prior_list)[j]} ~ {uninformed}\n")
    model <- gsub(prior_lookup, prior_list[[j]], model, fixed = TRUE)
  }
  return(model)
}
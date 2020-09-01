#' Estimate parameters using bayesian models in JAGS 
#' @Details
#' @param bites, ttimes, pop, ncovars, nlocs, catch, ncatches, start, end, group = data to pass for 
#' fitting, see candidate_mods folder for more details
#' @param pop_predict type of population scaling in the model
#' @param intercept type of intercept in the model
#' @param summed whether to used the aggregated or unaggregated model (for fitting national data but
#' at the district scale)
#' @param OD whether to fit an overdispersion parameter for the poisson model
#' @param data_source data used to fit the model (either National or Moramanga data)
#' @param scale scale of the model (either commune or district level)
#' @param trans transformation denominator for pop variable
#' @param chains number of mcmc chains
#' @param adapt number of iterations for adapt phase (see \code[rjags::jags.model])
#' @param burn burn in for mcmc chains
#' @param iter number of iterations to run the mcmc chains
#' @param thinning number of iterations to thin at (i.e. save every n steps of each chain)
#' @param dic whether to estimate the model dic
#' @param save whether to save the mcmc chains
#' @param pass_priors a list of priors to pass, uses the \code[pass.priors] functions to gsub out old
#' priors
#' @param seed seed to use for the jags model (if NULL will not be reproducible)
#' @param suffix another label to pass when saving the object (i.e. to identify the saved files)
#' @return Summary of estimates from mcmc.
#' @section Dependencies:
#'     rjags, foreach
#'   
  
estimate.pars <- function(data_df, covar_df, model_func, pars,
                          trans = 1e5, chains = 3, adapt = 500, burn = 100, 
                          iter = 10000, thinning = 5, 
                          dic = TRUE, save = FALSE, pass_priors = NULL, 
                          seed = NULL, suffix = NULL, ...) {
  
  
  mod_name <- paste0(pars$scale, "_", pars$intercept, "_", pars$pop_predict, 
                     ifelse(pars$OD == TRUE, "_OD", ""),
                     ifelse(!is.null(suffix), suffix, ""))
  
  # get the model function and out its output to the function environment
  list2env(model_func(summed = pars$summed, pop_predict = pars$pop_predict, OD = pars$OD, 
                      bites = data_df$avg_bites, ttimes = covar_df$ttimes, 
                      pop = covar_df$pop, group = covar_df$group, catch = covar_df$catch,
                      ncovars = nrow(covar_df), nlocs = nrow(data_df), 
                      ncatches = max(data_df$catch), start = covar_df$start, end = covar_df$end, 
                      trans = trans), envir = environment())

  if(!is.null(pass_priors)) {
    model <- pass.priors(prior_list = pass_priors, uninformed = "dnorm(0, 10^-3)", model)
  }
  
  cat(model)
  
  if(!is.null(seed)) {
    mcmc.combine <- function( ... ){
      return(as.mcmc.list(sapply(list( ... ), mcmc)))
    }
    
    foreach(i = 1:chains, .combine = mcmc.combine, .multicombine = TRUE) %do% {
      inits <-  list(.RNG.name = 'base::Wichmann-Hill', .RNG.seed = seed + i)
      jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                             n.chains = 1, n.adapt = adapt)
      update(jags_mod, burn)
      samps <- coda.samples(jags_mod, pars, n.iter = iter, thin = thinning)
    } -> samps
    
  } else {
    jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                           n.chains = chains, n.adapt = adapt)
    update(jags_mod, burn)
    samps <- coda.samples(jags_mod, pars, n.iter = iter, thin = thinning)
  }
  
  if(dic == TRUE) {
    if(chains > 1) {
      # these inits will be the same as passed from the model script
      # DIC ests are thus not reproducible!
      jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                             n.chains = chains, n.adapt = adapt)
      update(jags_mod, burn)
      dic_est <- dic.samples(jags_mod, n.iter = iter, thin = thinning, type = "pD")
      samps <- list(samps = samps, dic = dic_est)
    } else {
      samps <- list(samps = samps, dic = NA)
    }
  } else {
    samps <- list(samps = samps)
  }
  
  if(save == TRUE) {
    ## Directory to output results
    dir_name <- paste0("output/mods/samps/", pars$data_source)
    
    if (!dir.exists(dir_name)) {
      dir.create(dir_name, recursive = TRUE)
    }
    saveRDS(samps, paste0(dir_name, "/", mod_name, ".rds")) 
  }
  return(samps)
}


#' Pass priors from list
#' Looks for priors and gsubs with the priors passed in prior_list
#' @Details Assumes that all supplied priors are the same (i.e. uninformed)
#' @param prior_list list of named priors
#' @param uninformed prior to replace/update
#' @return The model text modified with the updated priors
#' @section Dependencies:
#'     glue
#'     
pass.priors <- function(prior_list, uninformed = "dnorm(0, 10^-3)", model) {
  for(j in 1:length(prior_list)) {
    prior_lookup <- glue("{names(prior_list)[j]} ~ {uninformed}\n")
    model <- gsub(prior_lookup, prior_list[[j]], model, fixed = TRUE)
  }
  return(model)
}
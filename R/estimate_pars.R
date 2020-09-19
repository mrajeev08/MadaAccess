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

estimate.pars <- function(data_df, covar_df, model = "random", pars,
                          trans = 1e5, chains = 3, adapt = 500, burn = 100,
                          iter = 10000, thinning = 5,
                          dic = TRUE, save = FALSE, pass_priors = NULL,
                          seed = NULL, suffix = NULL,
                          out_dir = "analysis/out/mods/samps/", ...) {
  mod_name <- paste0(
    pars$scale, "_", pars$intercept, "_", pars$pop_predict,
    ifelse(pars$OD == TRUE, "_OD", ""),
    ifelse(!is.null(suffix), suffix, "")
  )

  model_func <- ifelse(pars$intercept == "random", random_mod, fixed_mod)

  # get the model function and out its output to the function environment
  list2env(model_func(
    summed = pars$summed, pop_predict = pars$pop_predict, OD = pars$OD,
    bites = data_df$avg_bites, ttimes = covar_df$ttimes,
    pop = covar_df$pop, group = covar_df$group, catch = covar_df$catch,
    ncovars = nrow(covar_df), nlocs = nrow(data_df),
    ncatches = max(data_df$catch), start = data_df$start, end = data_df$end,
    trans = trans
  ), envir = environment())

  if (!is.null(pass_priors)) {
    model <- pass.priors(prior_list = pass_priors, uninformed = "dnorm(0, 10^-3)", model)
  }

  cat(model)

  if (!is.null(seed)) {
    mcmc.combine <- function(...) {
      return(as.mcmc.list(sapply(list(...), mcmc)))
    }

    foreach(i = 1:chains, .combine = mcmc.combine, .multicombine = TRUE) %do% {
      inits <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seed + i)
      jags_mod <- jags.model(textConnection(model),
        data = data, inits = inits,
        n.chains = 1, n.adapt = adapt
      )
      update(jags_mod, burn)
      samps <- coda.samples(jags_mod, params, n.iter = iter, thin = thinning)
    } -> samps
  } else {
    jags_mod <- jags.model(textConnection(model),
      data = data, inits = inits,
      n.chains = chains, n.adapt = adapt
    )
    update(jags_mod, burn)
    samps <- coda.samples(jags_mod, params, n.iter = iter, thin = thinning)
  }

  if (dic == TRUE) {
    if (chains > 1) {
      # these inits will be the same as passed from the model script
      # DIC ests are thus not reproducible!
      jags_mod <- jags.model(textConnection(model),
        data = data, inits = inits,
        n.chains = chains, n.adapt = adapt
      )
      update(jags_mod, burn)
      dic_est <- dic.samples(jags_mod, n.iter = iter, thin = thinning, type = "pD")
      samps <- list(samps = samps, dic = dic_est)
    } else {
      samps <- list(samps = samps, dic = NA)
    }
  } else {
    samps <- list(samps = samps)
  }

  if (save == TRUE) {
    ## Directory to output results
    dir_name <- paste0(out_dir, pars$data_source)

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
  for (j in 1:length(prior_list)) {
    prior_lookup <- glue("{names(prior_list)[j]} ~ {uninformed}\n")
    model <- gsub(prior_lookup, prior_list[[j]], model, fixed = TRUE)
  }
  return(model)
}

# Models w/out random effect

fixed_mod <- function(summed, pop_predict, OD, bites, ttimes, pop, group, catch,
                      ncovars, nlocs, ncatches, start, end, trans, ...) {
  # Model with latent var @ commune level
  if (summed == TRUE) {
    model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 0.1)
    beta_ttimes ~ dnorm(0, 0.1)

    # Insert OD prior here

    # Likelihood
    for (i in 1:ncovars) {
      nbites[i] <- exp_bites[i]*pop[i] # remove offset
      log(exp_bites[i]) <- beta_0 + beta_ttimes*ttimes[i] # Insert OD param here
    }

    for(k in 1:nlocs) {
      bites[k] ~ dpois(sum_bites[k])
      sum_bites[k] <- sum(nbites[start[k]:end[k]])
    }
  }"

    # data
    data <- list(
      bites = round(bites), ttimes = ttimes, pop = pop, group = group,
      ncovars = ncovars, nlocs = nlocs, start = start, end = end
    )
  }

  # Model w/out latent var
  if (summed == FALSE) {
    model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 0.1)
    beta_ttimes ~ dnorm(0, 0.1)

    # Insert OD prior here

    # Likelihood
    for (i in 1:nlocs) {
      bites[i] ~ dpois(nbites[i])
      nbites[i] <- exp_bites[i]*pop[i] # remove offset
      log(exp_bites[i]) <- beta_0 + beta_ttimes*ttimes[i] # Insert OD param here
    }
  }"

    # data
    data <- list(
      bites = round(bites), ttimes = ttimes, pop = pop,
      nlocs = nlocs
    )
  }

  # params
  params <- c("beta_0", "beta_ttimes")
  # inits
  inits <- list(
    beta_0 = rnorm(1, 0, 1),
    beta_ttimes = rnorm(1, 0, 1)
  )

  # Pop options
  if (pop_predict == "addPop") {
    # edit model text accordingly
    model <- gsub("beta_ttimes ~ dnorm(0, 0.1)",
                  "beta_ttimes ~ dnorm(0, 0.1)\n    beta_pop ~ dnorm(0, 0.1)",
                  model,
                  fixed = TRUE
    ) # add extra params + priors
    model <- gsub("*pop[i] # remove offset", "", model, fixed = TRUE) # remove the offset
    model <- gsub("beta_0 + beta_ttimes*ttimes[i]",
                  "beta_0 + beta_ttimes*ttimes[i] + beta_pop*pop[i]/trans",
                  model,
                  fixed = TRUE
    ) # change formula for exp_bites
    # data add in trans
    data <- c(data, trans = trans)
    # params: add beta pop
    params <- c(params, "beta_pop")
    # inits: add beta pop
    inits <- c(inits, beta_pop = rnorm(1, 0, 1))
  }

  if (pop_predict == "onlyPop") {
    # edit model text accordingly
    model <- gsub("beta_ttimes ~ dnorm(0, 0.1)",
                  "beta_pop ~ dnorm(0, 0.1)", model,
                  fixed = TRUE
    ) # remove extra params + priors
    model <- gsub("*pop[i] # remove offset", "", model, fixed = TRUE) # remove the offset
    model <- gsub("beta_0 + beta_ttimes*ttimes[i]",
                  "beta_0 + beta_pop*pop[i]/trans",
                  model,
                  fixed = TRUE
    ) # change formula for exp_bites
    # data add in trans
    data <- c(data, trans = trans)
    params <- c(params, "beta_pop")

    # remove ttimes from every bit
    data[["ttimes"]] <- NULL
    inits[["beta_ttimes"]] <- NULL
    params <- params[params != "beta_ttimes"]
  }

  # Overdispersion
  if (OD == TRUE) {
    OD_priors <-
      "sigma_e ~ dunif(0, 10)
    tau_e <- pow(sigma_e, -2)
    for(j in 1:nlocs){
      epsilon[j] ~ dnorm(0, tau_e)
    }"

    # add lines for prior & param
    model <- gsub("# Insert OD prior here", OD_priors, model, fixed = TRUE)
    if (summed == FALSE) {
      model <- gsub("# Insert OD param here", "+ epsilon[i]", model, fixed = TRUE)
    } else {
      model <- gsub("# Insert OD param here", "+ epsilon[group[i]]", model, fixed = TRUE)
    }

    # add sigma_e to inits
    inits <- c(inits, sigma_e = rlnorm(1))
    # add sigma_e to param list
    params <- c(params, "sigma_e")
  }

  return(list(model = model, inits = inits, params = params, data = data))
}

# Models with random effect
random_mod <- function(summed, pop_predict, OD, bites, ttimes, pop, group, catch,
                       ncovars, nlocs, ncatches, start, end, trans, ...) {
  if (summed == TRUE) {
    model <- "model {

      # Priors
      sigma_0 ~ dunif(0, 10) # SD hyperparameter for random intercepts
      tau_0 <- pow(sigma_0, -2)
      for (i in 1:ncatches) {
          alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
      }

      beta_0 ~ dnorm(0, 0.1)
      beta_ttimes ~ dnorm(0, 0.1)

      # Insert OD prior here

      # Likelihood
      for (i in 1:ncovars) {
        nbites[i] <- exp_bites[i]*pop[i] # remove offset
        log(exp_bites[i]) <- alpha[catch[i]] + beta_ttimes*ttimes[i] # Insert OD param here
      }

      for(k in 1:nlocs) {
        bites[k] ~ dpois(sum_bites[k])
        sum_bites[k] <- sum(nbites[start[k]:end[k]])
      }

    }"

    # data
    data <- list(
      bites = round(bites), ttimes = ttimes, pop = pop,
      catch = catch, ncatches = ncatches, group = group,
      ncovars = ncovars, nlocs = nlocs, start = start, end = end
    )
  }

  # Model w/out latent var
  if (summed == FALSE) {
    model <- "model {
      # Priors
      beta_0 ~ dnorm(0, 0.1) # Mean hyperparameter for random intercepts
      sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
      tau_0 <- pow(sigma_0, -2)
      for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
      }

      beta_ttimes ~ dnorm(0, 0.1)

      # Insert OD prior here

      # Likelihood
      for (i in 1:nlocs) {
          bites[i] ~ dpois(nbites[i])
          nbites[i] <- exp_bites[i]*pop[i] # remove offset
          log(exp_bites[i]) <- alpha[catch[i]] + beta_ttimes*ttimes[i] # Insert OD param here
      }
    }"

    # data
    data <- list(
      bites = round(bites), ttimes = ttimes, pop = pop,
      catch = catch, ncatches = ncatches, nlocs = nlocs
    )
  }

  # params
  params <- c("alpha", "beta_0", "beta_ttimes", "sigma_0")
  # inits
  inits <- list(
    alpha = rnorm(ncatches, 0, 1),
    beta_0 = rnorm(1, 0, 1),
    beta_ttimes = rnorm(1, 0, 1),
    sigma_0 = rlnorm(1)
  )

  # Pop options
  if (pop_predict == "addPop") {
    # edit model text accordingly
    model <- gsub("beta_ttimes ~ dnorm(0, 0.1)",
                  "beta_ttimes ~ dnorm(0, 0.1)\n    beta_pop ~ dnorm(0, 0.1)",
                  model,
                  fixed = TRUE
    ) # add extra params + priors
    model <- gsub("*pop[i] # remove offset", "", model, fixed = TRUE) # remove the offset
    model <- gsub("alpha[catch[i]] + beta_ttimes*ttimes[i]",
                  "alpha[catch[i]] + beta_ttimes*ttimes[i] + beta_pop*pop[i]/trans",
                  model,
                  fixed = TRUE
    ) # change formula for exp_bites
    # data add in trans
    data <- c(data, trans = trans)
    # params: add beta pop
    params <- c(params, "beta_pop")
    # inits: add beta pop
    inits <- c(inits, beta_pop = rnorm(1, 0, 1))
  }

  if (pop_predict == "onlyPop") {
    # edit model text accordingly
    model <- gsub("beta_ttimes ~ dnorm(0, 0.1)",
                  "beta_pop ~ dnorm(0, 0.1)", model,
                  fixed = TRUE
    ) # remove extra params + priors
    model <- gsub("*pop[i] # remove offset", "", model, fixed = TRUE) # remove the offset
    model <- gsub("alpha[catch[i]] + beta_ttimes*ttimes[i]",
                  "alpha[catch[i]] + beta_pop*pop[i]/trans",
                  model,
                  fixed = TRUE
    ) # change formula for exp_bites
    # data add in trans
    data <- c(data, trans = trans)
    params <- c(params, "beta_pop")

    # remove ttimes from every bit
    data[["ttimes"]] <- NULL
    inits[["beta_ttimes"]] <- NULL
    params <- params[params != "beta_ttimes"]
  }

  # Overdispersion
  # Overdispersion
  if (OD == TRUE) {
    OD_priors <-
      "sigma_e ~ dunif(0, 10)
      tau_e <- pow(sigma_e, -2)
      for(j in 1:nlocs){
        epsilon[j] ~ dnorm(0, tau_e)
      }"

    # add lines for prior & param
    model <- gsub("# Insert OD prior here", OD_priors, model, fixed = TRUE)
    if (summed == FALSE) {
      model <- gsub("# Insert OD param here", "+ epsilon[i]", model, fixed = TRUE)
    } else {
      model <- gsub("# Insert OD param here", "+ epsilon[group[i]]", model, fixed = TRUE)
    }

    # add sigma_e to inits
    inits <- c(inits, sigma_e = rlnorm(1))
    # add sigma_e to param list
    params <- c(params, "sigma_e")
  }

  return(list(model = model, inits = inits, params = params, data = data))
}


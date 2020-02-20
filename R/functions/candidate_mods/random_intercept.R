## Models with random effect
if (summed == TRUE) {
  ##' Model with grouping var
  model <- "model {

    # Priors
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- pow(sigma_0, -2)
    beta_0 ~ dnorm(0, 10^-6)
    beta_ttimes ~ dnorm(0, 10^-6) 
    sigma_e ~ dunif(0, 100) # SD overdispersion parameter
    tau_e <- pow(sigma_e, -2)

    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    for(j in 1:ncovars){
        epsilon[j] ~ dnorm(0, tau_e)
    }
    
    # Likelihood
    for (i in 1:ncovars) {
      nbites[i] <- exp_bites[i]*pop[i]
      log(exp_bites[i]) <- alpha[catch[i]] + beta_ttimes*ttimes[i] + epsilon[i]
    }
    
    for(k in 1:nlocs) {
      sum_bites[k] <- sum(nbites[start[k]:end[k]])
      bites[k] ~ dpois(sum_bites[k]) # The actual (random) responses
    }

  }"
  
  ## data
  data <- list(bites = round(bites), ttimes = ttimes, pop = pop,
               catch = catch, ncatches = ncatches, 
               ncovars = ncovars, nlocs = nlocs, start = start, end = end)
  
}

## Unsummed 
if(summed == FALSE) {
  model <- "model {
    # Priors
    beta_0 ~ dnorm(0, 10^-6) # Mean hyperparameter for random intercepts
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- 1/(sigma_0*sigma_0)
    beta_ttimes ~ dnorm(0, 10^-6) 
    sigma_e ~ dunif(0, 100) # SD overdispersion parameter
    tau_e <- pow(sigma_e, -2)

    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    for(j in 1:nlocs){
        epsilon[j] ~ dnorm(0, tau_e)
    }

    # Likelihood
    for (i in 1:nlocs) {
        bites[i] ~ dpois(nbites[i]) # The actual (random) responses
        nbites[i] <- exp_bites[i]*pop[i]
        log(exp_bites[i]) <- alpha[catch[i]] + beta_ttimes*ttimes[i] + epsilon[i]
    }
  }"
  
  ## data
  data <- list(bites = round(bites), ttimes = ttimes, pop = pop,
               catch = catch, ncatches = ncatches, nlocs = nlocs)
}

## params
params <- c("alpha", "beta_0", "beta_ttimes", "sigma_0", "sigma_e")
## inits
inits <- list(alpha = rnorm(ncatches, 0, 1e-6),
              beta_0 = rnorm(1, 0, 1e-6), 
              beta_ttimes = rnorm(1, 0, 1e-6),
              sigma_0 = rlnorm(1),
              sigma_e = rlnorm(1))

## Other options
if(pop_predict =="addPop") {
  ## edit model text accordingly
  model <- gsub("beta_ttimes ~ dnorm(0, 10^-6)",
                "beta_ttimes ~ dnorm(0, 10^-6)\n    beta_pop ~ dnorm(0, 10^-6)", 
                model, fixed = TRUE)   # add extra params + priors
  model <- gsub("alpha[catch[i]] + beta_ttimes*ttimes[i] + epsilon[i]",
                "alpha[catch[i]] + beta_ttimes*ttimes[i] + epsilon[i] + beta_pop*pop[i]/trans", 
                model, fixed = TRUE)    # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  ## params: add beta pop
  params <- c(params, "beta_pop")
  ## inits: add beta pop
  inits <- c(inits, beta_pop = rnorm(1, 0, 1e-6))
}

if(pop_predict =="onlyPop") {
  ## edit model text accordingly
  model <- gsub("beta_ttimes ~ dnorm(0, 10^-6)",
                "beta_pop ~ dnorm(0, 10^-6)", model, fixed = TRUE)   # remove extra params + priors
  model <- gsub("alpha[catch[i]] + beta_ttimes*ttimes[i] + epsilon[i]",
                "alpha[catch[i]] + epsilon[i] + beta_pop*pop[i]/trans",
                model, fixed = TRUE)   # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  params <- c(params, "beta_pop")
  
  ## remove ttimes from every bit
  data[["ttimes"]] <- NULL
  inits[["beta_ttimes"]] <- NULL
  params <- params[params != "beta_ttimes"]
}

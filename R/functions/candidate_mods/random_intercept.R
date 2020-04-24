# Models with random effect

# Model with latent var @ commune level
if (summed == TRUE) {
  model <- "model {

    # Priors
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- pow(sigma_0, -2)
    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    beta_0 ~ dnorm(0, 10^-3)
    beta_ttimes ~ dnorm(0, 10^-3) 
    
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
  data <- list(bites = round(bites), ttimes = ttimes, pop = pop,
               catch = catch, ncatches = ncatches, group = group, 
               ncovars = ncovars, nlocs = nlocs, start = start, end = end)
  
}

# Model w/out latent var
if(summed == FALSE) {
  model <- "model {
    # Priors
    beta_0 ~ dnorm(0, 10^-3) # Mean hyperparameter for random intercepts
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- pow(sigma_0, -2)
    for (i in 1:ncatches) {
      alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    beta_ttimes ~ dnorm(0, 10^-3) 
    
    # Insert OD prior here

    # Likelihood
    for (i in 1:nlocs) {
        bites[i] ~ dpois(nbites[i]) 
        nbites[i] <- exp_bites[i]*pop[i] # remove offset
        log(exp_bites[i]) <- alpha[catch[i]] + beta_ttimes*ttimes[i] # Insert OD param here
    }
  }"
  
  # data
  data <- list(bites = round(bites), ttimes = ttimes, pop = pop,
               catch = catch, ncatches = ncatches, nlocs = nlocs)
}

# params
params <- c("alpha", "beta_0", "beta_ttimes", "sigma_0")
# inits
inits <- list(alpha = rnorm(ncatches, 0, 1),
              beta_0 = rnorm(1, 0, 1), 
              beta_ttimes = rnorm(1, 0, 1),
              sigma_0 = rlnorm(1))

# Pop options
if(pop_predict =="addPop") {
  # edit model text accordingly
  model <- gsub("beta_ttimes ~ dnorm(0, 10^-3)",
                "beta_ttimes ~ dnorm(0, 10^-3)\n    beta_pop ~ dnorm(0, 10^-3)", 
                model, fixed = TRUE)   # add extra params + priors
  model <- gsub("*pop[i] # remove offset", "", model, fixed = TRUE) # remove the offset
  model <- gsub("alpha[catch[i]] + beta_ttimes*ttimes[i]",
                "alpha[catch[i]] + beta_ttimes*ttimes[i] + beta_pop*pop[i]/trans", 
                model, fixed = TRUE)    # change formula for exp_bites
  # data add in trans
  data <- c(data, trans = trans)
  # params: add beta pop
  params <- c(params, "beta_pop")
  # inits: add beta pop
  inits <- c(inits, beta_pop = rnorm(1, 0, 1))
}

if(pop_predict =="onlyPop") {
  # edit model text accordingly
  model <- gsub("beta_ttimes ~ dnorm(0, 10^-3)",
                "beta_pop ~ dnorm(0, 10^-3)", model, fixed = TRUE)   # remove extra params + priors
  model <- gsub("*pop[i] # remove offset", "", model, fixed = TRUE) # remove the offset
  model <- gsub("alpha[catch[i]] + beta_ttimes*ttimes[i]",
                "alpha[catch[i]] + beta_pop*pop[i]/trans",
                model, fixed = TRUE)   # change formula for exp_bites
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
if(OD == TRUE) {
  
  "sigma_e ~ dunif(0, 10)
    tau_e <- pow(sigma_e, -2)     
    for(j in 1:nlocs){
      epsilon[j] ~ dnorm(0, tau_e) 
    }" -> OD_priors
  
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

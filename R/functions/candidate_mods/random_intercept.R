## Models with random effect
if (summed == TRUE) {
  ##' Model with grouping var
  model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 0.0001) # Mean hyperparameter for random intercepts
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- 1/(sigma_0*sigma_0)
    beta_ctar ~ dnorm(0, 10^-6)
    beta_access ~ dnorm(0, 10^-6) # Common slope

    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
 
    # Likelihood
    for (i in 1:ncovars) {
      exp_bites[i] <- exp(alpha[catch[i]] + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i]
    }
    
    for(k in 1:nlocs) {
      sum_bites[k] <- sum(exp_bites[start[k]:end[k]])
      bites[k] ~ dpois(sum_bites[k]) # The actual (random) responses
    }

  }"
  
  ## data
  data <- list(bites = round(bites), access = access, ctar_in = ctar_in, pop = pop,
               catch = catch, ncatches = ncatches, 
               ncovars = ncovars, nlocs = nlocs, start = start, end = end)
  
}

## Unsummed 
if(summed == FALSE) {
  model <- "model {
    # Priors
    beta_0 ~ dnorm(0, 0.0001) # Mean hyperparameter for random intercepts
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- 1/(sigma_0*sigma_0)

    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    beta_ctar ~ dnorm(0, 10^-6)
    beta_access ~ dnorm(0, 0.0001) # Common slope

    # Likelihood
    for (i in 1:nlocs) {
        exp_bites[i] <- exp(alpha[catch[i]] + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i] 
        bites[i] ~ dpois(exp_bites[i]) # The actual (random) responses
    }
  }"
  
  ## data
  data <- list(bites = round(bites), access = access, ctar_in = ctar_in, pop = pop,
               catch = catch, ncatches = ncatches)
}

## params
params <- c("beta_0", "beta_access", "beta_ctar", "sigma_0")
## inits
inits <- list(beta_0 = rnorm(1, 0, 1e-6), 
              beta_access = rnorm(1, 0, 1e-6),
              beta_ctar = rnorm(1, 0, 1e-6),
              sigma_0 = rlnorm(1))

## Other options
if(pop == "addPop") {
  ## edit model text accordingly
  model <- gsub("beta_access ~ dnorm(0, 10^-6)",
                "beta_access ~ dnorm(0, 10^-6)\n    beta_pop ~ dnorm(0, 10^-6)", 
                model, fixed = TRUE)   # add extra params + priors
  model <- gsub("exp_bites[i] <- exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i]",
                "exp(beta_0 + beta_access*access[i] +  + beta_ctar*ctar_in[i] + beta_pop*pop[i]/trans)", 
                model, fixed = TRUE)    # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  ## params: add beta pop
  params <- c(params, "beta_pop")
  ## inits: add beta pop
  inits <- c(inits, beta_pop = rnorm(1, 0, 1e-6))
}

if(pop == "onlyPop") {
  ## edit model text accordingly
  model <- gsub("beta_access ~ dnorm(0, 10^-6)",
                "beta_pop ~ dnorm(0, 10^-6)", model, fixed = TRUE)   # remove extra params + priors
  model <- gsub("exp_bites[i] <- exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i]",
                "exp(beta_pop*pop[i]/trans + beta_ctar*ctar_in[i])",
                model, fixed = TRUE)   # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  
  ## remove access from every bit
  data[["access"]] <- NULL
  inits[["beta_access"]] <- NULL
  params <- params[params != "beta_access"]
}

if(ctar_bump == FALSE) {
  ## edit model text accordingly
  model <- gsub("beta_ctar ~ dnorm(0, 10^-6)",
                "", model, fixed = TRUE)   # remove extra params + priors
  model <- gsub("+ beta_ctar*ctar_in[i]",
                "", model, fixed = TRUE)   # change formula for exp_bites
  ## remove ctar from every bit
  data[["ctar_in"]] <- NULL
  inits[["beta_ctar"]] <- NULL
  params <- params[params != "beta_ctar"]
  
}

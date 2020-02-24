## Models w/out random effect
##' Model with grouping var w/out random effect
if (summed == TRUE) {
  model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 10^-6)
    beta_ttimes ~ dnorm(0, 10^-6)
    
    # Insert OD prior here
    
    # Likelihood
    for (i in 1:ncovars) {
      nbites[i] <- exp_bites[i]*pop[i]
      log(exp_bites[i]) <- beta_0 + beta_ttimes*ttimes[i] # Insert OD param here
    }
    
    for(k in 1:nlocs) {
      bites[k] ~ dpois(sum_bites[k]) 
      sum_bites[k] <- sum(nbites[start[k]:end[k]])
    }
  }"
  
  ## data
  data <- list(bites = round(bites), ttimes = ttimes, pop = pop,
               ncovars = ncovars, nlocs = nlocs, start = start, end = end)
}

## Unsummed model
if (summed == FALSE) {
  model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 10^-6)
    beta_ttimes ~ dnorm(0, 10^-6)
    
    # Insert OD prior here
    
    # Likelihood
    for (i in 1:nlocs) {
      bites[i] ~ dpois(nbites[i])
      nbites[i] <- exp_bites[i]*pop[i]
      log(exp_bites[i]) <- beta_0 + beta_ttimes*ttimes[i] # Insert OD param here
    }
  }"
  
  ## data
  data <- list(bites = round(bites), ttimes = ttimes, pop = pop,
               nlocs = nlocs)

}

## params
params <- c("beta_0", "beta_ttimes")
## inits
inits <- list(beta_0 = rnorm(1, 0, 1e-6), 
              beta_ttimes = rnorm(1, 0, 1e-6))

## Other options
if(pop_predict ==  "addPop") {
  ## edit model text accordingly
  model <- gsub("beta_ttimes ~ dnorm(0, 10^-6)",
                "beta_ttimes ~ dnorm(0, 10^-6)\n    beta_pop ~ dnorm(0, 10^-6)", 
                model, fixed = TRUE)   # add extra params + priors
  model <- gsub("beta_0 + beta_ttimes*ttimes[i]",
                "beta_0 + beta_ttimes*ttimes[i] + beta_pop*pop[i]/trans", 
                model, fixed = TRUE)    # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  ## params: add beta pop
  params <- c(params, "beta_pop")
  ## inits: add beta pop
  inits <- c(inits, beta_pop = rnorm(1, 0, 1e-6))
}

if(pop_predict ==  "onlyPop") {
  ## edit model text accordingly
  model <- gsub("beta_ttimes ~ dnorm(0, 10^-6)",
                "beta_pop ~ dnorm(0, 10^-6)", model, fixed = TRUE)   # remove extra params + priors
  model <- gsub("beta_0 + beta_ttimes*ttimes[i]",
                "beta_0 + beta_pop*pop[i]/trans",
                model, fixed = TRUE)   # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  params <- c(params, "beta_pop")
  
  ## remove ttimes from every bit
  data[["ttimes"]] <- NULL
  inits[["beta_ttimes"]] <- NULL
  params <- params[params != "beta_ttimes"]
}

if(OD == TRUE) {
  
  "sigma_e ~ dunif(0, 100)
    tau_e <- pow(sigma_e, -2)     
    for(j in 1:ncovars){
      epsilon[j] ~ dnorm(0, tau_e)\n  
    }" -> OD_priors
  
  if (summed == FALSE) {
    OD_priors <- gsub("ncovars", "nlocs", OD_priors, fixed = TRUE)
  }  
  
  # add lines for prior & param
  model <- gsub("# Insert OD prior here", OD_priors, model, fixed = TRUE)
  model <- gsub("# Insert OD param here", "+ epsilon[i]", model, fixed = TRUE)   
  
  # add sigma_e to inits
  inits <- c(inits, sigma_e = rlnorm(1))
  # add sigma_e to param list
  params <- c(params, "sigma_e")
}

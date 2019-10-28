## Models w/out random effect
##' Model with grouping var w/out random effect
if (summed == TRUE) {
  model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 10^-6)
    beta_access ~ dnorm(0, 10^-6)
    beta_ctar ~ dnorm(0, 10^-6)

    # Likelihood
    for (i in 1:ncovars) {
      exp_bites[i] <- exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i] # Expectation
    }
    
    for(k in 1:nlocs) {
      sum_bites[k] <- sum(exp_bites[start[k]:end[k]])
      bites[k] ~ dpois(sum_bites[k]) # The actual (random) responses
    }
  }"
  
  ## data
  data <- list(bites = round(bites), access = access, ctar_in = ctar_in, pop = pop,
               ncovars = ncovars, nlocs = nlocs, start = start, end = end)
}

## Unsummed model
if (summed == FALSE) {
  model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 10^-6)
    beta_access ~ dnorm(0, 10^-6)
    beta_ctar ~ dnorm(0, 10^-6)
    
    # Likelihood
    for (i in 1:nlocs) {
        exp_bites[i] <- exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i] # Expectation
        bites[i] ~ dpois(exp_bites[i]) # The actual (random) responses
    }
  }"
  
  ## data
  data <- list(bites = round(bites), access = access, ctar_in = ctar_in, pop = pop,
               nlocs = nlocs)

}

## params
params <- c("beta_0", "beta_access", "beta_ctar")
## inits
inits <- list(beta_0 = rnorm(1, 0, 1e-6), 
              beta_access = rnorm(1, 0, 1e-6),
              beta_ctar = rnorm(1, 0, 1e-6))

## Other options
if(pop_predict ==  "addPop") {
  ## edit model text accordingly
  model <- gsub("beta_access ~ dnorm(0, 10^-6)",
                "beta_access ~ dnorm(0, 10^-6)\n    beta_pop ~ dnorm(0, 10^-6)", 
                model, fixed = TRUE)   # add extra params + priors
  model <- gsub("exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i]",
                "exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i] + beta_pop*pop[i]/trans)", 
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
  model <- gsub("beta_access ~ dnorm(0, 10^-6)",
                "beta_pop ~ dnorm(0, 10^-6)", model, fixed = TRUE)   # remove extra params + priors
  model <- gsub("exp(beta_0 + beta_access*access[i] + beta_ctar*ctar_in[i])*pop[i]",
                "exp(beta_0 + beta_pop*pop[i]/trans + beta_ctar*ctar_in[i])",
                model, fixed = TRUE)   # change formula for exp_bites
  ## data add in trans
  data <- c(data, trans = trans)
  params <- c(params, "beta_pop")
  
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



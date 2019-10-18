library(rjags)
bites_by_ttimes$catch <- as.numeric(droplevels(bites_by_ttimes$catchment))
commcovars_by_ttimes$catch <- as.numeric(droplevels(commcovars_by_ttimes$catchment))

## Model w/out random effect
model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 10^-6)
    beta_access ~ dnorm(0, 0.0001) # Common slope

    # Likelihood
    for (i in 1:nlocs) {
        exp_bites[i] <- exp(beta_0 + beta_access*access[i])*pop[i] # Expectation
        bites[i] ~ dpois(exp_bites[i]) # The actual (random) responses
    }
  }"

# data
data <- list(bites = round(bites_by_ttimes$avg_bites), 
             access = bites_by_ttimes$covar/60, 
             nlocs = nrow(bites_by_ttimes), 
             pop = bites_by_ttimes$pop)
params <- c("beta_access", "beta_0")
inits <- list(beta_access = rnorm(1, 0, 1e-6), beta_0 = rnorm(1, 0, 1e-6))

jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                       n.chains = 3, n.adapt = 500)

samps <- coda.samples(jags_mod, params, n.iter = 10000, thin = 1)
densplot(samps)
traceplot(samps)
summary(samps)

##' Model with grouping var w/out random effect
model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 10^-6)
    beta_access ~ dnorm(0, 0.0001) # Common slope

    # Likelihood
    for (i in 1:ncovars) {
      exp_bites[i] <- exp(beta_0 + beta_access*access[i])*pop[i] # Expectation
    }
    
    for(k in 1:nlocs) {
      sum_bites[k] <- sum(exp_bites[start[k]:end[k]])
      bites[k] ~ dpois(sum_bites[k]) # The actual (random) responses
    }

  }"

## Where start and end are indexes of groups for each k (so covars should be ordered by districts!)
## And comm covars should be ordered in the same way!
bites_by_ttimes$group <- as.numeric(droplevels(as.factor(bites_by_ttimes$names_covar)))
ordered_bites <- bites_by_ttimes[order(bites_by_ttimes$group), ]
commcovars_by_ttimes$group <- as.numeric(droplevels(commcovars_by_ttimes$names_covar))
ordered_covars <- commcovars_by_ttimes[order(commcovars_by_ttimes$group), ]
end <- cumsum(rle(ordered_covars$group)$lengths)
start <- c(1, lag(end)[-1] + 1)

# Bundle data
data <- list(bites = round(ordered_bites$avg_bites), 
             access = ordered_covars$covar/60, 
             pop = ordered_covars$pop, 
             start = start, 
             end = end, 
             nlocs = nrow(ordered_bites), 
             ncovars = nrow(ordered_covars))

params <- c("beta_access", "beta_0")
inits <- list(beta_access = rnorm(1, 0, 1e-6), beta_0 = rnorm(1, 0, 1e-6))


jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                       n.chains = 3, n.adapt = 500)

samps <- coda.samples(jags_mod, params, n.iter = 10000, thin = 1)
densplot(samps)
traceplot(samps)
summary(samps)

## Model @ district level
model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 0.0001) # Mean hyperparameter for random intercepts
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- 1/(sigma_0*sigma_0)
    
    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    beta_access ~ dnorm(0, 0.0001) # Common slope

    # Likelihood
    for (i in 1:nlocs) {
        exp_bites[i] <- exp(alpha[catch[i]] + beta_access*access[i])*pop[i] # Expectation
        bites[i] ~ dpois(exp_bites[i]) # The actual (random) responses
    }
  }"

# data
data <- list(bites = round(bites_by_ttimes$avg_bites), 
             catch = as.numeric(droplevels(bites_by_ttimes$catchment)), access = bites_by_ttimes$covar/60, 
             nlocs = nrow(bites_by_ttimes), ncatches = length(unique(bites_by_ttimes$catchment)), 
             pop = bites_by_ttimes$pop)
params <- c("beta_access", "beta_0", "sigma_0")
inits <- list(beta_access = rnorm(1, 0, 1e-6), beta_0 = rnorm(1, 0, 1e-6), sigma_0 = rnorm(1, 1, 1e-6))

jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                       n.chains = 3, n.adapt = 500)

samps <- coda.samples(jags_mod, params, n.iter = 10000, thin = 1)
densplot(samps)
traceplot(samps)
summary(samps)

##' Model with grouping var
model <- "model {

    # Priors
    beta_0 ~ dnorm(0, 0.0001) # Mean hyperparameter for random intercepts
    sigma_0 ~ dunif(0, 100) # SD hyperparameter for random intercepts
    tau_0 <- 1/(sigma_0*sigma_0)
    
    for (i in 1:ncatches) {
        alpha[i] ~ dnorm(beta_0, tau_0) # Random intercepts
    }
    
    beta_access ~ dnorm(0, 0.0001) # Common slope

    # Likelihood
    for (i in 1:ncovars) {
      exp_bites[i] <- exp(alpha[catch[i]] + beta_access*access[i])*pop[i] # Expectation
    }
    
    for(k in 1:nlocs) {
      sum_bites[k] <- sum(exp_bites[start[k]:end[k]])
      bites[k] ~ dpois(sum_bites[k]) # The actual (random) responses
    }

  }"

## Where start and end are indexes of groups for each k (so covars should be ordered by districts!)
## And comm covars should be ordered in the same way!
bites_by_ttimes$catch <- as.numeric(droplevels(bites_by_ttimes$catchment))
commcovars_by_ttimes$catch <- as.numeric(droplevels(commcovars_by_ttimes$catchment))
bites_by_ttimes$group <- as.numeric(droplevels(as.factor(bites_by_ttimes$names_covar)))
ordered_bites <- bites_by_ttimes[order(bites_by_ttimes$group), ]
commcovars_by_ttimes$group <- as.numeric(droplevels(commcovars_by_ttimes$names_covar))
ordered_covars <- commcovars_by_ttimes[order(commcovars_by_ttimes$group), ]
end <- cumsum(rle(ordered_covars$group)$lengths)
start <- c(1, lag(end)[-1] + 1)

# Bundle data
data <- list(bites = round(ordered_bites$avg_bites), 
             catch = ordered_covars$catch, 
             access = ordered_covars$covar/60, 
             pop = ordered_covars$pop, 
             start = start, 
             end = end, 
             nlocs = nrow(ordered_bites), 
             ncatches = max(ordered_covars$catch), 
             ncovars = nrow(ordered_covars))

params <- c("beta_access", "beta_0", "sigma_0")
inits <- list(beta_access = rnorm(1, 0, 1e-6), beta_0 = rnorm(1, 0, 1e-6), sigma_0 = rlnorm(1))


jags_mod <- jags.model(textConnection(model), data = data, inits = inits, 
                       n.chains = 3, n.adapt = 500)

samps <- coda.samples(jags_mod, params, n.iter = 10000, thin = 1)
densplot(samps)
traceplot(samps)
summary(samps)

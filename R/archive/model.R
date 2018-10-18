### data
bites <- round(moramanga$monthly.bites*12)
ttimes <- moramanga$study.area.accessibility/60
pop <- moramanga$commune.midpops
nlocs <- length(bites)
expected.mean <- exps.per100k.low/2*moramanga$commune.midpops/1e5

## model
tt.model <- "model {
# reported bites
for (j in 1:nlocs){
# Number of bites
bites[j] ~ dpois(expected.mean[j]*rho[j]/p_rabid[j])
}

# spatial effects
for (g in 1:nlocs){
# Spatial random effect
loc_eff[g] ~ dnorm(0, tau_loc)

# Reporting in place g
logit(rho[g]) <- B_tt*(1/ttimes[g]) + loc_eff[g]

# Background avg in place g
p_rabid[g] <- 1/(1 + sqrt(expected.mean[g]*rho[g])*B_norm)
}

tau_loc <- 1/sd

# priors
B_tt ~ dnorm(0, 0.1)
sd ~ dnorm(0, 0.1)
B_norm ~ dnorm(0.1, 0.1)
}"

data <- list (bites = bites, 
              expected.mean = expected.mean,
              ttimes = ttimes, nlocs = nlocs)

inits <- list(B_tt = rnorm(1, 0, 1e-6), sd = 1,
              B_norm = rnorm(1, 0, 1e-6))

jags_mod <- jags.model(textConnection(tt.model), data = data, inits = inits, n.chains = 5, 
                       n.adapt = 1000)

params <- c("B_tt", "sd", "B_norm")

samps <- coda.samples(jags_mod, params, n.iter = 10000)
burn.in <- 5000
check <- summary(window(samps, start = burn.in))
check
par(mar = c(2, 2, 2, 2))
plot(samps)
# gelman.plot(samps)
gelman.diag(samps)

## parameter estimates
par(mfrow = c(2, 2), mar = c(5, 5, 2,  2))
plot(expected.mean, 1/(1 + sqrt(expected.mean)*check$statistics[,"Mean"]["B_norm"]), bty = "l")
plot(ttimes, plogis(check$statistics[,"Mean"]["B_tt"]*(1/ttimes)), bty = "l")
plot(ttimes, plogis(check$statistics[,"Mean"]["B_tt"]*(1/ttimes) 
                    + rnorm(length(ttimes), mean = 0, sd = check$statistics[,"Mean"]["sd"])),
     bty = "l")

## mean and confidence interval
bites.sim <- matrix(0, nrow = sims, ncol = length(bites))
sims = 1000
for (i in 1:sims){
  bites.sim[i, ] <- rpois(length(p_rabid), expected.mean*plogis(check$statistics[,"Mean"]["B_tt"]*(1/ttimes) 
                                                                + rnorm(length(ttimes), mean = 0, sd = check$statistics[,"Mean"]["sd"]))/p_rabid)
}
rankCI <- function(x){
  upper <- x[order(x)][0.95*length(x)]
  lower <- x[order(x)][0.05*length(x)]
  return(c(lower, upper))
}

upper.CI <- apply(bites.sim, 2, rankCI)[2,]
lower.CI <- apply(bites.sim, 2, rankCI)[1,]
par(mfrow = c(1, 1))
plot(colMeans(bites.sim), pch = 20, col = "darkred", bty = "l")
points(bites, col = "red")
segments(x0 = 1:length(bites), y0 = lower.CI, x1 = 1:length(bites), 
         y1 = upper.CI, col = "darkred")

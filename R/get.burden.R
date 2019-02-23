### Functions for running burden results
## Malavika Rajeev
## Jan 2019

# Get burden using any params -------------------------------------------------------
get.burden <- function(names = comm_data$mdg_cm_, ttimes = comm_data$ttimes_weighted, 
                       pop = comm_data$pop, 
                       param_ttimes = B_ttimes_mada,
                       param_intercept = B_0_mada,
                       p_rabid = 0.5, rho_max = 0.9,
                       max_HDR = 25, min_HDR = 5, 
                       dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                       prob_death = 0.16, nsims = 1000) {
  
  store_deaths <- store_p_rabid <- matrix(NA, length(ttimes), nsims)
  
  for (i in 1:nsims) {
    ## for testing
    #                    names = comm_data$mdg_cm_, ttimes = comm_data$ttimes_weighted; 
    #                    pop = comm_data$pop;
    #                    param_ttimes = B_ttimes_mada;
    #                    param_intercept = B_0_mada;
    #                    p_rabid = 0.5; rho_max = 0.9;
    #                    max_HDR = 25; min_HDR = 5;
    #                    dog_rabies_inc = 0.01; human_exp_rate = 0.39;
    #                    prob_death = 0.16;
    
    bites <- rpois(length(ttimes), exp(param_ttimes*ttimes + param_intercept)*pop)
    mean_exps <- runif(length(pop), min = human_exp_rate*dog_rabies_inc*pop/max_HDR, 
                       max = human_exp_rate*dog_rabies_inc*pop/min_HDR)
    rabid_exps <- rpois(length(mean_exps), mean_exps)
    p_rabid_t <- ifelse(bites == 0 & rabid_exps == 0, 0,
                        ifelse(bites*p_rabid/rabid_exps > rho_max, (rho_max*rabid_exps)/bites, p_rabid))
    reported_rabid <- round(bites*p_rabid_t)
    reported_rabid <- ifelse(reported_rabid > rabid_exps, rabid_exps, reported_rabid)
    deaths <- rbinom(length(ttimes), rabid_exps - reported_rabid, prob_death)
    store_deaths[, i] <- deaths
    store_p_rabid[, i] <- p_rabid_t
  }
  
  results <- cbind(get.meanCI(store_deaths), get.meanCI(store_p_rabid), rep(p_rabid, length(mean_exps)))
  colnames(results) <- c("deaths_mean", "deaths_lowerCI", "deaths_upperCI",
                         "p_rabid_mean", "p_rabid_lowerCI", "p_rabid_upperCI", "pr")
  results <- cbind(names, ttimes, pop, results)
  return(results)
}


# Get flat estimate of burden -----------------------------------------------------------------
get.burden.flat <- function(covars = comm_data, ttimes = comm_data$ttimes_weighted, pop = comm_data$pop, 
                            param_ttimes = B_ttimes_mada,
                            param_intercept = B_0_mada,
                            p_rabid = 0.5, rho = 0.9,
                            max_HDR = 25, min_HDR = 5, 
                            dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                            prob_death = 0.16, nsims = 1000) {
  
  store_deaths <- matrix(NA, length(ttimes), nsims)
  
  for (i in 1:nsims) {
    bites <- rpois(length(ttimes), exp(param_ttimes*ttimes + param_intercept)*pop)
    reported_rabid <- rbinom(length(ttimes), size = bites, prob = 0.5)
    total_rabid <- round(reported_rabid/rho)
    deaths <- rbinom(length(mean_bites), total_rabid - reported_rabid, prob_death)
    store_deaths[, i] <- deaths
  }
  
  results <- get.meanCI(store_deaths)
  colnames(results) <- c("deaths_mean", "deaths_lowerCI", "deaths_upperCI")
  results <- cbind(covars, results)
  return(results)
}

# Bootstrapped CI's ---------------------------------------------------------------------------
boots <- function(x, nboot = 1000){
  n = length(x)
  xbar = mean(x)
  tmpdata = sample(x,n*nboot, replace=TRUE)
  bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)
  bsmeans = colMeans(bootstrapsample)
  deltastar = bsmeans - xbar
  d = quantile(deltastar, c(0.05, 0.95), na.rm = TRUE)
  ## Get from quantiles
  ci = xbar - c(d[2], d[1])
  
  ## Sorted CIs
  sorteddeltastar = sort(deltastar)
  # # Look at the sorted results
  # Find the .05 and .95 critical values of deltastar
  d9alt = sorteddeltastar[5]
  d1alt = sorteddeltastar[950]
  # Find and print the 80% confidence interval for the mean
  ciAlt = xbar - c(d1alt,d9alt)
  # cat(’Alternative confidence interval: ’,ciAlt, ’\n’)
  return(ci)
}


# Get mean and CI from matrix of simulations --------------------------------------------------
get.meanCI <- function(matrix_by_row, nboots = 1000) {
  mean_rows <- apply(matrix_by_row, 1, mean)
  CIs <- apply(matrix_by_row, 1, boots, nboot = nboots)
  lower <- unlist(CIs)[1, ] 
  upper <- unlist(CIs)[2, ]
  summ <- cbind(mean_rows, lower, upper)
  colnames(summ) <- c("mean", "lower", "upper")
  return(summ)
}

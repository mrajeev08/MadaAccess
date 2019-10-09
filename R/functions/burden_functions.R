get.burden.fixed <- function(predicted_bites, pop, p_rabid = 0.2, rho_max = 0.9,
                       HDR = 25, dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                       prob_death = 0.16) {
  ## testing
  # predicted_bites = preds$predicted; pop = preds$pop; p_rabid = 0.2; rho_max = 0.9;
  # HDR = 25; dog_rabies_inc = 0.01; human_exp_rate = 0.39;
  # prob_death = 0.16;
  
  rabid_exps <- human_exp_rate*dog_rabies_inc*pop/HDR
  reported_exps <- ifelse(predicted_bites*p_rabid > rabid_exps*rho_max, 
                          rabid_exps*rho_max, predicted_bites*p_rabid)
  p_rabid_adj <- reported_exps/predicted_bites
  reporting <- reported_exps/rabid_exps
  deaths <- (rabid_exps - reported_exps)*prob_death
  averted <- reported_exps*prob_death
  
  results <- data.frame(predicted = predicted_bites, deaths, averted, reporting, 
                             p_rabid_adj = p_rabid_adj, p_rabid, HDR)
  return(results)
}


# Get burden using any params -------------------------------------------------------
comb <- function(...) {
  mapply('cbind', ..., SIMPLIFY=FALSE)
}

get.burden <- function(predicted_bites, pop, p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                                  max_HDR = 25, min_HDR = 5, 
                                  dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                                  prob_death = 0.16, nsims = 1000) {
  
  results <- foreach (i = 1:nsims, .combine = comb) %dopar% {
    ## for testing
    # predicted_bites = preds$predicted; pop = preds$pop, p_rab_min = 0.2; p_rab_max = 0.6; rho_max = 0.9;
    # max_HDR = 25; min_HDR = 5; 
    # dog_rabies_inc = 0.01; human_exp_rate = 0.39; 
    # prob_death = 0.16; nsims = 10
    
    
    bites <- rpois(length(predicted_bites), predicted_bites)
    hdr <- runif(length(predicted_bites), min = min_HDR, max = max_HDR)
    rabid_exps <- rpois(length(predicted_bites), human_exp_rate*dog_rabies_inc*pop/hdr)
    p_rabid <- runif(length(predicted_bites), min = p_rab_min, max = p_rab_max)
    p_rabid_t <- ifelse(bites == 0 & rabid_exps == 0, 0,
                        ifelse(bites*p_rabid/rabid_exps > rho_max, (rho_max*rabid_exps)/bites, p_rabid))
    reported_rabid <- round(bites*p_rabid_t)
    reported_rabid <- ifelse(reported_rabid > rabid_exps, rabid_exps, reported_rabid)
    deaths <- rbinom(length(predicted_bites), rabid_exps - reported_rabid, prob_death)
    averted <- rbinom(length(predicted_bites), reported_rabid, prob_death)
    list(store_averted = averted,
         store_deaths = deaths, store_p_rabid = p_rabid_t)
  }

  results <- cbind(get.meanCI(results[["store_deaths"]]), get.meanCI(results[["store_p_rabid"]]), 
                   get.meanCI(results[["store_averted"]]))
  colnames(results) <- c("deaths_mean", "deaths_lowerCI", "deaths_upperCI",
                         "p_rabid_mean", "p_rabid_lowerCI", "p_rabid_upperCI",
                         "averted_mean", "averted_lowerCI", "averted_upperCI")
  results <- data.frame(results)
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

### Functions for running burden results
## Malavika Rajeev
## Jan 2019

# Get burden or reporting (fixed) -------------------------------------------------------------
## Function for reporting
get.burden.fixed <- function(pop = 1e5, names, covar, B_covar, B_0, hdr = 25, incidence = 0.01, 
                              exp_rate = 0.39, p_rabid = 0.6, rho_max = 0.98, 
                              p_death = 0.16, scenario = 0, scale = FALSE, 
                              type = "reporting", slope = 1, intercept = 0, inc_max = inc100k_max, 
                              inc_min = inc100k_min) {
  
  bites_100k <- exp(B_covar*covar + B_0)*pop
  
  if(scale == TRUE) {
    ## Constrained and scaled incidence
    inc <- slope*pop + intercept
    inc[inc > inc_max] <- inc_max
    inc[inc < inc_min] <- inc_min
    R_100k <- pop*inc
    
  } else {
    R_100k <- pop/hdr*incidence*exp_rate
  }
  
  if (type == "reporting"){
    rho <- bites_100k*(ifelse(bites_100k*p_rabid > R_100k, rho_max*R_100k/bites_100k, p_rabid))/R_100k
    return(rho)
  } else {
    unreported <- R_100k - bites_100k*(ifelse(bites_100k*p_rabid > R_100k, 
                                              rho_max*R_100k/bites_100k, p_rabid))
    deaths <- 0.16*unreported
    if(scale == TRUE) {
      return(as.data.frame(list(names = names, scenario = scenario, pop = pop, covar = covar, 
                                B_covar = B_covar, B_0 = B_0, slope = slope, 
                                p_rabid = p_rabid, rho_max = rho_max, deaths = deaths)))
    } else {
      return(as.data.frame(list(names = names, scenario = scenario, pop = pop, covar = covar, 
                                B_covar = B_covar, B_0 = B_0, 
                                hdr = hdr, p_rabid = p_rabid, rho_max = rho_max, deaths = deaths)))
    }
  }
}

# Get flat estimate of burden -----------------------------------------------------------------
get.burden.flat <- function(names = comm_data$mdg_cm_, covar = comm_data$covar_weighted, 
                            pop = comm_data$pop, scenario = comm_data$scenario, 
                            rho = 0.9, prob_death = 0.16, nsims = 1000) {
  
  store_deaths <- matrix(NA, length(covar), nsims)
  
  for (i in 1:nsims) {
    bites <- rpois(length(covar), exp(param_covar*covar + param_intercept)*pop)
    reported_rabid <- rbinom(length(covar), size = bites, prob = 0.5)
    total_rabid <- round(reported_rabid/rho)
    deaths <- rbinom(length(mean_bites), total_rabid - reported_rabid, prob_death)
    store_deaths[, i] <- deaths
  }
  
  results <- get.meanCI(store_deaths)
  colnames(results) <- c("deaths_mean", "deaths_lowerCI", "deaths_upperCI")
  results <- cbind(names, scenario, covar, pop, results)
  return(results)
}

# Get burden using any params -------------------------------------------------------
get.burden.stochastic <- function(names = comm_data$mdg_cm_, covar = comm_data$covar_weighted, 
                           pop = comm_data$pop, scenario = comm_data$scenario, 
                           param_covar = B_covar_mada,
                           param_intercept = B_0_mada,
                           p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                           max_HDR = 25, min_HDR = 5, 
                           dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                           prob_death = 0.16, nsims = 1000, cores = 3) {
  
  comb <- function(...) {
    mapply('cbind', ..., SIMPLIFY=FALSE)
  }
  
    # Initiate cluster
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  results <- foreach (i = 1:nsims, .combine = comb) %dopar% {
    ## for testing
    #                    names = comm_data$mdg_cm_, covar = comm_data$covar_weighted; 
    #                    pop = comm_data$pop; scenario = comm_data$scenario;
    #                    param_covar = B_covar_mada;
    #                    param_intercept = B_0_mada;
    #                    p_rabid = 0.5; rho_max = 0.9;
    #                    max_HDR = 25; min_HDR = 5;
    #                    dog_rabies_inc = 0.01; human_exp_rate = 0.39;
    #                    prob_death = 0.16;
    
    bites <- rpois(length(covar), exp(param_covar*covar + param_intercept)*pop)
    hdr <- runif(length(covar), min = min_HDR, max = max_HDR)
    rabid_exps <- rpois(length(covar), human_exp_rate*dog_rabies_inc*pop/hdr)
    p_rabid <- runif(length(covar), min = p_rab_min, max = p_rab_max)
    p_rabid_t <- ifelse(bites == 0 & rabid_exps == 0, 0,
                        ifelse(bites*p_rabid/rabid_exps > rho_max, (rho_max*rabid_exps)/bites, p_rabid))
    reported_rabid <- round(bites*p_rabid_t)
    reported_rabid <- ifelse(reported_rabid > rabid_exps, rabid_exps, reported_rabid)
    deaths <- rbinom(length(covar), rabid_exps - reported_rabid, prob_death)
    averted <- rbinom(length(covar), reported_rabid, prob_death)
    list(store_averted = averted,
         store_deaths = deaths, store_p_rabid = p_rabid_t)
  }
  stopCluster(cl)
  
  results <- cbind(get.meanCI(results[["store_deaths"]]), get.meanCI(results[["store_p_rabid"]]), 
                   get.meanCI(results[["store_averted"]]))
  colnames(results) <- c("deaths_mean", "deaths_lowerCI", "deaths_upperCI",
                         "p_rabid_mean", "p_rabid_lowerCI", "p_rabid_upperCI",
                         "averted_mean", "averted_lowerCI", "averted_upperCI")
  results <- as.data.frame(list(names = names, scenario = scenario, covar = covar, pop = pop, 
                                results))
  return(results)
}


# Get # Vials used by each clinic -------------------------------------------------------------
# get.mean.vials <- function(comm_mat, param_covar = B_covar_mada,
#                            param_intercept = B_0_mada, param_mu = 0.2, param_mu_intercept = 0.6, 
#                            param_k = 0.9, param_k_intercept = 0.1, nsims = 1000) {
#   
#   ## testing
#   param_covar = B_covar_mada; param_intercept = B_0_mada;
#   param_mu = 0.029; param_mu_intercept = -0.307; 
#   param_k = 0.022; param_k_intercept = -0.39; nsims = 10;
# 
#   comm_mat %>%
#     mutate(bites = inv.logit(param_covar*covar/60 + param_intercept)*pop) %>%
#     group_by(scenario, catchment) %>%
#     summarize(bites = sum(bites)) %>%
#     mutate(k = exp(param_k*bites/100 + param_k_intercept), 
#            mu = exp(param_mu*bites/100 + param_mu_intercept))-> summed
#   
#   store_vials <- matrix(NA, nrow(summed), nsims)
#   
#   foreach(i = iter(summed, by = "row")) %do% {
#     for (t in 1:nsims) {
#     
#       ## for testing
#       i = summed[1, ]
#       ts <- rnbinom(365, mu = i$mu, size = i$k)
#       
#       vials <- c(sum(ceiling(ts/2)))
#     }
#   }
#   
#   
#   results <- cbind(get.meanCI(store_deaths), get.meanCI(store_p_rabid), get.meanCI(store_averted))
#   colnames(results) <- c("deaths_mean", "deaths_lowerCI", "deaths_upperCI",
#                          "p_rabid_mean", "p_rabid_lowerCI", "p_rabid_upperCI",
#                          "averted_mean", "averted_lowerCI", "averted_upperCI")
#   results <- as.data.frame(list(names = names, scenario = scenario, covar = covar, pop = pop, 
#                                 results))
#   return(results)
# }

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

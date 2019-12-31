####################################################################################################
##' Prediction functions for getting bites, deaths, and vials 
##' Details: Wrapper function for getting bites, deaths, and vials from inputs 
##' Author: Malavika Rajeev
####################################################################################################

##' Function for getting bites from model inputs (return a matrix)
##' ------------------------------------------------------------------------------------------------
predict.bites <- function(ttimes, pop, catch, names,
                           beta_ttimes, beta_0, beta_pop, sigma_0, known_alphas, 
                           pop_predict = "addPop", intercept = "random",
                           trans = 1e5, known_catch = TRUE, nsims = 1000) {
  
  foreach(i = 1:nsims, .combine = cbind) %do% {
    if(intercept == "random") {
      ## draw catchment level effects
      ## first make catchment a factor and then drop levels and convert to numeric
      catch_val <- as.numeric(droplevels(as.factor(catch)))
      
      alpha <- rnorm(max(catch_val, na.rm = TRUE), mean = beta_0, sd = sigma_0)
      alpha <- alpha[catch_val]
      
      ## get alpha so that if known_catch is TRUE it pulls from estimates
      ## known alphas have to be the same length as the data (potentially change this part!)
      if(known_catch == TRUE) {
        alpha <- ifelse(is.na(known_alphas) | known_alphas == 0, alpha, known_alphas)
      } 
      
      if (pop_predict == "flatPop") {
        exp_bites <- exp(alpha + beta_ttimes*ttimes)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(alpha + beta_ttimes*ttimes + beta_pop*pop/trans) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(alpha + beta_pop*pop/trans)
      }
    }
    
    if(intercept == "fixed") {
      if (pop_predict == "flatPop") {
        exp_bites <- exp(beta_0 + beta_ttimes*ttimes)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(beta_0 + beta_ttimes*ttimes + beta_pop*pop/trans) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(beta_0 + beta_pop*pop/trans)
      }
      exp_bites
    }
    
    bites <- rpois(length(exp_bites), exp_bites)
    
  } -> bite_mat
  
  return(bite_mat)
}

## Function for getting deaths from input bites (return a matrix)
## Work in incidence! (so you don't have to worry about binomial limitations)
predict.deaths <- function(bite_mat, pop,
                           p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                           max_HDR = 25, min_HDR = 5, dog_rabies_inc = 0.01, 
                           human_exp_rate = 0.39, 
                           prob_death = 0.16) {
  
  ## Drawing rabies exposures
  hdr <- runif(length(bite_mat), min = min_HDR, max = max_HDR)
  pop_vec <- rep(pop, ncol(bite_mat))
  rabid_exps <- rpois(length(bite_mat), human_exp_rate*dog_rabies_inc*pop_vec/hdr)
  
  ## Getting proportion rabid
  p_rabid <- runif(length(bite_mat), min = p_rab_min, max = p_rab_max)
  reported_rabid <- bite_mat*p_rabid
  max_reported <- rabid_exps*rho_max ## so that reporting is set at max
  reported_rabid <- ifelse(reported_rabid > max_reported, max_reported, reported_rabid)
  
  ## Simulating deaths
  deaths <- matrix(rbinom(length(bite_mat), round(rabid_exps - reported_rabid), prob_death),
                   nrow = nrow(bite_mat), ncol = ncol(bite_mat))
  averted <- matrix(rbinom(length(bite_mat), round(reported_rabid), prob_death), 
                    nrow = nrow(bite_mat), ncol = ncol(bite_mat))
  p_rabid <- matrix(reported_rabid/as.vector(bite_mat), nrow = nrow(bite_mat), 
                    ncol = ncol(bite_mat))
  reporting <- matrix(reported_rabid/rabid_exps, nrow = nrow(bite_mat), 
                      ncol = ncol(bite_mat))
  check <- list(deaths = deaths, averted = averted, p_rabid = p_rabid,
                reporting = reporting)
  
  return(check)
}

##' Get burden or reporting (fixed) -------------------------------------------------------------
get.burden.fixed <- function(bite_mat, pop, hdr = 25, incidence = 0.01, 
                             exp_rate = 0.39, p_rabid = 0.6, rho_max = 0.98, 
                             prob_death = 0.16, scale = FALSE, slope = 1, intercept = 0, 
                             inc_max = inc100k_max, 
                             inc_min = inc100k_min) {
  
  if(scale == TRUE) {
    ## Constrained and scaled incidence
    inc <- slope*pop + intercept
    inc[inc > inc_max] <- inc_max
    inc[inc < inc_min] <- inc_min
    rabid_exps <- pop*inc
  } else {
    rabid_exps <- pop/hdr*incidence*exp_rate
  }
  
  rabid_exps <- matrix(rabid_exps, nrow = nrow(bite_mat), 
                      ncol = ncol(bite_mat))
  
  max_reported <- rabid_exps*rho_max ## so that reporting is set at max
  reported_rabid <- bite_mat*p_rabid
  reported_rabid <- ifelse(reported_rabid > max_reported, max_reported, reported_rabid)
  
  ## Simulating deaths
  deaths <- (rabid_exps - reported_rabid)*prob_death
  return(deaths)
}

##' Function for getting vials from input bites
##' ------------------------------------------------------------------------------------------------
get.vials <- function(x) {
  day0 <- round(runif(x, min = 1, max = 365))
  days <- data.table(days = c(day0, day0 + 3, day0 + 7))
  return(list(vials = sum(days[, .(.N), by = days][, ceiling(N/2)]), 
              throughput = mean(days[, .(.N), by = days]$N)))
}

##' Helper Functions 
##' ------------------------------------------------------------------------------------------------
## Function for getting bootstrapped CIS
boots_mean <- function(data, indices) {mean(data[indices], na.rm = TRUE)}
boots_median <- function(data, indices) {median(data[indices], na.rm = TRUE)}

get.boots.dt <- function(vector, type_CI = "basic", names,
                         func = boots_mean, nsims = 999) {
  check <- unique(vector)
  
  if(length(vector[is.na(vector)]) == length(vector) |
     (length(check[!is.na(check)]) == 1 & sum(check[!is.na(check)]) > 0)){
    out <- data.table(point_est = NA, upper = NA, lower = NA)
    names(out) <- names
  } else {
    samples <- boot(vector, func, R = nsims)
    ests <- boot.ci(samples, type = type_CI)
    out <- data.table(point_est = ests$t0, upper = ests[[4]][5], lower = ests[[4]][4])
    names(out) <- names
  }
  return(out)
}

multicomb <- function(x, ...) {
  mapply(cbind, x, ..., SIMPLIFY = FALSE)
}

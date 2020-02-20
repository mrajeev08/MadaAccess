# ------------------------------------------------------------------------------------------------ #
#' Prediction functions for getting bites, deaths, and vials
#' Details: Wrapper function for getting bites, deaths, and vials from inputs
# ------------------------------------------------------------------------------------------------ #

# 1. Function for getting bites from model inputs (return a matrix) -------------------------------
#' Function for getting bite incidence or # of bites from model inputs
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions triangle

predict.bites <- function(ttimes, pop, catch, names, group, ngroups,
                          beta_ttimes, beta_0, beta_pop, sigma_0, sigma_e, known_alphas, 
                          pop_predict = "addPop", intercept = "random",
                          trans = 1e5, known_catch = TRUE, nsims = 1000, 
                          type = "bites") {
  
  foreach(i = 1:nsims, .combine = cbind) %do% {
    
    epsilon <- rnorm(length(ttimes), mean = 0, sd = sigma_e) # overdispersion 
      
    if(intercept == "random") {
      # draw catchment level effects (pull this out for foreach)
      # first make catchment a factor and then drop levels and convert to numeric
      catch_val <- as.numeric(droplevels(as.factor(catch)))
      
      alpha <- rnorm(max(catch_val, na.rm = TRUE), mean = beta_0, sd = sigma_0)
      alpha <- alpha[catch_val]
      
      # get alpha so that if known_catch is TRUE it pulls from estimates
      # known alphas have to be the same length as the data (potentially change this part!)
      if(known_catch == TRUE) {
        alpha <- ifelse(is.na(known_alphas) | known_alphas == 0, alpha, known_alphas)
      } 
      
      if (pop_predict == "flatPop") {
        exp_bites <- exp(alpha + beta_ttimes*ttimes + epsilon)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(alpha + beta_ttimes*ttimes + beta_pop*pop/trans + epsilon) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(alpha + beta_pop*pop/trans + epsilon)
      }
    }
    
    if(intercept == "fixed") {
      if (pop_predict == "flatPop") {
        exp_bites <- exp(beta_0 + beta_ttimes*ttimes + epsilon)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(beta_0 + beta_ttimes*ttimes + beta_pop*pop/trans + epsilon) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(beta_0 + beta_pop*pop/trans + epsilon)
      }
    }
    if(type == "bites") {
      exp_bites <- rpois(length(exp_bites), exp_bites)
    }
    exp_bites
  } -> bite_mat
  
  return(bite_mat)
}

#  Predict deaths and other key decision tree outputs -----------------------------------------
# Function for getting deaths from input bites (return a matrix)
#' Title
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions
predict.deaths <- function(bite_mat, pop,
                           p_rab_min = 0.2, p_rab_max = 0.6, p_rab_mode = NULL,
                           rho_max = 0.9,
                           exp_min = 15/1e5, exp_max = 76/1e5, exp_mode = NULL,
                           prob_death = 0.16, dist = "uniform") {
  
  # Getting rabies exposures incidence and p_rabid
  if(dist == "uniform") {
    human_exp_inc <- runif(n = length(bite_mat), min = exp_min, max = exp_max)
    p_rabid <- runif(n = length(bite_mat), min = p_rab_min, max = p_rab_max)
  }
  
  if(dist == "triangle") {
    
    if(is.null(p_rab_mode)) {
      p_rab_mode <- (p_rab_min + p_rab_max)/2
    }
    
    if(is.null(exp_mode)) {
      exp_mode <- (exp_min + exp_max)/2
    }
    
    human_exp_inc <- rtriangle(n = length(bite_mat), a = exp_min, b = exp_max, c = exp_mode)
    p_rabid <- rtriangle(n = length(bite_mat), a = p_rab_min, b = p_rab_max, c = p_rab_mode)
  }
  
  # Rabied exposures
  pop_vec <- rep(pop, ncol(bite_mat))
  rabid_exps <- human_exp_inc*pop_vec # expected rabid exposures
  
  #  Rabid reported bites
  reported_rabid <- bite_mat*p_rabid 
  
  # Constrain so that reporting is set at max
  max_reported <- rabid_exps*rho_max 
  reported_rabid <- ifelse(reported_rabid > max_reported, max_reported, reported_rabid)

  # Estimating reporting and proportion rabid 
  # not binomially because we don't want to deal with zeros/undefined!
  p_rabid <- matrix(reported_rabid/as.vector(bite_mat), nrow = nrow(bite_mat), 
                    ncol = ncol(bite_mat))
  reporting <- matrix(reported_rabid/rabid_exps, nrow = nrow(bite_mat), 
                      ncol = ncol(bite_mat))
  
  # Estimating deaths and deaths averted
  unreported <- rpois(length(rabid_exps), rabid_exps - reported_rabid)
  deaths <- matrix(rbinom(length(rabid_exps), size = unreported, prob = prob_death),
                    nrow = nrow(bite_mat), ncol = ncol(bite_mat))
  reported <- rpois(length(rabid_exps), reported_rabid)
  averted <- matrix(rbinom(length(rabid_exps), size = reported, prob = prob_death), 
                    nrow = nrow(bite_mat), ncol = ncol(bite_mat))
  
  out <- list(deaths = deaths, averted = averted, p_rabid = p_rabid,
                reporting = reporting, rabid_exps = matrix(rabid_exps, nrow = nrow(bite_mat),
                                                           ncol = ncol(bite_mat)))
  
  return(out)
}

# Get burden or reporting (fixed) -------------------------------------------------------------
# Use human_exp_rate instead of incidence*hdr*rho_max!
get.burden.fixed <- function(bite_mat, pop, hdr = 25, incidence = 0.01, 
                             exp_rate = 0.39, p_rabid = 0.6, rho_max = 0.9, 
                             prob_death = 0.16, scale = FALSE, slope = 1, intercept = 0, 
                             inc_max = inc100k_max, 
                             inc_min = inc100k_min) {
  
  if(scale == TRUE) {
    # Constrained and scaled incidence
    inc <- slope*pop + intercept
    inc[inc > inc_max] <- inc_max
    inc[inc < inc_min] <- inc_min
    rabid_exps <- pop*inc
  } else {
    rabid_exps <- pop/hdr*incidence*exp_rate
  }
  
  rabid_exps <- matrix(rabid_exps, nrow = nrow(bite_mat), 
                      ncol = ncol(bite_mat))
  
  max_reported <- rabid_exps*rho_max # so that reporting is set at max
  reported_rabid <- bite_mat*p_rabid
  reported_rabid <- ifelse(reported_rabid > max_reported, max_reported, reported_rabid)
  
  # Simulating deaths
  deaths <- (rabid_exps - reported_rabid)*prob_death
  return(deaths)
}


# Helper functions for getting incidence from HDR/p_exp/dog_inc -------------------------------
hdr_from_inc <- function(inc = 48, pop = 1e5, p_exp = 0.39, dog_inc = 0.01) {
  1/(inc/pop/dog_inc/p_exp)
}

inc_from_hdr <- function(hdr = 5, pop = 1e5, p_exp = 0.39, dog_inc = 0.01) {
  p_exp*dog_inc*pop/hdr
}


# Function for getting vials from input bites -------------------------------------------------
get.vials <- function(x) {
  day0 <- floor(runif(x, min = 1, max = 365))
  days <- tabulate(c(day0, day0 + 3, day0 + 7))
  return(list(vials = sum(ceiling(days/2)), 
              throughput = mean(days)))
}


# Helper Functions -----------------------------------------------------------------------------
# Function for getting bootstrapped CIS
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

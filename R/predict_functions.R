# ------------------------------------------------------------------------------------------------ #
#' Prediction functions for getting bites, deaths, and vials
#' Details: Wrapper function for getting bites, deaths, and vials from inputs
# ------------------------------------------------------------------------------------------------ #

#' Predict bites given model parameter estimates
#' Takes covariates and estimates from fitted models to predict either the number of bites or
#' expectation.
#' @param ttimes, pop, catch, names = from data frame, pass as NULL/NA if not included in the model
#' @param beta_ttimes, beta_0, beta_pop, sigma_0, known_alphas, sigma_e = from model estimates,
#' pass as NULL/NA if not included in the model
#' @param pop_predict type of population scaling in the model
#' @param intercept type of intercept in the model
#' @param known_catch whether to use the estimates of the catchment level intercept
#' @param trans value to normalize the population covariate by
#' @param OD whether model includes an overdispersion parameter (sigma_e)
#' @param par_type predict either the expectation (pred_type = "exp") or number of bites
#' (pred_type = "bites") from poisson given expecatation
#' @param pred_type If par_type = "point_est" uses point estimate of parameters.
#' If par_type = "posterior" need to pass vectors (or matrix if passing known_alphas) with length
#' equal to nsims (use \code[get_samps])
#' for this.
#' @param error_n set to 1 when generating expectated range of preds for a given covariate value,
#' defaults to length of the data when predicting for a set of locations
#' @param nsims number of simulations to run
#' @return Matrix of simulated predictions (nrow = length of data, ncol = nsims)
#' @section Dependencies: foreach

predict_bites <- function(ttimes, pop, catch, names,
                          beta_ttimes, beta_0, beta_pop, sigma_0, known_alphas, sigma_e,
                          pop_predict = c("flatPop", "addPop", "onlyPop"),
                          intercept = c("random", "fixed"), trans = 1e5, known_catch = TRUE,
                          OD = FALSE, par_type = c("point_est", "posterior"),
                          pred_type = c("bites", "exp"),
                          error_n = length(ttimes), nsims = 1000, ...) {
  foreach(i = 1:nsims, .combine = cbind) %do% {
    if (par_type == "posterior") {
      beta_0_val <- beta_0[i]
      beta_ttimes_val <- beta_ttimes[i]
      beta_pop_val <- beta_pop[i]
      sigma_0_val <- sigma_0[i]
      sigma_e_val <- sigma_e[i]
    }

    if (par_type == "point_est") {
      beta_0_val <- beta_0
      beta_ttimes_val <- beta_ttimes
      beta_pop_val <- beta_pop
      sigma_0_val <- sigma_0
      sigma_e_val <- sigma_e
    }

    if (OD == TRUE) {
      errs <- rnorm(error_n, mean = 0, sd = sigma_e_val)
    } else {
      errs <- 0
    }

    if (intercept == "random") {
      if (known_catch == FALSE) {
        catch_val <- as.numeric(droplevels(as.factor(catch)))
        alpha <- rnorm(max(catch_val, na.rm = TRUE), mean = beta_0_val, sd = sigma_0_val)
        alpha <- alpha[catch_val]
      }

      if (known_catch == TRUE) {
        if (par_type == "posterior") {
          alphas <- unlist(known_alphas[i, ]) # passed as matrix row of posterior samples!
        } else {
          alphas <- known_alphas
        }
        alpha <- alphas[catch]
      }

      if (pop_predict == "flatPop") {
        exp_bites <- exp(alpha + beta_ttimes_val * ttimes + errs) * pop
      }

      if (pop_predict == "addPop") {
        exp_bites <- exp(alpha + beta_ttimes_val * ttimes + beta_pop_val * pop / trans + errs)
      }

      if (pop_predict == "onlyPop") {
        exp_bites <- exp(alpha + beta_pop_val * pop / trans + errs)
      }
    }

    if (intercept == "fixed") {
      if (pop_predict == "flatPop") {
        exp_bites <- exp(beta_0_val + beta_ttimes_val * ttimes + errs) * pop
      }

      if (pop_predict == "addPop") {
        exp_bites <- exp(beta_0_val + beta_ttimes_val * ttimes + beta_pop_val * pop / trans + errs)
      }

      if (pop_predict == "onlyPop") {
        exp_bites <- exp(beta_0_val + beta_pop_val * pop / trans + errs)
      }
    }
    if (pred_type == "bites") {
      exp_bites <- rpois(length(exp_bites), exp_bites)
    }

    exp_bites
  } -> bite_mat

  return(bite_mat)
}

#' Predict deaths from reported bite incidence
#' Takes matrix of predicted reported bites from \code[predict_bites] and applies decision tree
#' framework to simulate reporting, deaths, and deaths averted.
#' @param p_rab_min minimum estimate of proportion of reported bites that are rabies exposures
#' @param p_rab_max maximum estimate of proportion of reported bites that are rabies exposures
#' @param p_rab_mode median estimate of proportion of reported bites that are rabies exposures
#' @param exp_min minimum estimate of rabies exposure incidence
#' @param exp_max maximum estimate of rabies exposure incidence
#' @param exp_mode median estimate of rabies exposure incidence
#' @param rho_max maximum possible reporting
#' @param prob_death probability of death in the absence of PEP given a genuine rabies exposure
#' @param dist "uniform" for sampling probabilities from uniform distribution and "triangle" for
#' sampling probabilities from a triangular distribution
#' @param exp_scaled whether to ignore incidence parameters and instead use a vector of incidence
#' estimates for each location
#' @details If dist = "uniform", the _mode parameters are not used. If dist = "triangle",
#' the mode is used if passed, but if NULL defaults to median of min - max.
#' median of min - max.
#' @return List of matrices of simulated deaths, deaths averted, reporting, and the parameter estimates
#' (p_rabid, the proportion of bites reported that were rabid and rabid_exps, exposure incidence) used
#' to generate those predictions.
#' @section Dependencies: triangle

predict_deaths <- function(bite_mat, pop, p_rab_min = 0.2, p_rab_max = 0.6, p_rab_mode = NULL,
                           exp_min = 15 / 1e5, exp_max = 76 / 1e5, exp_mode = NULL, rho_max = 0.9,
                           prob_death = 0.16, dist = "uniform", exp_scaled = NULL, ...) {

  # Getting rabies exposures incidence and p_rabid
  if (dist == "uniform") {
    human_exp_inc <- runif(n = length(bite_mat), min = exp_min, max = exp_max)
    p_rabid <- runif(n = length(bite_mat), min = p_rab_min, max = p_rab_max)
  }

  if (dist == "triangle") {
    if (is.null(p_rab_mode)) {
      p_rab_mode <- (p_rab_min + p_rab_max) / 2
    }

    if (is.null(exp_mode)) {
      exp_mode <- (exp_min + exp_max) / 2
    }

    human_exp_inc <- rtriangle(n = length(bite_mat), a = exp_min, b = exp_max, c = exp_mode)
    p_rabid <- rtriangle(n = length(bite_mat), a = p_rab_min, b = p_rab_max, c = p_rab_mode)
  }

  # Overide the previous kind if scaled!
  if (!is.null(exp_scaled)) {
    human_exp_inc <- exp_scaled
  }

  # Rabid exposures
  pop_vec <- rep(pop, ncol(bite_mat))
  rabid_exps <- human_exp_inc * pop_vec # expected rabid exposures

  #  Rabid reported bites
  reported_rabid <- bite_mat * p_rabid

  # Constrain so that reporting is set at max
  max_reported <- rabid_exps * rho_max
  reported_rabid <- ifelse(reported_rabid > max_reported, max_reported, reported_rabid)

  # Estimating reporting and proportion rabid
  # not binomially because we don't want to deal with zeros/undefined!
  p_rabid <- matrix(reported_rabid / as.vector(bite_mat),
    nrow = nrow(bite_mat),
    ncol = ncol(bite_mat)
  )
  reporting <- matrix(reported_rabid / rabid_exps,
    nrow = nrow(bite_mat),
    ncol = ncol(bite_mat)
  )

  # Estimating deaths and deaths averted
  unreported <- rabid_exps - reported_rabid
  deaths <- matrix(unreported * prob_death, nrow = nrow(bite_mat), ncol = ncol(bite_mat))
  averted <- matrix(reported_rabid * prob_death,
    nrow = nrow(bite_mat), ncol = ncol(bite_mat)
  )

  out <- list(
    deaths = deaths, averted = averted, p_rabid = p_rabid,
    reporting = reporting, rabid_exps = matrix(rabid_exps,
      nrow = nrow(bite_mat),
      ncol = ncol(bite_mat)
    )
  )

  return(out)
}

#' Helper function for constraining incidence when scaling by pop
#' From model where y intercept is set to either the max or the minimum and incidence scales
#' by slope with population size (if negatively, then not below a minimum threshold and if
#' positively then not above a maximum threshold).
#' @param slope slope that scales incidence with population
#' @param pop vector of population sizes
#' @param max maximum possible incidence
#' @param min minimum possible incidence
#' @return Vector of scaled incidence by pop.
#' @section Dependencies: None
constrained_inc <- function(slope, pop, max, min) {
  intercept <- ifelse(slope > 0, min, max)
  inc <- slope * pop + intercept
  inc[inc > max] <- max
  inc[inc < min] <- min
  return(inc)
}

#' Helper function to simulate vials
#' @details This function takes the average number of reported bites annually, and samples reporting dates
#' and subsequent follow-up dates (on days 3 + 7 per the new abridged regimen)
#' to estimate vial demand based on sharing of vials (2 patients per vial on a given day)
#' @param x numeric number of bites reported in a given year
#' @return List of simulated annual vial demand and average daily throughput.
#' @section Dependencies: None
get_vials <- function(x) {
  day0 <- floor(runif(rpois(1, x), min = 1, max = 365))
  days <- tabulate(c(day0, day0 + 3, day0 + 7))
  return(list(
    vials = sum(ceiling(days / 2)),
    throughput = mean(days)
  ))
}

#' Draw independent samples from the posterior
#' @details Helper function to sample posterior estimates (results saved from \code[estimate_pars]).
#' Loads an rds file identified through the parameters passed in the function.
#' @param pop_predict type of population scaling in the model
#' @param intercept type of intercept in the model
#' @param scale scale of the model (either commune or district level)
#' @param data_source data used to fit the model (either National or Moramanga data)
#' @param suff pass another suffix to identify the file
#' @param parent_dir where to look for the file
#' @param nsims number of samples to draw
#' @param hpd numeric 0 - 1, whether to draw from the higher posterior density and what level
#' @return a matrix of independent draws from the posterior distributions of the sourced mcmc chains
#' @section Dependencies: coda, glue
get_samps <- function(pop_predict = "flatPop", data_source = "National", intercept = "random",
                      scale = "Commune", suff = "", parent_dir = "analysis/out/mods/samps/",
                      nsims = 1000, hpd = NULL) {
  samps <- readRDS(glue("{parent_dir}{data_source}/{scale}_{intercept}_{pop_predict}{suff}.rds"))[[1]]
  samp_mat <- do.call(rbind, samps)

  if (!is.null(hpd)) {
    samp_mat <- apply(samp_mat, 2,
      function(x, prob, length) {
        hpd <- HPDinterval(as.mcmc(x), prob = prob)
        to_samp <- x[x >= hpd[, 1] & x <= hpd[, 2]]
        rep <- ifelse(length(to_samp) < length, TRUE, FALSE)
        return(sample(to_samp, size = length, replace = rep))
      },
      prob = hpd, length = nsims
    )
  } else {
    samp_mat <- apply(samp_mat, 2, sample, size = nsims) # independent draws from posterior w/out rep
  }
  return(samp_mat)
}

####################################################################################################
##' Prediction functions for getting bites, deaths, and vials 
##' Details: Wrapper function for getting bites, deaths, and vials from inputs 
##' Author: Malavika Rajeev
####################################################################################################

##' Predict deaths
##' ------------------------------------------------------------------------------------------------
predict.all <- function(ttimes, pop, catch, names, 
                        beta_ttimes, beta_0, beta_pop, sigma_0, known_alphas, 
                        pop_predict = "addPop", intercept = "random",
                        data_source = "national", scale = "Commune",
                        trans = 1e5, known_catch = TRUE,
                        p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                        max_HDR = 25, min_HDR = 5, 
                        dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                        prob_death = 0.16, nsims = 1000,
                        outputs = c("bites", "deaths", "vials"), 
                        scenario = 0, seed = 123) {
  
  # check <- comm_run[1:100, ]
  # # Testing
  # ttimes = check$weighted_times/60;
  # pop = check$pop; catch = check$base_catches;
  # names = check$commune_id;
  # beta_ttimes = model_means$beta_access[model_means$scale == "District"];
  # beta_0 = model_means$beta_0[model_means$scale == "District"]; beta_pop = 0;
  # sigma_0 = model_means$sigma_0[model_means$scale == "District"];
  # known_alphas = NA; pop_predict = "flatPop"; intercept = "random";
  # data_source = "National"; scale = "District";
  # trans = 1e5; known_catch = FALSE;
  # p_rab_min = 0.2; p_rab_max = 0.6; rho_max = 0.98;
  # max_HDR = 25; min_HDR = 5;
  # dog_rabies_inc = 0.01; human_exp_rate = 0.39;
  # prob_death = 0.16; nsims = 10; scenario = check$scenario;
  # outputs = c("bites", "deaths", "vials")
  # cl <- makeCluster(cores)
  # registerDoParallel(cl)
  # registerDoRNG(456) ## Reproducible within the loops?
  
  registerDoRNG(seed)
  
  bite_mat <- predict.bites(ttimes, pop, catch, names, beta_ttimes, beta_0, beta_pop, sigma_0, 
                            known_alphas, pop_predict, intercept, trans, known_catch, 
                            nsims)
  print("bites done")
  foreach(vals = iter(bite_mat, by = "row"), .combine = 'rbind',
          .export = c('get.boots.dt', 'boots_mean'), 
          .packages = c('boot', 'data.table', 'iterators')) %dopar% {
    ests <- get.boots.dt(as.vector(vals), names = c("bites_mean", "bites_upper", "bites_lower"))
  } -> summarized
  
  bites_dt <- data.table(names, summarized, data_source, scale, pop_predict, intercept,
                                          ttimes, pop, catch, scenario)
  print("bites summarized")
  
  if("deaths" %in% outputs) {
    deaths_dt <- predict.deaths(bite_mat, pop, names, p_rab_min, p_rab_max, rho_max,
                               max_HDR, min_HDR, dog_rabies_inc, human_exp_rate, 
                               prob_death)
    print("deaths done")
    
    deaths_dt <- data.table(names, deaths_dt, data_source, scale, pop_predict, intercept,
                            ttimes, pop, catch, scenario)
    print("deaths summarized")
    
  } else {
    deaths_dt <- NULL
  }
  
  if("vials" %in% outputs) {
    vials_dt <- sim.throughput(bite_mat, catch, scenario)
    print("vials done")
    
    vials_dt <- data.table(vials_dt, data_source, scale, pop_predict, intercept)
    print("vials summarized")
    
  } else {
    vials_dt <- NULL
  }
  
  return(list(bites_dt = bites_dt, deaths_dt = deaths_dt, vials_dt = vials_dt))
}

##' Function for getting bites from model inputs (return a matrix)
##' ------------------------------------------------------------------------------------------------
predict.bites <- function(ttimes, pop, catch, names,
                           beta_ttimes, beta_0, beta_pop, sigma_0, known_alphas, 
                           pop_predict = "addPop", intercept = "random",
                           trans = 1e5, known_catch = TRUE, nsims = 1000) {
  
  foreach(i = 1:nsims, .combine = cbind) %dopar% {
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
predict.deaths <- function(bite_mat, pop, names,
                           p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                           max_HDR = 25, min_HDR = 5, dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                           prob_death = 0.16) {
  
  all_mats <- foreach(bites = iter(bite_mat, by = "row"), .combine = multicomb,
                      .export = c('get.boots.dt', 'boots_mean'), 
                      .packages = c('boot', 'data.table', 'iterators')) %dopar% {
    ## Run preds
    hdr <- runif(length(bites), min = min_HDR, max = max_HDR)
    rabid_exps <- rpois(length(bites), human_exp_rate*dog_rabies_inc*pop/hdr)
    p_rabid <- runif(length(bites), min = p_rab_min, max = p_rab_max)
    reported_rabid <- rbinom(length(bites), bites, p_rabid)
    max_reported <- floor(rabid_exps*rho_max) ## so that reporting is not always one
    reported_rabid <- ifelse(reported_rabid > max_reported, max_reported, reported_rabid)
    deaths <- rbinom(length(bites), rabid_exps - reported_rabid, prob_death)
    averted <- rbinom(length(bites), reported_rabid, prob_death)
    
    check <- list(deaths = deaths, averted = averted, p_rabid = reported_rabid/as.vector(bites), 
         reporting = reported_rabid/rabid_exps)
    out <- lapply(check, get.boots.dt, names = c("mean", "upper", "lower"))
  }
  summary_admin <- do.call(cbind, all_mats)
  deaths_dt <- data.table(names, summary_admin)
  return(deaths_dt)
}

##' Function for getting vials from input bites
##' ------------------------------------------------------------------------------------------------
get.vials <- function(x) {
  day0 <- round(runif(x, min = 1, max = 365))
  days <- c(day0, day0 + 3, day0 + 7)
  vials <- sum(ceiling(table(days)/2))
  return(vials)
}

sim.throughput <- function(bite_mat, catch, scenario) {
  catch_dt <- data.table(bite_mat, catch, scenario)
  catch_dt <- catch_dt[, lapply(.SD, sum, na.rm = TRUE), by = c("catch", "scenario")]
  catches <- catch_dt$catch
  scenarios <- catch_dt$scenario
  catch_mat <- as.matrix(catch_dt[, c("catch", "scenario") := NULL])
  
  ## Simulate vials
  foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, 
          .export = c('get.boots.dt', 'boots_mean', 'get.vials'), 
          .packages = c('boot', 'data.table', 'iterators')) %dopar% {
      vials <- sapply(bycatch, get.vials)
      vial_ests <- get.boots.dt(as.vector(vials), names = c("vials_mean", 
                                                      "vials_upper", "vials_lower"))
      bite_ests <- get.boots.dt(as.vector(bycatch), names = c("bites_mean", 
                                                           "bites_upper", "bites_lower"))
      out <- cbind(vial_ests, bite_ests)
      
  } -> vials_dt

  vials_dt <- data.table(catch = catches, scenario = scenarios, vials_dt)
  return(vials_dt)
}


##' Helper Functions 
##' ------------------------------------------------------------------------------------------------
## Function for getting bootstrapped CIS
boots_mean <- function(data, indices) {mean(data[indices], na.rm = TRUE)}
boots_median <- function(data, indices) {median(data[indices], na.rm = TRUE)}

get.boots.dt <- function(vector, type_CI = "basic", names,
                         func = boots_mean, nsims = 1000) {
  samples <- boot(vector, func, R = nsims)
  ests <- boot.ci(samples, type = type_CI)
  out <- data.table(point_est = ests$t0, upper = ests[[4]][5], lower = ests[[4]][4])
  names(out) <- names
  return(out)
}

get.boots.list <- function(vector, type_CI = "basic", names,
                           func = boots_mean, nsims = 1000) {
  samples <- boot(vector, func, R = nsims)
  ests <- boot.ci(samples, type = type_CI)
  out <- list(ests$t0, ests[[4]][5], ests[[4]][4])
  names(out) <- names
  return(out)
}

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

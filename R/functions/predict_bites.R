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
                          prob_death = 0.16, nsims = 1000, scenario = 0, 
                          outputs = c("bites", "deaths", "vials")) {
  
  # district_df <- district_df[scenario < 2]
  # ## Testing
  # ttimes = district_df$weighted_times/60;
  # pop = district_df$pop; catch = district_df$catch_numeric;
  # names = district_df$district_id;
  # beta_ttimes = model_means$beta_access[model_means$scale == "District"];
  # beta_0 = model_means$beta_0[model_means$scale == "District"]; beta_pop = 0;
  # sigma_0 = model_means$sigma_0[model_means$scale == "District"];
  # known_alphas = NA; pop_predict = "flatPop"; intercept = "random";
  # data_source = "National"; scale = "District";
  # trans = 1e5; known_catch = FALSE;
  # p_rab_min = 0.2; p_rab_max = 0.6; rho_max = 0.98;
  # max_HDR = 25; min_HDR = 5;
  # dog_rabies_inc = 0.01; human_exp_rate = 0.39;
  # prob_death = 0.16; nsims = 1000; scenario = district_df$scenario;
  
  foreach(i = 1:nsims, .combine = cbind) %dopar% {
    if(intercept == "random") {
      ## draw catchment level effects
      alpha <- rnorm(max(catch, na.rm = TRUE), mean = beta_0, sd = sigma_0)
      alpha <- alpha[catch]
      
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
  
  foreach(vals = iter(bite_mat, by = "row"), .combine = 'rbind') %dopar% {
    ests <- get.boots.dt(as.vector(vals), names = c("bites_mean", "bites_upper", "bites_lower"))
  } -> summarized
  
  out_bites <- list(bites_dt = data.table(names, summarized, data_source, scale, pop_predict,
                                          ttimes, pop, catch, scenario))

  if("deaths" %in% outputs) {
    all_mats <- foreach(bites = iter(bite_mat, by = "col"), .combine = multicomb) %do% {
      ## Run preds
      hdr <- runif(length(bites), min = min_HDR, max = max_HDR)
      rabid_exps <- rpois(length(bites), human_exp_rate*dog_rabies_inc*pop/hdr)
      p_rabid <- runif(length(bites), min = p_rab_min, max = p_rab_max)
      p_rabid_t <- ifelse(bites == 0 & rabid_exps == 0, 0,
                          ifelse(bites*p_rabid/rabid_exps > rho_max, (rho_max*rabid_exps)/bites, p_rabid))
      reported_rabid <- round(bites*p_rabid_t)
      reported_rabid <- ifelse(reported_rabid > rabid_exps, rabid_exps, reported_rabid)
      deaths <- rbinom(length(bites), rabid_exps - reported_rabid, prob_death)
      averted <- rbinom(length(bites), reported_rabid, prob_death)
      
      list(deaths = deaths, averted = averted, p_rabid = p_rabid_t, reporting = reported_rabid/rabid_exps)
    }
    
    foreach(i = 1:length(all_mats), .combine = 'cbind') %dopar% {
      mat <- all_mats[[i]]
      labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
      foreach(vals = iter(mat, by = "row"), .combine = 'rbind') %dopar% {
        ests <- get.boots.dt(as.vector(vals), names = labels)
      } -> summarized
    } -> summary_admin
    
    admin_dt <- data.table(names,summary_admin, data_source, scale, pop_predict,
                           ttimes, pop, catch, scenario)
    
    natl_mats <- all_mats[c("deaths", "averted")]
    
    ## Get means
    foreach(i = 1:length(natl_mats)) %dopar% {
      natl <- data.table(natl_mats[[i]], scenario)
      labels <- paste0(names(natl_mats)[i], "_", c("mean", "mean_upper", "mean_lower"))
      natl <- natl[, lapply(.SD, sum, na.rm = TRUE), by = scenario]
      natl <- melt(natl, id = "scenario")
      natl<- natl[, get.boots.list(value, func = boots_mean, names = labels), by = scenario]
    } -> natl_means
    
    ## Get natl_medians
    foreach(i = 1:length(natl_mats)) %dopar% {
      natl <- data.table(natl_mats[[i]], scenario)
      labels <- paste0(names(natl_mats)[i], "_", c("median", "median_upper", "median_lower"))
      natl <- natl[, lapply(.SD, sum, na.rm = TRUE), by = scenario]
      natl <- melt(natl, id = "scenario")
      natl<- natl[, get.boots.list(value, func = boots_median, names = labels), by = scenario]
    } -> natl_medians
    
    natl_df <- Reduce(merge, c(natl_means, natl_medians))
    out_deaths <- list(admin_dt = admin_dt, natl_df = natl_df)
  } else {
    out_deaths <- NULL
  }
  
  if("vials" %in% outputs) {
    catch_dt <- data.table(bite_mat, scenario, catch)
    catch_dt <- catch_dt[, lapply(.SD, sum, na.rm = TRUE), by = c("scenario", "catch")]
    catches <- catch_dt$catch
    scenarios <- catch_dt$scenarios
    catch_mat <- as.matrix(catch_dt[, c("scenario", "catch") := NULL])
    
    foreach(bycatch = iter(catch_mat, by = "col"), .combine = cbind) %dopar% {
      sapply(bycatch, get.vials)
    } -> vial_mat
    
    labels <- paste0("vials", "_", c("mean", "upper", "lower"))
    
    foreach(vals = iter(vial_mat, by = "row"), .combine = 'rbind') %dopar% {
      ests <- get.boots.dt(as.vector(vals), names = c("vials_mean", 
                                                      "vials_upper", "vials_lower"))
    } -> vials_df
    
    ## Also get estimates of bites by catchment
    foreach(vals = iter(catch_mat, by = "row"), .combine = 'rbind') %dopar% {
      ests <- get.boots.dt(as.vector(vals), names = c("bites_mean", 
                                                     "bites_upper", "bites_lower"))
    } -> bites_df
    
    out_vials <- list(vials = data.table(catch = catches, scenario = scenarios, vials_df,
                                         bites_df))
  }
  
  return(c(out_bites, out_deaths, out_vials))
}

## Function for getting bites from model inputs (return a matrix)
predict.bites <- function(ttimes, pop, catch, names,
                           beta_ttimes, beta_0, beta_pop, sigma_0, known_alphas, 
                           pop_predict = "addPop", intercept = "random",
                           trans = 1e5, known_catch = TRUE, nsims = 1000, 
                           scenario = 0) {
  
  # district_df <- district_df[scenario < 2]
  # ## Testing
  # ttimes = district_df$weighted_times/60;
  # pop = district_df$pop; catch = district_df$catch_numeric;
  # names = district_df$district_id;
  # beta_ttimes = model_means$beta_access[model_means$scale == "District"]; 
  # beta_0 = model_means$beta_0[model_means$scale == "District"]; beta_pop = 0; 
  # sigma_0 = model_means$sigma_0[model_means$scale == "District"]; 
  # known_alphas = NA; pop_predict = "flatPop"; intercept = "random";
  # trans = 1e5; known_catch = FALSE; nsims = 1000; scenario = district_df$scenario;
  
  foreach(i = 1:nsims, .combine = cbind) %dopar% {
    if(intercept == "random") {
      ## draw catchment level effects
      alpha <- rnorm(max(catch, na.rm = TRUE), mean = beta_0, sd = sigma_0)
      alpha <- alpha[catch]
      
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
predict.deaths <- function(bite_mat, p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                           max_HDR = 25, min_HDR = 5, dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                           prob_death = 0.16) {
  
  # district_df <- district_df[scenario < 2]
  # ## Testing
  # p_rab_min = 0.2; p_rab_max = 0.6; rho_max = 0.98;
  # max_HDR = 25; min_HDR = 5; 
  # dog_rabies_inc = 0.01; human_exp_rate = 0.39; 
  # prob_death = 0.16;
  
  all_mats <- foreach(bites = iter(bite_mat, by = "col"), .combine = multicomb) %do% {
    ## Run preds
    hdr <- runif(length(bites), min = min_HDR, max = max_HDR)
    rabid_exps <- rpois(length(bites), human_exp_rate*dog_rabies_inc*pop/hdr)
    p_rabid <- runif(length(bites), min = p_rab_min, max = p_rab_max)
    p_rabid_t <- ifelse(bites == 0 & rabid_exps == 0, 0,
                        ifelse(bites*p_rabid/rabid_exps > rho_max, (rho_max*rabid_exps)/bites, p_rabid))
    reported_rabid <- round(bites*p_rabid_t)
    reported_rabid <- ifelse(reported_rabid > rabid_exps, rabid_exps, reported_rabid)
    deaths <- rbinom(length(bites), rabid_exps - reported_rabid, prob_death)
    averted <- rbinom(length(bites), reported_rabid, prob_death)
    
    list(deaths = deaths, averted = averted, p_rabid = p_rabid_t, reporting = reported_rabid/rabid_exps)
  }
  
  foreach(i = 1:length(all_mats), .combine = 'cbind') %dopar% {
    mat <- all_mats[[i]]
    labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
    foreach(vals = iter(mat, by = "row"), .combine = 'rbind') %dopar% {
      ests <- get.boots.dt(as.vector(vals), names = labels)
    } -> summarized
  } -> summary_admin
  
  admin_dt <- data.table(names,summary_admin, data_source, scale, pop_predict,
                         ttimes, pop, catch, scenario)
  
  natl_mats <- all_mats[c("deaths", "averted")]
  
  ## Get means
  foreach(i = 1:length(natl_mats)) %dopar% {
    natl <- data.table(natl_mats[[i]], scenario)
    labels <- paste0(names(natl_mats)[i], "_", c("mean", "mean_upper", "mean_lower"))
    natl <- natl[, lapply(.SD, sum, na.rm = TRUE), by = scenario]
    natl <- melt(natl, id = "scenario")
    natl<- natl[, get.boots.list(value, func = boots_mean, names = labels), by = scenario]
  } -> natl_means
  
  ## Get natl_medians
  foreach(i = 1:length(natl_mats)) %dopar% {
    natl <- data.table(natl_mats[[i]], scenario)
    labels <- paste0(names(natl_mats)[i], "_", c("median", "median_upper", "median_lower"))
    natl <- natl[, lapply(.SD, sum, na.rm = TRUE), by = scenario]
    natl <- melt(natl, id = "scenario")
    natl<- natl[, get.boots.list(value, func = boots_median, names = labels), by = scenario]
  } -> natl_medians
  
  natl_df <- Reduce(merge, c(natl_means, natl_medians))
  out_deaths <- list(admin_dt = admin_dt, natl_df = natl_df)
  
  return(out_deaths)
}

##' Function for getting vials from input bites
##' ------------------------------------------------------------------------------------------------
get.vials <- function(x) {
  day0 <- round(runif(x, min = 1, max = 365))
  days <- c(day0, day0 + 3, day0 + 7)
  vials <- sum(ceiling(table(days)/2))
  return(vials)
}

sim.throughput <- function(bite_mat, scenario, catch) {
  catch_dt <- data.table(bite_mat, scenario, catch)
  catch_dt <- catch_dt[, lapply(.SD, sum, na.rm = TRUE), by = c("scenario", "catch")]
  catches <- catch_dt$catch
  scenarios <- catch_dt$scenarios
  catch_mat <- as.matrix(catch_dt[, .("scenario", "catch") := NULL])
  
  foreach(vals = iter(vial_mat, by = "row"), .combine = 'rbind') %dopar% {
    ests <- get.boots.dt(as.vector(vals), names = c("vials_mean", 
                                                    "vials_upper", "vials_lower"))
  } -> vials_dt
  
  ## Also get estimates of bites by catchment
  foreach(vals = iter(catch_mat, by = "row"), .combine = 'rbind') %dopar% {
    ests <- get.boots.dt(as.vector(vals), names = c("bites_mean", 
                                                    "bites_upper", "bites_lower"))
  } -> bites_dt
  
  out_vials <- list(vials = data.table(catch = catches, scenario = scenarios, vials_dt,
                                       bites_dt))
  return(out_vials)
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
  mapply(cbind, x, ..., SIMPLIFY = FALSE)
}

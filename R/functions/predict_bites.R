####################################################################################################
##' Prediction functions for getting bites, deaths, and vials 
##' Details: Wrapper function for getting bites, deaths, and vials from inputs 
##' Author: Malavika Rajeev
####################################################################################################

##' Predict deaths
##' ------------------------------------------------------------------------------------------------
predict.deaths <- function(ttimes, pop, catch, names, group_name,
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
  # group_name = district_df$district_id;
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
  
  multicomb <- function(x, ...) {
    mapply(cbind, x, ..., SIMPLIFY = FALSE)
  }
  
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
  } -> exp_bites_mat
  
  if(length(output) == 1 & output == "bites") {
    foreach(vals = iter(exp_bites_mat, by = "row"), .combine = 'rbind') %dopar% {
      ests <- get.boots.dt(as.vector(vals), names = c("exp_bites_mean", "exp_bites_upper", "exp_bites_lower"))
    } -> summarized
    
    exp_bites_df <- data.table(names, group_name, summarized, data_source, scale, pop_predict,
                           ttimes, pop, catch, scenario)
    out_bites <- list(exp_bites_df = exp_bites_df)
  } else {
    out_bites <- NULL
  }
  
  if("deaths" %in% output) {
    all_mats <- foreach(exp_bites = iter(exp_bites_mat, by = "col"), .combine = multicomb) %do% {
      ## Run preds
      bites <- rpois(length(exp_bites), exp_bites)
      hdr <- runif(length(bites), min = min_HDR, max = max_HDR)
      rabid_exps <- rpois(length(bites), human_exp_rate*dog_rabies_inc*pop/hdr)
      p_rabid <- runif(length(bites), min = p_rab_min, max = p_rab_max)
      p_rabid_t <- ifelse(bites == 0 & rabid_exps == 0, 0,
                          ifelse(bites*p_rabid/rabid_exps > rho_max, (rho_max*rabid_exps)/bites, p_rabid))
      reported_rabid <- round(bites*p_rabid_t)
      reported_rabid <- ifelse(reported_rabid > rabid_exps, rabid_exps, reported_rabid)
      deaths <- rbinom(length(bites), rabid_exps - reported_rabid, prob_death)
      averted <- rbinom(length(bites), reported_rabid, prob_death)
      
      list(bites = bites, deaths = deaths, averted = averted, p_rabid = p_rabid_t, reporting = reported_rabid/rabid_exps)
      # bite_mat[, i] <- bites
      # death_mat[, i] <- deaths
      # averted_mat[, i] <- averted
      # p_rabid_mat[, i] <- p_rabid_t
      # reporting_mat[, i] <- reported_rabid/rabid_exps
    }
    
    foreach(i = 1:length(all_mats), .combine = 'cbind') %dopar% {
      mat <- all_mats[[i]]
      labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
      foreach(vals = iter(mat, by = "row"), .combine = 'rbind') %dopar% {
        ests <- get.boots.dt(as.vector(vals), names = labels)
      } -> summarized
    } -> summary_admin
    
    admin_df <- data.table(names, group_name, summary_admin, data_source, scale, pop_predict,
                           ttimes, pop, catch, scenario)
    
    natl_mats <- all_mats[c("bites", "deaths", "averted")]
    
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
    out_deaths <- list(admin_df = admin_df, natl_df = natl_df)
  } else {
    out_deaths <- NULL
  }
  
  if("vials" %in% output) {
  
  }
  return(c(out_bites, out_deaths, out_vials))
}

## Function for getting bites from model inputs

## Function for getting deaths from input bites

##' Function for getting vials from input bites 
##' ------------------------------------------------------------------------------------------------

##' Helper Functions 
##' ------------------------------------------------------------------------------------------------
## Function for getting bootstrapped CIS
boots_mean <- function(data, indices) {mean(data[indices], na.rm = TRUE)}
boots_median <- function(data, indices) {median(data[indices], na.rm = TRUE)}

get.boots.dt <- function(vector, type_CI = "basic", names = names,
                         func = boots_mean, nsims = 1000) {
  samples <- boot(vector, func, R = nsims)
  ests <- boot.ci(samples, type = type_CI)
  return(data.table(point_est = ests$t0, upper = ests[[4]][5], lower = ests[[4]][4]))
}

get.boots.list <- function(vector, type_CI = "basic", names,
                           func = boots_mean, nsims = 1000) {
  samples <- boot(vector, func, R = nsims)
  ests <- boot.ci(samples, type = type_CI)
  out <- list(ests$t0, ests[[4]][5], ests[[4]][4])
  names(out) <- names
  return(out)
}

## Bite Models
## Malavika Rajeev
## Mar 2019

## Modeling reported bites given covariates ----------------------------------------------------
## 1. Use to estimate (run = "optim")
## 2. Use to predict from data (run = "predict.data")
## 3. Pass pop and other covars to get predictions for given range (run = "predict.fixed")

model.bites <- function(bites, names_bites, covar, pop, names_covar, sum = TRUE, 
                        pop_predict = "addPop", covar_name = "ttimes_weighted",
                        beta = 1e-6, beta_pop = 1e-6, intercept = 0.1, trans = 1e5, run = "optim",...) {
  ## Testing
  ## Need a dataframe with all covars + pop + names_covar
  # beta_pop = 1e-5;
  # trans = 1e5;
  # beta = 1e-5; intercept = 0.1;
  
  if(pop_predict == "onlyPop") {
    covar <- pop
    exp_bites <- exp(beta*covar/trans + intercept)
  }
  
  if(pop_predict == "flatPop") {
    exp_bites <- exp(beta*covar + intercept)*pop
  }
  
  if(pop_predict == "addPop") {
    exp_bites <- exp(beta_pop*pop/trans + beta*covar + intercept)
  }
  
  check_bites <- exp_bites
  
  if (run == "predict.fixed") {
    return(as.data.frame(list(covar = covar, predicted = exp_bites, pop = pop, sum = sum, 
                              pop_predict = pop_predict, covar_name = covar_name)))
  } else {
    if(sum == TRUE) {
      df <- as.data.frame(list(exp_bites = exp_bites, 
                               names = names_covar))
      df %>%
        group_by(names) %>%
        summarize(exp_bites = sum(exp_bites, na.rm = TRUE)) %>%
        right_join(as.data.frame(list(bites = bites, 
                                      names = names_bites)), by = c("names" = "names")) -> sum_bites
      exp_bites <- sum_bites$exp_bites
      bites <- sum_bites$bites
    }
    if(run == "optim") {
      return(-sum(dpois(round(bites), lambda = exp_bites, log = TRUE)))
    }
    if (run == "predict.data") {
      return(as.data.frame(list(observed = bites, predicted = exp_bites, sum = sum,
                                pop_predict = pop_predict, covar_name = covar_name)))
    }
    if (run == "predict.raw") {
      return(as.data.frame(list(names_covar = names_covar, covar = covar, predicted = check_bites, sum = sum, pop = pop,
                                pop_predict = pop_predict, covar_name = covar_name)))
    }
  }
}

## Running sets of models using foreach --------------------------------------------------------
run.mods <- function(bites = exps_dist$bites, names_bites = exps_dist$district,
                     pop_predict = c("addPop", "onlyPop", "none"),
                     pop_vars = list(pop_dist = exps_dist$pop, pop_commune = comm_covars$pop),
                     covars = list(distance_district = exps_dist$distance, distance_commune = comm_covars$distance, 
                                   ttimes_district = exps_dist$ttimes_weighted_dist, 
                                   ttimes_commune = comm_covars$ttimes_weighted),
                     sum_it = c(FALSE, TRUE, FALSE, TRUE), run_type = "optim",
                     label = "Mada") {

  mods <- 
    foreach(i = 1:length(pop_predict), .combine = c) %:%
    foreach(k = 1:length(covars), .combine = c) %do% {
      data_est <- list(bites = bites, run = run_type, covar = covars[[k]],
                       names_bites = names_bites, sum = sum_it[k],
                       trans = 1e5, pop_predict = pop_predict[i])
      start_params <- list(beta = 1e-5, intercept = 0.1)
      
      if(pop_predict[i] == "addPop") {
        ## need beta_pop
        start_params <- c(start_params, list(beta_pop = 1e-5))
      }
      
      if(sum_it[k] == TRUE) {
        data_est <- c(data_est, list(names_covar = comm_covars$mdg_dis_co))
      }
      
      if(grepl("commune", names(covars)[k], fixed = TRUE)) {
        covar_name <- ifelse(pop_predict[i] == "onlyPop", "pop_commune", names(covars)[k])
        data_est <- c(data_est, list(pop = pop_vars$pop_commune, covar_name = covar_name))
      } else {
        covar_name <- ifelse(pop_predict[i] == "onlyPop", "pop_district", names(covars)[k])
        data_est <- c(data_est, list(pop = pop_vars$pop_dist, covar_name = covar_name))
      }
      
      mods <- mle2(model.bites, start = start_params, data = data_est)
      
      attributes(mods)$label <- paste(label, pop_predict[i], covar_name)
      attributes(mods)$nobs <- length(bites)
      mods
    }
  return(mods)
}


# Getting dataframe from model list -----------------------------------------------------------
# 
# mods_covars <- Filter(function(x) !grepl("addPop", attributes(x)$label, fixed = TRUE), mods_all)
# prof_CI_covars <- lapply(mods_covars, confint)
# 
# mods_pop <- Filter(function(x) grepl("addPop", attributes(x)$label, fixed = TRUE), mods_all)
# prof_CI_pop <- lapply(mods_pop, confint)
# 
# bite_mods_covar_df <- as.data.frame(list(beta = mapply(function(x) coef(x)["beta"], mods_covars),
#                                          intercept = mapply(function(x) coef(x)["intercept"], mods_covars),
#                                          beta_upper = mapply(function(x) x["beta", 2], prof_CI_covars),
#                                          beta_lower = mapply(function(x) x["beta", 1], prof_CI_covars),
#                                          intercept_lower = mapply(function(x) x["intercept", 1], prof_CI_covars),
#                                          intercept_upper = mapply(function(x) x["intercept", 2], prof_CI_covars),
#                                          likelihood = mapply(function(x) logLik(x), mods_covars),
#                                          model = mapply(function(x) attributes(x)$label, mods_covars)))
# 
# bite_mods_pop_df <- as.data.frame(list(beta = mapply(function(x) coef(x)["beta"], mods_pop),
#                                        beta_pop = mapply(function(x) coef(x)["beta_pop"], mods_pop),
#                                        intercept = mapply(function(x) coef(x)["intercept"], mods_pop),
#                                        beta_upper = mapply(function(x) x["beta", 2], prof_CI_pop),
#                                        beta_lower = mapply(function(x) x["beta", 1], prof_CI_pop),
#                                        beta_pop_upper = mapply(function(x) x["beta_pop", 2], prof_CI_pop),
#                                        beta_pop_lower = mapply(function(x) x["beta_pop", 1], prof_CI_pop),
#                                        intercept_lower = mapply(function(x) x["intercept", 1], prof_CI_pop),
#                                        intercept_upper = mapply(function(x) x["intercept", 2], prof_CI_pop),
#                                        likelihood = mapply(function(x) logLik(x), mods_pop),
#                                        model = mapply(function(x) attributes(x)$label, mods_pop)))

## Running sets of predictions using foreach --------------------------------------------------------
run.predicts.fixed <- function(covar_df, pop_fixed = 1e5, pop_seq = pop_plot) {
  preds_data <- foreach(i = iter(covar_df, by = "row"), .combine = "rbind") %do% {
    mod_attrs <- str_split(i$model, " ")[[1]]
    mod_pop <- mod_attrs[2]
    covar_name <- mod_attrs[3]
    scale <- ifelse(mod_attrs[1] == "Mora", "Moramanga", 
                    ifelse(grepl("district", covar_name, fixed = TRUE), "District", 
                           "Commune"))
    covar_name <- str_split(covar_name, "_")[[1]][1]
    names_covar = comm_covars$mdg_dis_co
    
    if(mod_pop == "addPop"){
      beta_pop <- i$beta_pop
      beta_pop_lower <- i$beta_pop_lower
      beta_pop_upper <- i$beta_pop_upper
    } else {
      beta_pop = 1e5
    }
    
    if(mod_pop == "onlyPop") {
      pop = pop_seq
    } else {
      pop = pop_fixed
    }
    
    if(scale == "District" | scale == "Commune") {
      bites = exps_dist$bites
      names_bites = exps_dist$district
      if(scale == "Commune") {
        sum_it = TRUE
      } else {
        sum_it = FALSE
      }
    } else {
      bites = exps_mora$bites
      names_bites = exps_mora$commune
      sum_it = FALSE
    }
    
    if(grepl("distance", covar_name, fixed = TRUE)) {
      covar = distance_plot
    } else {
      covar = ttimes_plot
    }
    
    ests <- model.bites (bites, names_bites, covar, pop = pop, names_covar, sum = sum_it, 
                         pop_predict = mod_pop, covar_name,
                         beta = i$beta, beta_pop = beta_pop, 
                         intercept = i$intercept, trans = 1e5, run = "predict.fixed")
    
    ests$lower <- model.bites (bites, names_bites, covar, pop = pop, names_covar, sum = sum_it, 
                               pop_predict = mod_pop, covar_name,
                               beta = i$beta_lower, beta_pop = beta_pop_lower, 
                               intercept = i$intercept_lower, trans = 1e5, run = "predict.fixed")$predicted
    
    ests$upper <- model.bites (bites, names_bites, covar, pop = pop, names_covar, sum = sum_it, 
                               pop_predict = mod_pop, covar_name,
                               beta = i$beta_upper, beta_pop = beta_pop_upper, 
                               intercept = i$intercept_upper, trans = 1e5, run = "predict.fixed")$predicted
    ests$scale <- scale
    ests
  }
  return(preds_data)
}

## Running sets of predictions from data --------------------------------------------------------
run.predicts.data<- function(covar_df, run_type = "predict.raw") {
  ## testing
  preds_data <- foreach(i = iter(covar_df, by = "row"), .combine = "rbind") %do% {
    mod_attrs <- str_split(i$model, " ")[[1]]
    mod_pop <- mod_attrs[2]
    covar_name <- mod_attrs[3]
    loc <- mod_attrs[1]
    scale <- ifelse(mod_attrs[1] == "Mora", "Moramanga", 
                    ifelse(grepl("district", covar_name, fixed = TRUE), "District", 
                           "Commune"))
    covar_name <- str_split(covar_name, "_")[[1]][1]
    
    if(mod_pop == "addPop"){
      beta_pop_est <- i$beta_pop
      beta_pop_lower <- i$beta_pop_lower
      beta_pop_upper <- i$beta_pop_upper
    } else {
      beta_pop_est <- beta_pop_lower <- beta_pop_lower <- 0
    }
    
    if(loc == "Mada") {
      bites = exps_dist$bites
      names_bites = exps_dist$district
      if(scale == "Commune") {
        names_covar = comm_covars$mdg_dis_co
        sum_it = TRUE
        pop_data = comm_covars$pop
        if(grepl("distance", covar_name, fixed = TRUE)) {
          covar = comm_covars$distance
        } else {
          covar = comm_covars$ttimes_weighted/60
        }
      } else {
        sum_it = FALSE
        pop_data = exps_dist$pop
        names_covar = exps_dist$district
        if(grepl("distance", covar_name, fixed = TRUE)) {
          covar = exps_dist$distance
        } else {
          covar = exps_dist$ttimes_weighted_dist/60
        }
      }
    } else {
      bites = exps_mora$bites
      names_bites = exps_mora$mdg_cm_
      names_covar = exps_mora$mdg_cm_
      pop_data = exps_mora$pop
      sum_it = FALSE
      if(grepl("distance", covar_name, fixed = TRUE)) {
        covar = exps_mora$distance
      } else {
        covar = exps_mora$ttimes_weighted/60
      }
    }
    
    ests <- model.bites (bites, names_bites, covar, pop = pop_data, names_covar, sum = sum_it, 
                        pop_predict = mod_pop, covar_name,
                        beta = i$beta, beta_pop = beta_pop_est, 
                        intercept = i$intercept, trans = 1e5, run = run_type)
    
    ests$lower <- model.bites (bites, names_bites, covar, pop = pop_data, names_covar, sum = sum_it, 
                             pop_predict = mod_pop, covar_name,
                             beta = i$beta_lower, beta_pop = beta_pop_lower, 
                             intercept = i$intercept_lower, trans = 1e5, run = run_type)$predicted
    
    ests$upper <- model.bites (bites, names_bites, covar, pop = pop_data, names_covar, sum = sum_it, 
                               pop_predict = mod_pop, covar_name,
                               beta = i$beta_upper, beta_pop = beta_pop_upper, 
                               intercept = i$intercept_upper, trans = 1e5, run = run_type)$predicted
    ests$scale <- scale
    ests
  }
  return(preds_data)
}

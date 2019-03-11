## Bite Models
## Malavika Rajeev
## Mar 2019

## Modeling reported bites given covariates ----------------------------------------------------
## 1. Use to estimate (run = "optim")
## 2. Use to predict from data (run = "predict.data")
## 3. Pass pop and other covars to get predictions for given range (run = "predict.fixed")

model.bites <- function(bites, names_bites, covar, pop, names_covar, sum = TRUE, 
                        pop_predict = "addPop", covar_name = "ttimes_weighted",
                        beta = 1e-5, beta_pop = 1e-5, intercept = 0.1, trans, run = "optim",...) {
  ## Testing
  ## Need a dataframe with all covars + pop + names_covar
  # beta_pop = 1e-5;
  # trans = 1e5;
  # beta = 1e-5; intercept = 0.1;
  
  if(pop_predict == "onlyPop") {
    exp_bites <- exp(beta*pop/trans + intercept)
  }
  
  if(pop_predict == "none") {
    exp_bites <- inv.logit(beta*covar + intercept)*pop
  }
  
  if(pop_predict == "addPop") {
    exp_bites <- exp(beta_pop*pop/trans + beta*covar + intercept)
  }
  
  if (run == "predict.fixed") {
    return(as.data.frame(list(covar = covar, predicted = exp_bites, pop = pop, sum = sum, 
                              pop = pop_predict, covar_name = covar_name)))
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
                                pop = pop_predict, covar_name = covar_name)))
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
                       trans = 1e5)
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

## Running sets of predictions using foreach --------------------------------------------------------

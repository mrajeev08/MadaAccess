## Bite Models
## Malavika Rajeev
## Mar 2019

## Modeling reported bites given covariates ----------------------------------------------------
## 1. Use to estimate (run = "optim")
## 2. Use to predict from data (run = "predict.data")
## 3. Pass pop and other covars to get predictions for given range (run = "predict.fixed")

model.bites <- function(bites, ctar_in, names_bites, covar, pop, names_covar, sum = TRUE, 
                        pop_predict = "addPop", covar_name = "ttimes_weighted",
                        beta = 1e-6, beta_pop = 1e-6, intercept = 0.1, trans = 1e5, 
                        run = "optim", beta_ctar = 1e-6, ctar_bump = FALSE, ...) {
  ## Testing
  ## Need a dataframe with all covars + pop + names_covar
  # beta_pop = 1e-5;
  # trans = 1e5;
  # beta = 1e-5; intercept = 0.1;
  
  if(pop_predict == "onlyPop") {
    covar <- pop
    if(ctar_bump == FALSE) {
      exp_bites <- exp(beta*covar/trans + intercept)
    } else {
      exp_bites <- exp(beta*covar/trans + beta_ctar*ctar_in + intercept)
    }
  }
  
  if(pop_predict == "flatPop") {
    if(ctar_bump == FALSE) {
      exp_bites <- exp(beta*covar + intercept)*pop
    } else {
      exp_bites <- exp(beta*covar + beta_ctar*ctar_in + intercept)*pop
    }
  }
  
  if(pop_predict == "addPop") {
    if(ctar_bump == FALSE) {
      exp_bites <- exp(beta_pop*pop/trans + beta*covar + intercept)
    } else {
      exp_bites <- exp(beta_pop*pop/trans + beta*covar + beta_ctar*ctar_in + intercept)
    }
  }
  
  check_bites <- exp_bites
  
  if (run == "predict.fixed") {
    return(as.data.frame(list(covar = covar, predicted = exp_bites, pop = pop, ctar_in = ctar_in,
                              sum = sum, 
                              pop_predict = pop_predict, covar_name = covar_name,
                              ctar_bump = ctar_bump)))
  } else {
    if(sum == TRUE) {
      df <- as.data.frame(list(exp_bites = exp_bites, 
                               names = names_covar))
      df %>%
        group_by(names) %>%
        summarize(exp_bites = sum(exp_bites, na.rm = TRUE)) %>%
        right_join(as.data.frame(list(bites = bites, 
                                      names = names_bites)), 
                   by = c("names" = "names")) -> sum_bites
      exp_bites <- sum_bites$exp_bites
      bites <- sum_bites$bites
    }
    if(run == "optim") {
      return(-sum(dpois(round(bites), lambda = exp_bites, log = TRUE)))
    }
    if (run == "predict.data") {
      return(as.data.frame(list(observed = bites, predicted = exp_bites, sum = sum,
                                pop_predict = pop_predict, covar_name = covar_name,
                                ctar_bump = ctar_bump)))
    }
    if (run == "predict.raw") {
      return(as.data.frame(list(names_covar = names_covar, covar = covar, predicted = check_bites, 
                                sum = sum, pop = pop, ctar_in = ctar_in, ctar_bump = ctar_bump,
                                pop_predict = pop_predict, covar_name = covar_name)))
    }
  }
}

## Run predictions with inputs
predict.bites <- function(names_bites, ctar_in, covar, pop, names_covar, sum = TRUE, 
                          pop_predict = "addPop", covar_name = "ttimes_weighted", ctar_bump = FALSE,
                          beta = 1e-6, beta_pop = 1e-6, beta_ctar = 1e-6, intercept = 0.1, trans = 1e5) {
  ## Testing
  ## Need a dataframe with all covars + pop + names_covar
  # beta_pop = 1e-5;
  # trans = 1e5;
  # beta = 1e-5; intercept = 0.1;
  if(pop_predict == "onlyPop") {
    covar <- pop
    covar_name = "pop"

    if(ctar_bump == FALSE) {
      exp_bites <- exp(beta*covar/trans + intercept)
    } else {
      exp_bites <- exp(beta*covar/trans + beta_ctar*ctar_in + intercept)
    }
  }
  
  if(pop_predict == "flatPop") {
    if(ctar_bump == FALSE) {
      exp_bites <- exp(beta*covar + intercept)*pop
    } else {
      exp_bites <- exp(beta*covar + beta_ctar*ctar_in + intercept)*pop
    }
  }
  
  if(pop_predict == "addPop") {
    if(ctar_bump == FALSE) {
      exp_bites <- exp(beta_pop*pop/trans + beta*covar + intercept)
    } else {
      exp_bites <- exp(beta_pop*pop/trans + beta*covar + beta_ctar*ctar_in + intercept)
    }
  }

  return(as.data.frame(list(names_bites = names_bites, 
                            names_covar = names_covar, covar = covar, predicted = exp_bites, 
                            sum = sum, pop = pop, pop_predict = pop_predict, ctar_bump = ctar_bump,
                            covar_name = covar_name)))
}

## Bite Models
## Malavika Rajeev
## Mar 2019

## Modeling reported bites given covariates ----------------------------------------------------
## 1. Use to estimate (run = "optim")
## 2. Use to predict from data (run = "predict.data")
## 3. Pass pop and other covars to get predictions for given range (run = "predict.fixed")

model.bites <- function(bites, names_bites, covar, pop, names_covar, sum = TRUE, 
                        pop_predict = "addPop", covar_name = "ttimes_weighted",
                        beta, beta_pop, intercept, trans, run = "optim",...) {
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
                                      names = names_bites))) -> sum_bites
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

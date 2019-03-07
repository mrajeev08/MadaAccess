######### Model Likelihood and Predictions ##################

## Likelihood of data --------------------------------------------------
get.likelihood.bites <- function(bites, names_bites, covar, pop,
                                 pop_sub, covar_sub, 
                                 names_pop, names_covar, sum = TRUE, pop_predict = TRUE, 
                                 beta, intercept, trans, ...) {
  ## Testing
  # covar = exps_dist$ttimes_weighted_dist/60;
  # bites = exps_dist$bites; 
  # names_bites = exps_dist$district;
  # pop = exps_dist$pop;
  # pop_sub = comm_data$pop; 
  # names_pop = comm_data$mdg_dis_co; 
  # names_covar = comm_data$mdg_dis_co; trans = 1e5;
  # beta = 1e-5; intercept = 0.1;
  
  if(pop_predict == TRUE) {
    if(sum == TRUE) {
      df <- as.data.frame(list(exp_bites = exp(beta*pop_sub/trans + intercept), 
                               names = names_pop))
      df %>%
        group_by(names) %>%
        summarize(exp_bites = sum(exp_bites, na.rm = TRUE)) %>%
        right_join(as.data.frame(list(bites = bites, 
                                     names = names_bites))) -> sum_bites
      exp_bites <- sum_bites$exp_bites
      bites <- sum_bites$bites
    } else {
      exp_bites <- exp(beta*pop/trans + intercept)
    }
  }
  if(pop_predict == FALSE) {
    if(sum == TRUE) {
      df <- as.data.frame(list(exp_bites = inv.logit(beta*covar_sub + intercept)*pop_sub, 
                               names = names_covar))
      df %>%
        group_by(names) %>%
        summarize(exp_bites = sum(exp_bites, na.rm = TRUE)) %>%
        right_join(as.data.frame(list(bites = bites, 
                                     names = names_bites))) -> sum_bites
      exp_bites <- sum_bites$exp_bites
      bites <- sum_bites$bites
    } else {
      exp_bites <- inv.logit(beta*covar + intercept)*pop
    }
  }
  return(-sum(dpois(round(bites), lambda = exp_bites, log = TRUE)))
}

## Expected bites from data --------------------------------------------------
predict.bites  <- function(bites, names_bites, covar, pop,
                           pop_sub, covar_sub, 
                           names_pop, names_covar, sum = TRUE, pop_predict = TRUE, 
                           beta, intercept, trans, ...) {
  if(pop_predict == TRUE) {
    if(sum == TRUE) {
      df <- as.data.frame(list(exp_bites = exp(beta*pop_sub/trans + intercept), 
                               names = names_pop))
      df %>%
        group_by(names) %>%
        summarize(exp_bites = sum(exp_bites, na.rm = TRUE)) %>%
        left_join(as.data.frame(list(bites = bites, 
                                     names = names_bites))) -> sum_bites
      exp_bites <- sum_bites$exp_bites
      bites <- sum_bites$bites
    } else {
      exp_bites <- exp(beta*pop/trans + intercept)
    }
  }
  if(pop_predict == FALSE) {
    if(sum == TRUE) {
      df <- as.data.frame(list(exp_bites = inv.logit(beta*covar_sub + intercept)*pop_sub, 
                               names = names_covar))
      df %>%
        group_by(names) %>%
        summarize(exp_bites = sum(exp_bites, na.rm = TRUE)) %>%
        left_join(as.data.frame(list(bites = bites, 
                                     names = names_bites))) -> sum_bites
      exp_bites <- sum_bites$exp_bites
      bites <- sum_bites$bites
    } else {
      exp_bites <- inv.logit(beta*covar + intercept)*pop
    }
  }
  return(as.data.frame(list(observed = bites, predicted = exp_bites, sum = sum, pop = pop_predict)))
}

## Expected bites given range of travel times --------------------------------------------------
expected_bites  <- function(covar, pop, pop_base, 
                            sum = TRUE, pop_predict = TRUE, 
                            beta, intercept, trans, ...) {
  if(pop_predict == TRUE) {
    exp_bites <- exp(beta*pop/trans + intercept) 
  }
  if(pop_predict == FALSE) {
    exp_bites <- inv.logit(beta*covar + intercept)*pop_base
  }
  return(exp_bites)
}

predict.bites <- function(access, ctar_in, pop, catch, names, group_name,
                          beta_access, beta_ctar, beta_0, beta_pop, sigma_0, known_alphas, 
                          covar_name = "ttimes", pop_predict = "addPop", intercept = "random",
                          summed = TRUE, ctar_bump = FALSE, data_source = "national", scale = "Commune",
                          trans = 1e5, nsims = 1000, known_catch = TRUE) {

  ## testing
  # access = covar_df$covar; ctar_in = covar_df$ctar_in_district;
  #  pop = covar_df$pop; catch = covar_df$catch; names = covar_df$names;
  #  group_name = covar_df$group_name; beta_access = i$beta_access;
  #  beta_ctar = i$beta_ctar; beta_0 = i$beta_0; beta_pop = i$beta_pop;
  #  sigma_0 = i$sigma_0;
  #  known_alphas = known_alphas;
  #  covar_name = i$covar_name; pop_predict = i$pop_predict; intercept = i$intercept;
  #  summed = i$summed; ctar_bump = i$ctar_bump; data_source = i$data_source;
  #  scale = i$scale;
  #  trans = 1e5; nsims = 1000; known_catch = TRUE;
  
  bite_mat <- matrix(NA, nrow = length(access), ncol = nsims)
  
  for(i in 1:nsims) {
    
    if(intercept == "random") {
      ## draw catchment level effects
      alpha <- rnorm(max(catch), mean = beta_0, sd = sigma_0)
      alpha <- alpha[catch]
      
      ## get alpha so that if known_catch is TRUE it pulls from estimates
      ## known alphas have to be the same length as the data (potentially change this part!)
      if(known_catch == TRUE) {
        alpha <- ifelse(is.na(known_alphas) | known_alphas == 0, alpha, known_alphas)
      } 
      
      if (pop_predict == "flatPop") {
        exp_bites <- exp(alpha + beta_access*access + beta_ctar*ctar_in)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(alpha + beta_access*access + beta_ctar*ctar_in + beta_pop*pop/trans) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(alpha + beta_pop*pop/trans + beta_ctar*ctar_in)
      }
    }
    
    if(intercept == "fixed") {
      if (pop_predict == "flatPop") {
        exp_bites <- exp(beta_0 + beta_access*access + beta_ctar*ctar_in)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(beta_0 + beta_access*access + beta_ctar*ctar_in + beta_pop*pop/trans) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(beta_0 + beta_pop*pop/trans + beta_ctar*ctar_in)
      }    
    }
    
    bite_mat[, i] <- rpois(length(exp_bites), exp_bites)
    
  }
  
  bites_mean <- rowMeans(bite_mat, na.rm = TRUE)
  bites_sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
  bites_05quant <- apply(bite_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
  bites_95quant <- apply(bite_mat, 1, quantile, prob = 0.95, na.rm = TRUE)
  
  return(data.frame(names, group_name, bites_mean, bites_sd, bites_05quant, bites_95quant,
                    covar_name, data_source, scale, pop_predict, ctar_bump, summed, intercept, 
                    access, ctar_in, pop, catch))

}

##' Predict deaths
##' ------------------------------------------------------------------------------------------------
##' TO DO: save random effect?
predict.deaths <- function(access, ctar_in, pop, catch, names, group_name,
                          beta_access, beta_ctar, beta_0, beta_pop, sigma_0, known_alphas, 
                          covar_name = "ttimes", pop_predict = "addPop", intercept = "random",
                          summed = TRUE, ctar_bump = FALSE, data_source = "national", scale = "Commune",
                          trans = 1e5, known_catch = TRUE,
                          p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                          max_HDR = 25, min_HDR = 5, 
                          dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                          prob_death = 0.16, nsims = 1000) {
  
  ## testing
  # access = covar_df$covar; ctar_in = covar_df$ctar_in_district;
  #  pop = covar_df$pop; catch = covar_df$catch; names = covar_df$names;
  #  group_name = covar_df$group_name; beta_access = i$beta_access;
  #  beta_ctar = i$beta_ctar; beta_0 = i$beta_0; beta_pop = i$beta_pop;
  #  sigma_0 = i$sigma_0;
  #  known_alphas = known_alphas;
  #  covar_name = i$covar_name; pop_predict = i$pop_predict; intercept = i$intercept;
  #  summed = i$summed; ctar_bump = i$ctar_bump; data_source = i$data_source;
  #  scale = i$scale;
  #  trans = 1e5; nsims = 1000; known_catch = TRUE;
  
  bite_mat <- death_mat <- averted_mat <- p_rabid_mat <- reporting_mat <- matrix(NA,
                                                                                 nrow = length(access), 
                                                                                 ncol = nsims)
  
  for(i in 1:nsims) {
    
    if(intercept == "random") {
      ## draw catchment level effects
      alpha <- rnorm(max(catch), mean = beta_0, sd = sigma_0)
      alpha <- alpha[catch]
      
      ## get alpha so that if known_catch is TRUE it pulls from estimates
      ## known alphas have to be the same length as the data (potentially change this part!)
      if(known_catch == TRUE) {
        alpha <- ifelse(is.na(known_alphas) | known_alphas == 0, alpha, known_alphas)
      } 
      
      if (pop_predict == "flatPop") {
        exp_bites <- exp(alpha + beta_access*access + beta_ctar*ctar_in)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(alpha + beta_access*access + beta_ctar*ctar_in + beta_pop*pop/trans) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(alpha + beta_pop*pop/trans + beta_ctar*ctar_in)
      }
    }
    
    if(intercept == "fixed") {
      if (pop_predict == "flatPop") {
        exp_bites <- exp(beta_0 + beta_access*access + beta_ctar*ctar_in)*pop
      }
      
      if(pop_predict == "addPop") {
        exp_bites <- exp(beta_0 + beta_access*access + beta_ctar*ctar_in + beta_pop*pop/trans) 
      }
      
      if(pop_predict == "onlyPop") {
        exp_bites <- exp(beta_0 + beta_pop*pop/trans + beta_ctar*ctar_in)
      }    
    }
    
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
    
    bite_mat[, i] <- bites
    death_mat[, i] <- deaths
    averted_mat[, i] <- averted
    p_rabid_mat[, i] <- p_rabid_t
    reporting_mat[, i] <- reported_rabid/rabid_exps
    
  }
  
  bites_mean <- rowMeans(bite_mat, na.rm = TRUE)
  bites_lower05 <- apply(bite_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
  bites_upper95 <-apply(bite_mat, 1, quantile, prob = 0.95, na.rm = TRUE)

  deaths_mean <- rowMeans(death_mat, na.rm = TRUE)
  deaths_lower05 <- apply(death_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
  deaths_upper95 <-apply(death_mat, 1, quantile, prob = 0.95, na.rm = TRUE)

  averted_mean <- rowMeans(averted_mat, na.rm = TRUE)
  averted_lower05 <- apply(averted_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
  averted_upper95 <-apply(averted_mat, 1, quantile, prob = 0.95, na.rm = TRUE)

  p_rabid_mean <- rowMeans(p_rabid_mat, na.rm = TRUE)
  p_rabid_lower05 <- apply(p_rabid_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
  p_rabid_upper95 <-apply(p_rabid_mat, 1, quantile, prob = 0.95, na.rm = TRUE)
  
  reporting_mean <- rowMeans(reporting_mat, na.rm = TRUE)
  reporting_lower05 <- apply(reporting_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
  reporting_upper95 <-apply(reporting_mat, 1, quantile, prob = 0.95, na.rm = TRUE)
  
  return(data.frame(names, group_name, bites_mean, bites_upper95, bites_lower05, 
                    deaths_mean, deaths_upper95, deaths_lower05, averted_mean,
                    averted_upper95, averted_lower05, 
                    p_rabid_mean, p_rabid_upper95, p_rabid_lower05,
                    reporting_mean, reporting_upper95, reporting_lower05,
                    covar_name, data_source, scale, pop_predict, ctar_bump, summed, intercept, 
                    access, ctar_in, pop, catch))
  
}


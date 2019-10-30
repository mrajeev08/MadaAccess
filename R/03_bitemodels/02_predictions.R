####################################################################################################
##' Run predictions from all candidate models
##' Details: Models include travel times and distance as access metrics, in addition to population 
##' Author: Malavika Rajeev 
####################################################################################################
## source bite ests
source("R/02_bitedata/03_estimate_biteinc.R") # either source this or output bite data
# probs want to output bite data in order to pull into other things
source("R/functions/predict_bites.R")

## libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)

## Estimates
model_ests <- read.csv("output/bitemod_results.csv")
model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means

##' Observed vs. Predicted for all models
##' ------------------------------------------------------------------------------------------------
preds_mada <- 
  foreach(i = iter(model_means, by = "row"), j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(model_means)*100)
    # i <- as.data.frame(model_means[2, ])
    
    if(i$data_source == "Moramanga") {
        bite_df <- covar_df <- morabites_by_ttimes
    } else {
      bite_df <- covar_df <- bites_by_ttimes
      if(i$scale == "Commune") {
        covar_df <- commcovars_by_ttimes
      }
    }
    
    ## Also transform covar
    covar_df$covar <- covar_df$covar/60
    
    if(i$intercept == "random") {
      known_alphas <-  as.numeric(i[, match(glue("alpha[{covar_df$catch}]"), colnames(i))])
    } else {
      known_alphas <- rep(NA, nrow(covar_df))
    }
    
    predict.bites(access = covar_df$covar, ctar_in = covar_df$ctar_in_district, 
                  pop = covar_df$pop, catch = covar_df$catch, names = covar_df$names, 
                  group_name = covar_df$group_name, beta_access = i$beta_access, 
                  beta_ctar = i$beta_ctar, beta_0 = i$beta_0, beta_pop = i$beta_pop, 
                  sigma_0 = i$sigma_0, 
                  known_alphas = known_alphas, 
                  covar_name = i$covar_name, pop_predict = i$pop_predict, intercept = i$intercept,
                  summed = i$summed, ctar_bump = i$ctar_bump, data_source = i$data_source, 
                  scale = i$scale,
                  trans = 1e5, nsims = 1000, known_catch = TRUE)
  }

observed_mada <- bind_rows(bites_by_ttimes, bites_by_distwtd, bites_by_distcent)
observed_mada$data_source <- "National"
observed_mora <- bind_rows(morabites_by_ttimes, morabites_by_distwtd, morabites_by_distcent)
observed_mora$data_source <- "Moramanga"
observed <- bind_rows(observed_mada, observed_mora)

preds_mada %>%
  group_by(group_name, data_source, covar_name, data_source, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(starts_with("bites")), sum) %>%
  left_join(observed) -> preds_grouped
write.csv(preds_grouped, "output/preds/fitted_grouped_all.csv", row.names = FALSE)
write.csv(preds_mada, "output/preds/fitted_ungrouped_all.csv", row.names = FALSE)

##' Out of fit 
##' ------------------------------------------------------------------------------------------------
##' Use commune and district models to predict Moramanga data
model_means %>%
  filter(data_source == "National") -> mada_means

outfit_mora <- 
  foreach(i = iter(mada_means, by = "row"), j = icount(), 
          .combine = "rbind") %do% {
    
    print(j/nrow(mada_means)*100)
    # i <- as.data.frame(mada_means[2, ])
            
    bite_df <- covar_df <- morabites_by_ttimes
    covar_df$covar <- covar_df$covar/60
    
    if(i$intercept == "random") {
      known_alphas <-  as.numeric(i[, match(glue("alpha[{covar_df$catch}]"), colnames(i))])
    } else {
      known_alphas <- rep(NA, nrow(covar_df))
    }
    
    check <- predict.bites(access = covar_df$covar, ctar_in = covar_df$ctar_in_district, 
                  pop = covar_df$pop, catch = covar_df$catch, names = covar_df$names, 
                  group_name = covar_df$group_name, beta_access = i$beta_access, 
                  beta_ctar = i$beta_ctar, beta_0 = i$beta_0, beta_pop = i$beta_pop, 
                  sigma_0 = i$sigma_0, 
                  known_alphas = known_alphas, 
                  covar_name = i$covar_name, pop_predict = i$pop_predict, intercept = i$intercept,
                  summed = i$summed, ctar_bump = i$ctar_bump, data_source = i$data_source, 
                  scale = i$scale,
                  trans = 1e5, nsims = 1000, known_catch = TRUE)
    
    check$type <- "Mora_outfit"
    check$avg_bites <- bite_df$avg_bites
    check
  }
write.csv(outfit_mora, "output/preds/outoffit_mora.csv", row.names = FALSE)


##' Use Moramanga model to predict district and commune model
##' 
model_means %>%
  filter(data_source == "Moramanga") -> mora_means
scale <- c("Commune", "District")

outfit_mada <- 
  foreach(k = 1:length(scale), .combine = "rbind") %:%
  foreach(i = iter(mora_means, by = "row"), j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(mora_means)*100)
    # i <- as.data.frame(mora_means[2, ])
 
    
    
    bite_df <- covar_df <- bites_by_ttimes
    if(scale[k] == "Commune") {
      covar_df <- commcovars_by_ttimes
    }
    covar_df$covar <- covar_df$covar/60
    
    if(i$intercept == "random") {
      known_alphas <-  as.numeric(i[, match(glue("alpha[{covar_df$catch}]"), colnames(i))])
    } else {
      known_alphas <- rep(NA, nrow(covar_df))
    }
    
    check <- predict.bites(access = covar_df$covar, ctar_in = covar_df$ctar_in_district, 
                           pop = covar_df$pop, catch = covar_df$catch, names = covar_df$names, 
                           group_name = covar_df$group_name, beta_access = i$beta_access, 
                           beta_ctar = i$beta_ctar, beta_0 = i$beta_0, beta_pop = i$beta_pop, 
                           sigma_0 = i$sigma_0, 
                           known_alphas = known_alphas, 
                           covar_name = i$covar_name, pop_predict = i$pop_predict, intercept = i$intercept,
                           summed = i$summed, ctar_bump = i$ctar_bump, data_source = "National", 
                           scale = scale[k],
                           trans = 1e5, nsims = 1000, known_catch = TRUE)
    check$type <- "Mada_outoffit"
    check
  }

## Additional option for fitted vs. random catchment effect
outfit_mada %>%
  group_by(group_name, data_source, covar_name, data_source, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(starts_with("bites")), sum, na.rm = TRUE) %>%
  left_join(observed) -> outfit_grouped
write.csv(outfit_grouped, "output/preds/outoffit_grouped_mada.csv", row.names = FALSE)

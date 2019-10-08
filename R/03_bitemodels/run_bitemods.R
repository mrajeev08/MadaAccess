####################################################################################################
##' Run models of bite incidence using spatial covariates 
##' Details: Models include travel times and distance as access metrics, in addition to population 
##' Author: Malavika Rajeev 
####################################################################################################
## source bite ests
source("R/02_bitedata/03_estimate_biteinc.R") # either source this or output bite data
# probs want to output bite data in order to pull into other things
source("R/functions/models.R")

## libraries
library(bbmle)
library(foreach)
library(iterators)

##' Run national models 
##' ------------------------------------------------------------------------------------------------
## Mada
## These all should have same index letter
bites <- list(bites_by_distance$avg_bites, bites_by_distance$avg_bites,
              bites_by_ttimes$avg_bites, bites_by_ttimes$avg_bites,
              morabites_by_ttimes$avg_bites, morabites_by_distance$avg_bites)
covars <- list(bites_by_distance$distance, commcovars_by_distance$distance,
               bites_by_ttimes$ttimes/60, commcovars_by_ttimes$ttimes/60,
               morabites_by_ttimes$ttimes/60, morabites_by_distance$distance)
covar_name <- c("distance", "distance", "ttimes", "ttimes", "ttimes", "distance")
scale <- c("District", "Commune", "District", "Commune", "Moramanga", "Moramanga")
names_bites <- list(bites_by_distance$distcode, bites_by_distance$distcode, 
                    bites_by_ttimes$distcode, bites_by_ttimes$distcode,
                    morabites_by_ttimes$commcode, morabites_by_distance$commcode)
names_covar <- list(bites_by_distance$distcode, commcovars_by_distance$distcode, 
                    bites_by_ttimes$distcode, commcovars_by_ttimes$distcode,
                    morabites_by_ttimes$commcode, morabites_by_distance$commcode)
pop <- list(bites_by_distance$pop, commcovars_by_distance$pop,
            bites_by_ttimes$pop, commcovars_by_ttimes$pop,
            morabites_by_ttimes$pop, morabites_by_distance$pop)
sum_it <- c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)

## The second index letter
pop_predict <- c("addPop", "onlyPop", "flatPop")

## Plus extra labels
run_type = "optim"
loc = "Mada"

mods_all <- 
  foreach(i = 1:length(bites), .combine = c) %:%
  foreach(k = 1:length(pop_predict), .combine = c, .errorhandling = 'remove') %do% {
    data_est <- list(bites = bites[[i]], names_bites = names_bites[[i]], covar = covars[[i]],
                     names_covar = names_covar[[i]], sum = sum_it[i], pop = pop[[i]],
                     pop_predict = pop_predict[k], trans = 1e5, run = run_type)
    
    start_params <- list(beta = 1e-5, intercept = 0.1)
    
    if(pop_predict[k] == "addPop") {
      ## need beta_pop
      start_params <- c(start_params, list(beta_pop = 1e-5))
    }
    
    mods <- mle2(model.bites, start = start_params, data = data_est)
    
    attributes(mods)$scale <- scale[i]
    attributes(mods)$pop_predict <- pop_predict[k]
    attributes(mods)$covar_name <- covar_name[i]
    attributes(mods)$nobs <- length(bites[[i]])
    attributes(mods)$mnames <- paste0(covar_name[i], "_", scale[i], "_", pop_predict[k])
    attributes(mods)$AICc <- AICc(mods, nobs = length(bites[[i]]))
    mods
  }

mods_covars <- Filter(function(x) !grepl("addPop", attributes(x)$pop_predict, fixed = TRUE), mods_all)
prof_CI_covars <- lapply(mods_covars, confint)
mods_pop <- Filter(function(x) grepl("addPop", attributes(x)$pop_predict, fixed = TRUE), mods_all)
prof_CI_pop <- lapply(mods_pop, confint)

bite_mods_covar_df <- data.frame(covar_name = mapply(function(x) attributes(x)$covar_name, mods_covars),
                                 scale = mapply(function(x) attributes(x)$scale, mods_covars),
                                 pop_predict = mapply(function(x) attributes(x)$pop_predict, mods_covars), 
                                 beta = mapply(function(x) coef(x)["beta"], mods_covars),
                                 intercept = mapply(function(x) coef(x)["intercept"], mods_covars),
                                 beta_upper = mapply(function(x) x["beta", 2], prof_CI_covars),
                                 beta_lower = mapply(function(x) x["beta", 1], prof_CI_covars),
                                 intercept_lower = mapply(function(x) x["intercept", 1], prof_CI_covars),
                                 intercept_upper = mapply(function(x) x["intercept", 2], prof_CI_covars),
                                 likelihood = mapply(function(x) logLik(x), mods_covars),
                                 AICc = mapply(function(x) attributes(x)$AICc, mods_covars), 
                                 bites = I(mapply(function(x) x@data$bites, mods_covars)), 
                                 names_bites = I(mapply(function(x) x@data$names_bites, mods_covars)),
                                 covar = I(mapply(function(x) x@data$covar, mods_covars)), 
                                 names_covar = I(mapply(function(x) x@data$names_covar, mods_covars)),
                                 pop = I(mapply(function(x) x@data$pop, mods_covars)), 
                                 sum = mapply(function(x) x@data$sum, mods_covars))

bite_mods_pop_df <- data.frame(covar_name = mapply(function(x) attributes(x)$covar_name, mods_pop),
                               scale = mapply(function(x) attributes(x)$scale, mods_pop),
                               pop_predict = mapply(function(x) attributes(x)$pop_predict, mods_pop),
                               beta = mapply(function(x) coef(x)["beta"], mods_pop),
                               beta_pop = mapply(function(x) coef(x)["beta_pop"], mods_pop),
                               intercept = mapply(function(x) coef(x)["intercept"], mods_pop),
                               beta_upper = mapply(function(x) x["beta", 2], prof_CI_pop),
                               beta_lower = mapply(function(x) x["beta", 1], prof_CI_pop),
                               beta_pop_upper = mapply(function(x) x["beta_pop", 2], prof_CI_pop),
                               beta_pop_lower = mapply(function(x) x["beta_pop", 1], prof_CI_pop),
                               intercept_lower = mapply(function(x) x["intercept", 1], prof_CI_pop),
                               intercept_upper = mapply(function(x) x["intercept", 2], prof_CI_pop),
                               likelihood = mapply(function(x) logLik(x), mods_pop),
                               AICc = mapply(function(x) attributes(x)$AICc, mods_pop),
                               bites = I(mapply(function(x) x@data$bites, mods_pop)), 
                               names_bites = I(mapply(function(x) x@data$names_bites, mods_pop)),
                               covar = I(mapply(function(x) x@data$covar, mods_pop)), 
                               names_covar = I(mapply(function(x) x@data$names_covar, mods_pop)),
                               pop = I(mapply(function(x) x@data$pop, mods_pop)), 
                               sum = mapply(function(x) x@data$sum, mods_pop))
                               
mod_df <- bind_rows(bite_mods_covar_df, bite_mods_pop_df)

## Preds data
preds_data <- foreach(i = iter(mod_df, by = "row"), 
                      .combine = "rbind") %do% {
    ests <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                        covar = unlist(i$covar), pop = unlist(i$pop), 
                        names_covar = unlist(i$names_covar), sum = i$sum, 
                        pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                        beta_pop = i$beta_pop, intercept = i$intercept, trans = 1e5, run = "predict.data")
       
    ests$lower <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                              covar = unlist(i$covar), pop = unlist(i$pop), 
                              names_covar = unlist(i$names_covar), sum = i$sum, 
                              pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta_lower, 
                              beta_pop = i$beta_pop_lower, intercept = i$intercept_lower, trans = 1e5, 
                              run = "predict.data")$predicted
    
    ests$upper <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                              covar = unlist(i$covar), pop = unlist(i$pop), 
                              names_covar = unlist(i$names_covar), sum = i$sum, 
                              pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta_upper, 
                              beta_pop = i$beta_pop_upper, intercept = i$intercept_upper, trans = 1e5, 
                              run = "predict.data")$predicted  
    ests$scale <- i$scale  
    ests
}


## Predictions: fixed
ttimes_plot <- seq(0, 25, by = 0.5)
distance_plot <- seq(0, 200, by = 2.5)

preds_fixed <- foreach(i = iter(mod_df, by = "row"), .combine = "rbind") %do% {
        
  if(i$covar_name == "ttimes"){
    covar_plot <- ttimes_plot
  } else { covar_plot <- distance_plot }
  
  if (i$pop_predict == "onlyPop"){
    pop_plot <- seq(1000, 1e6, length.out = length(covar_plot))
  } else{ pop_plot = 1e5 } 
  
  preds <- predict.bites(names_bites = NA,
                         covar = covar_plot, pop = pop_plot, 
                         names_covar = NA, sum = i$sum, 
                         pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                         beta_pop = i$beta_pop, intercept = i$intercept, trans = 1e5)
  preds$scale <- i$scale
  preds
}

## Just data and covars

## Preds data
preds_data <- foreach(i = iter(mod_df, by = "row"), 
                      .combine = "rbind") %do% {
    ests <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                        covar = unlist(i$covar), pop = unlist(i$pop), 
                        names_covar = unlist(i$names_covar), sum = i$sum, 
                        pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                        beta_pop = i$beta_pop, intercept = i$intercept, trans = 1e5, run = "predict.data")
                        
    ests$lower <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                              covar = unlist(i$covar), pop = unlist(i$pop), 
                              names_covar = unlist(i$names_covar), sum = i$sum, 
                              pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta_lower, 
                              beta_pop = i$beta_pop_lower, intercept = i$intercept_lower, trans = 1e5, 
                              run = "predict.data")$predicted
    
    ests$upper <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                              covar = unlist(i$covar), pop = unlist(i$pop), 
                              names_covar = unlist(i$names_covar), sum = i$sum, 
                              pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta_upper, 
                              beta_pop = i$beta_pop_upper, intercept = i$intercept_upper, trans = 1e5, 
                              run = "predict.data")$predicted  
    ests$scale <- i$scale  
    ests
  }


mod_names <- c("flatPop" = "Flat incidence", "addPop" = "With pop", 
               "onlyPop" = "Only pop")

ggplot(preds_data, aes(x = log(observed + 0.1), y = log(predicted + 0.1), color = scale)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("#004b49", "#cc7722", "#b7410e"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", 
             labeller = labeller(pop_predict = as_labeller(mod_names)), drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Observed bites") +
  ylab("Predicted bites") +
  labs(tag = "A") 

bites_by_distance %>%
  select(covar = distance, avg_bites, pop) %>%
  mutate(scale = "District", covar_name = "distance") -> obs_covar_dist
bites_by_ttimes %>%
  select(covar = ttimes, avg_bites, pop) %>%
  mutate(covar = covar/60, scale = "District", covar_name = "ttimes") %>%
  bind_rows(obs_covar_dist) -> obs_covar_dist
morabites_by_distance %>%
  select(covar = distance, avg_bites, pop) %>%
  mutate(scale = "Moramanga", covar_name = "distance") -> mora_covar_dist
morabites_by_ttimes %>%
  select(covar = ttimes, avg_bites, pop) %>%
  mutate(covar = covar/60, scale = "Moramanga", covar_name = "ttimes") %>%
  bind_rows(mora_covar_dist) %>%
  bind_rows(obs_covar_dist) -> obs_covar_dist

ggplot(filter(preds_fixed, pop_predict != "onlyPop"), aes(x = covar, y = predicted, color = scale)) + 
  geom_line() +
  geom_point(data = obs_covar_dist, aes(x = covar, y = avg_bites/pop*1e5, color = scale), alpha = 0.5) +
  scale_color_manual(values = c("#004b49", "#cc7722", "darkred"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free", 
             labeller = labeller(pop_predict = as_labeller(mod_names)), drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Covariate") +
  ylab("Predicted bites") +
  labs(tag = "A")

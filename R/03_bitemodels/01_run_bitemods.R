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
bites_df <- list(bites_by_distance, bites_by_distance,
              bites_by_ttimes, bites_by_ttimes,
              morabites_by_ttimes, morabites_by_distance)
covar_df <- list(bites_by_distance, commcovars_by_distance, 
                 bites_by_ttimes, commcovars_by_ttimes, 
                 morabites_by_ttimes, morabites_by_distance)
scale <- c("District", "Commune", "District", "Commune", "Moramanga", "Moramanga")
sum_it <- c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
ctar_bump <- c(TRUE, FALSE)

## The second index letter
pop_predict <- c("addPop", "onlyPop", "flatPop")

## Plus extra labels
run_type = "optim"

mods_all <- 
  foreach(i = 1:length(bites_df), .combine = c) %:%
  foreach(j = 1:length(ctar_bump), .combine = c) %:%
  foreach(k = 1:length(pop_predict), .combine = c) %do% {
    
    start_params <- list(beta = 1e-5, intercept = 0.1)
    
    if(pop_predict[k] == "addPop") {
      ## need beta_pop
      start_params <- c(start_params, list(beta_pop = 1e-5))
    }
    
    if(ctar_bump[j] == TRUE) {
      start_params <- c(start_params, list(beta_ctar = 1e-5))
    }
    
    bites <- bites_df[[i]]
    covars <- covar_df[[i]]
    
    if(covars$covar_name[1] == "ttimes") {
      covar <- covars$covar/60
    } else {
      covar <- covars$covar
    }
    
    data_est <- list(bites = bites$avg_bites, ctar_in = covars$ctar_in_district, 
                     names_bites = bites$names_covar, covar = covar,
                     names_covar = covars$names_covar, sum = sum_it[i], pop = covars$pop,
                     pop_predict = pop_predict[k], ctar_bump = ctar_bump[j], 
                     trans = 1e5, run = run_type)
    
    mods <- mle2(model.bites, start = start_params, data = data_est)
    
    attributes(mods)$scale <- scale[i]
    attributes(mods)$ctar_bump <- ctar_bump[j]
    attributes(mods)$pop_predict <- pop_predict[k]
    attributes(mods)$covar_name <- covars$covar_name[1]
    attributes(mods)$nobs <- length(bites$avg_bites)
    attributes(mods)$mnames <- paste0(covars$covar_name[1], "_", scale[i], "_", pop_predict[k])
    attributes(mods)$AICc <- AICc(mods, nobs = length(bites$avg_bites))
    mods
  }

prof_CI <- lapply(mods_all, confint)

mod_df <- data.frame(covar_name = mapply(function(x) attributes(x)$covar_name, mods_all),
                     scale = mapply(function(x) attributes(x)$scale, mods_all),
                     pop_predict = mapply(function(x) attributes(x)$pop_predict, mods_all),
                     ctar_bump = mapply(function(x) attributes(x)$ctar_bump, mods_all),
                     beta = mapply(function(x) coef(x)["beta"], mods_all),
                     beta_upper = mapply(function(x) x["beta", 2], prof_CI),
                     beta_lower = mapply(function(x) x["beta", 1], prof_CI),
                     beta_pop = mapply(function(x) coef(x)["beta_pop"], mods_all),
                     beta_pop_upper = mapply(function(x) ifelse("beta_pop" %in% rownames(x), 
                                                                x["beta_pop", 2], NA), prof_CI),
                     beta_pop_lower = mapply(function(x) ifelse("beta_pop" %in% rownames(x), 
                                                                x["beta_pop", 1], NA), prof_CI),
                     beta_ctar = mapply(function(x) coef(x)["beta_ctar"], mods_all),
                     beta_ctar_upper = mapply(function(x) ifelse("beta_ctar" %in% rownames(x), 
                                                                x["beta_ctar", 2], NA), prof_CI),
                     beta_ctar_lower = mapply(function(x) ifelse("beta_ctar" %in% rownames(x), 
                                                                x["beta_ctar", 1], NA), prof_CI),
                     intercept = mapply(function(x) coef(x)["intercept"], mods_all),
                     intercept_lower = mapply(function(x) x["intercept", 1], prof_CI),
                     intercept_upper = mapply(function(x) x["intercept", 2], prof_CI),
                     likelihood = mapply(function(x) logLik(x), mods_all),
                     AICc = mapply(function(x) attributes(x)$AICc, mods_all),
                     bites = I(mapply(function(x) x@data$bites, mods_all)), 
                     names_bites = I(mapply(function(x) x@data$names_bites, mods_all)),
                     covar = I(mapply(function(x) x@data$covar, mods_all)), 
                     names_covar = I(mapply(function(x) x@data$names_covar, mods_all)),
                     pop = I(mapply(function(x) x@data$pop, mods_all)), 
                     ctar_in = I(mapply(function(x) x@data$ctar_in, mods_all)), 
                     sum = mapply(function(x) x@data$sum, mods_all))
                               

## Preds data
preds_data <- foreach(i = iter(mod_df, by = "row"), 
                      .combine = "rbind") %do% {
    ests <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                        covar = unlist(i$covar), pop = unlist(i$pop),  ctar_in = unlist(i$ctar_in),
                        names_covar = unlist(i$names_covar), sum = i$sum, 
                        pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                        beta_pop = i$beta_pop, intercept = i$intercept, 
                        ctar_bump = i$ctar_bump, beta_ctar = i$beta_ctar, trans = 1e5, run = "predict.data")
       
    ests$lower <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                              covar = unlist(i$covar), pop = unlist(i$pop), ctar_in = unlist(i$ctar_in),
                              names_covar = unlist(i$names_covar), sum = i$sum, 
                              pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta_lower, 
                              beta_pop = i$beta_pop_lower, intercept = i$intercept_lower, 
                              ctar_bump = i$ctar_bump, beta_ctar = i$beta_ctar_lower, trans = 1e5, 
                              run = "predict.data")$predicted
    
    ests$upper <- model.bites(bites = unlist(i$bites), names_bites = unlist(i$names_bites), 
                              covar = unlist(i$covar), pop = unlist(i$pop),  ctar_in = unlist(i$ctar_in),
                              names_covar = unlist(i$names_covar), sum = i$sum, 
                              pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta_upper, 
                              beta_pop = i$beta_pop_upper, intercept = i$intercept_upper, 
                              ctar_bump = i$ctar_bump, beta_ctar = i$beta_ctar_upper, trans = 1e5, 
                              run = "predict.data")$predicted  
    ests$scale <- i$scale  
    ests
}

preds_data$dataset <- ifelse(preds_data$scale == "Moramanga", "Moramanga", "National")
mod_df$dataset <- ifelse(mod_df$scale == "Moramanga", "Moramanga", "National")
mod_df$scale <- ifelse(mod_df$scale == "District", "District", "Commune")

mod_names <- c("flatPop" = "Flat incidence", "addPop" = "With pop", 
               "onlyPop" = "Only pop")

ggplot(preds_data, aes(x = log(observed + 0.1), y = log(predicted + 0.1), color = scale,
                       shape = ctar_bump)) + 
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", 
             labeller = labeller(pop_predict = as_labeller(mod_names)), drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Log observed bites)") +
  ylab("Log predicted bites") +
  labs(tag = "A") 

ggplot(mod_df, aes(x = pop_predict, y = AICc, color = ctar_bump, shape = scale)) +
  geom_point() + 
  facet_grid(dataset ~ covar_name, scales = "free")

## Predictions: fixed
ttimes_plot <- seq(0, 25, by = 0.5)
distance_plot <- seq(0, 200, by = 2.5)

preds_fixed <- foreach(i = iter(mod_df, by = "row"), .combine = "rbind") %do% {
        
  if(i$covar_name == "ttimes"){
    covar_plot <- ttimes_plot
  } else { covar_plot <- distance_plot }
  
  if (i$pop_predict == "onlyPop"){
    pop_plot <- seq(1000, 1e6, length.out = length(covar_plot))
  } else {
    pop_plot = 1e5
  } 
  
  preds <- predict.bites(names_bites = NA,
                         covar = covar_plot, pop = pop_plot, 
                         names_covar = NA, sum = i$sum, 
                         pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                         beta_pop = i$beta_pop, intercept = i$intercept, trans = 1e5)
  preds$scale <- i$scale
  preds
}


## Out of fit
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
ctar_bump <- c(TRUE, FALSE)



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

ggplot() + 
  geom_line(data = filter(preds_fixed, pop_predict == "flatPop"), 
            aes(x = covar, y = predicted/pop*1e5, color = scale), 
            size = 1.2, alpha = 0.75) +
  geom_point(data = obs_covar_dist, aes(x = covar, y = avg_bites/pop*1e5, 
                                        color = scale), 
             alpha = 0.5, size = 2, shape = 1, stroke = 1.2) +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free", 
             labeller = labeller(pop_predict = as_labeller(mod_names)), drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Covariate") +
  ylab("Bites per 100k") +
  labs(tag = "A") +
  geom_hline(yintercept = c(15.6, 78), linetype = 2, color = "grey") +
  theme(panel.grid.minor = element_blank())

ggplot() + 
  geom_point(data = bites_by_distance, aes(x = distance, y = avg_bites/pop*1e5)) +
  expand_limits(y = 0) +
  xlab("Covariate") +
  ylab("Bites per 100k") +
  facet_wrap(~catchment, nrow = 5)
  labs(tag = "A") +
  geom_hline(yintercept = c(15.6, 78), linetype = 2, color = "grey") +
  theme(panel.grid.minor = element_blank())

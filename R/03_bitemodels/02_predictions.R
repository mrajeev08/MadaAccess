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
  group_by(data_source, covar_name, data_source, scale, pop_predict, ctar_bump, 
           summed, intercept) %>%
  summarize(dic = mean(dic)) -> dic_ests
dic_ests %>%
  group_by(data_source, covar_name) %>%
  arrange(dic, .by_group = TRUE) -> dic_ranks
knitr::kable(dic_ranks)

## convergence plots
model_ests %>%
  select(data_source, covar_name, data_source, scale, pop_predict, ctar_bump, 
           summed, intercept, val = psrf_upper) %>%
  mutate(type = "psrf_upper") -> psrf
model_ests %>%
  select(data_source, covar_name, data_source, scale, pop_predict, ctar_bump, 
         summed, intercept, val = mpsrf) %>%
  mutate(type = "mpsrf") -> mpsrf
convergence <- bind_rows(mpsrf, psrf)
ggplot(convergence, aes(x = type, y = val, color = interaction(data_source, scale))) + 
  geom_boxplot() +
  ylim(c(0.90, 1.2)) +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey")
  

model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means

## Estimates of access
ggplot(data = filter(model_means, intercept == "fixed"), 
       aes(x = pop_predict, y = beta_access, 
           color = interaction(scale, data_source), shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(~ covar_name, scales = "free", drop = TRUE) 


##' Observed vs. Predicted for all models
##' ------------------------------------------------------------------------------------------------
preds_mada <- 
  foreach(i = iter(model_means, by = "row"), j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(model_means)*100)
    # i <- as.data.frame(model_means[2, ])
    
    if(i$covar_name == "dist_cent") {
      if(i$data_source == "Moramanga") {
        bite_df <- covar_df <- morabites_by_distcent
      } else {
        bite_df <- covar_df <- bites_by_distcent
        if(i$scale == "Commune") {
          covar_df <- commcovars_by_distcent
        }
      }
    }
    
    if(i$covar_name == "ttimes") {
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
    }
    
    if(i$covar_name == "dist_wtd") {
      if(i$data_source == "Moramanga") {
        bite_df <- covar_df <- morabites_by_distwtd
      } else {
        bite_df <- covar_df <- bites_by_distwtd
        if(i$scale == "Commune") {
          covar_df <- commcovars_by_distwtd
        }
      }
    }
    
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
write.csv(preds_grouped, "output/preds/preds_grouped_all.csv", row.names = FALSE)
write.csv(preds_mada, "output/preds/preds_ungrouped_all.csv", row.names = FALSE)

ggplot(data = filter(preds_grouped, intercept == "fixed"), 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
                                 color = interaction(scale, data_source), shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_linerange(aes(ymin = log(bites_05quant + 0.1), ymax = log(bites_95quant + 0.1)), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Observed bites") +
  ylab("Predicted bites") +
  labs(tag = "A") 
  
ggplot(data = filter(preds_grouped, intercept == "random"), 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = interaction(scale, data_source), shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_linerange(aes(ymin = log(bites_05quant + 0.1), ymax = log(bites_95quant + 0.1)), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Observed bites") +
  ylab("Predicted bites") +
  labs(tag = "B") 

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
            
    if(i$covar_name == "dist_cent") {
      bite_df <- covar_df <- morabites_by_distcent
    }
    
    if(i$covar_name == "ttimes") {
      bite_df <- covar_df <- morabites_by_ttimes
      covar_df$covar <- covar_df$covar/60
    }
    
    if(i$covar_name == "dist_wtd") {
      bite_df <- covar_df <- morabites_by_distwtd
      }
    
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

write.csv(outfit_mora, "output/preds/outfit_mora.csv", row.names = FALSE)

ggplot(data = filter(outfit_mora, intercept == "fixed"), 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = scale, shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_linerange(aes(ymin = log(bites_05quant + 0.1), ymax = log(bites_95quant + 0.1)), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("darkred", "#cc7722"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Observed bites") +
  ylab("Predicted bites") +
  labs(tag = "A") 

ggplot(data = filter(outfit_mora, intercept == "random"), 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = scale, shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_linerange(aes(ymin = log(bites_05quant + 0.1), ymax = log(bites_95quant + 0.1)), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("darkred", "#cc7722"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Observed bites") +
  ylab("Predicted bites") +
  labs(tag = "A") 


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
    print(scale[k])
    if(i$covar_name == "dist_cent") {
      bite_df <- covar_df <- bites_by_distcent
      if(scale[k] == "Commune") {
        covar_df <- commcovars_by_distcent
      }
    }
    
    if(i$covar_name == "ttimes") {
      bite_df <- covar_df <- bites_by_ttimes
      if(scale[k] == "Commune") {
        covar_df <- commcovars_by_ttimes
      }
      covar_df$covar <- covar_df$covar/60
    }
    
    if(i$covar_name == "dist_wtd") {
      bite_df <- covar_df <- bites_by_distwtd
      if(scale[k] == "Commune") {
        covar_df <- commcovars_by_distwtd
      }
    }
    
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

ggplot(data = outfit_grouped, 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = scale, shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_linerange(aes(ymin = log(bites_05quant + 0.1), ymax = log(bites_95quant + 0.1)), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Observed bites") +
  ylab("Predicted bites") +
  labs(tag = "A") 
write.csv(outfit_grouped, "output/preds/outfit_grouped_mada.csv", row.names = FALSE)

##' Predict deaths 
##' ------------------------------------------------------------------------------------------------
mada_communes@data %>%
  select(group_name = distcode, district = ADM2_EN, names = ADM3_PCODE, commune = ADM3_EN,
         pop, long, lat, 
         catch_ttimes = ctch_ttwtd, ttimes = ttms_wtd, 
         catch_dist_cent = ctch_dsct, dist_cent,  catch_dist_wtd = ctch_dswtd, dist_wtd,
         ctar_in_district) %>%
  mutate(ttimes = ttimes/60) -> covar_df_commune
mada_districts@data %>%
  select(group_name = distcode, district = ADM2_EN,
         pop, long, lat, 
         catch_ttimes = ctch_ttwtd, ttimes = ttms_wtd, 
         catch_dist_cent = ctch_dsct, dist_cent,  catch_dist_wtd = ctch_dswtd, dist_wtd,
         ctar_in_district) %>%
  mutate(names = group_name, 
         ttimes = ttimes/60) -> covar_df_district
  
preds_burden <- 
  foreach(i = iter(model_means, by = "row"), j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(model_means)*100)
    
    if(i$scale == "Commune") {
      covar_df <- covar_df_commune
    } else {
      covar_df <- covar_df_district
    }
    
    catches <- as.numeric(droplevels(covar_df[, paste0("catch_", i$covar_name)]))
    
    predict.deaths(access = covar_df[, as.character(i$covar_name)], ctar_in = covar_df$ctar_in_district, 
                  pop = covar_df$pop, catch = catches, names = covar_df$names, 
                  group_name = covar_df$group_name, beta_access = i$beta_access, 
                  beta_ctar = i$beta_ctar, beta_0 = i$beta_0, beta_pop = i$beta_pop, 
                  sigma_0 = i$sigma_0, 
                  known_alphas = NA, 
                  covar_name = i$covar_name, pop_predict = i$pop_predict, intercept = i$intercept,
                  summed = i$summed, ctar_bump = i$ctar_bump, data_source = i$data_source, 
                  scale = i$scale,
                  trans = 1e5, known_catch = FALSE, 
                  p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.9,
                  max_HDR = 25, min_HDR = 5, 
                  dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                  prob_death = 0.16, nsims = 1000)
  }

## Grouped to district
preds_burden %>%
  group_by(group_name, data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(bites_mean:averted_lower05, pop), sum, na.rm = TRUE) -> grouped_deaths

preds_burden %>%
  group_by(group_name, data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(starts_with("p_rabid"), starts_with("reporting"), starts_with("access")), mean, na.rm = TRUE) %>%
  left_join(grouped_deaths) -> grouped_deaths


ggplot(data = filter(grouped_deaths, intercept == "fixed"), 
       aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5, 
           color = interaction(scale, data_source), shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Districts (ordered by decreasing access") +
  ylab("Predicted incidence of deaths") +
  labs(tag = "A") +
  theme(axis.text.x = element_blank())

ggplot(data = filter(grouped_deaths, intercept == "random"), 
       aes(x = access, y = deaths_mean/pop*1e5, 
           color = interaction(scale, data_source), shape = ctar_bump)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(pop_predict ~ covar_name, scales = "free_x", drop = TRUE) +
  expand_limits(y = 0) +
  xlab("Access metric") +
  ylab("Predicted incidence of deaths") +
  labs(tag = "B") 

grouped_deaths %>%
  group_by(data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(bites_mean:averted_lower05, pop), sum, na.rm = TRUE) -> national_deaths

## Need to do fixed bite ests part (need to think about how to plot this with random effects)
## i.e. covar vs. bite incidence for a range of access etc.

## Need to do min and max reporting with fixed p_rabid + rho_max + rabid bite incidence
## (given access what are the reporting estimates)
# ------------------------------------------------------------------------------------------------ #
# Run predictions from all candidate models
# Details: Models include travel times and distance as access metrics, in addition to population 
# Author: Malavika Rajeev 
# ------------------------------------------------------------------------------------------------ #

# Set-up --------------------------------------------------------------------------------------
library(foreach)
library(data.table)
library(iterators)
library(tidyverse)
library(glue)
select <- dplyr::select

# source scripts
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")

# source bite ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")


# Estimates
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means

# Observed vs. Predicted for all models -------------------------------------------------------
preds_mada <- 
  foreach(i = iter(model_means, by = "row"), 
          j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(model_means)*100)
    if(i$data_source == "Moramanga") {
      covar_df <- mora_bites
      covar_df$names <- mora_bites$commcode
    } else {
      if(i$scale == "District") {
        covar_df <- district_bites
        covar_df$names <- district_bites$distcode
      } else {
        covar_df <- comm_covars
        covar_df$names <- comm_covars$commcode
      }
    }
    
    # Also transform covar
    covar_df$ttimes <- covar_df$ttimes_wtd/60
    
    if(i$intercept == "random") {
      known_alphas <-  as.numeric(i[, match(glue("alpha[{covar_df$catch}]"), colnames(i))])
    } else {
      known_alphas <- rep(NA, nrow(covar_df))
    }
    
    bite_mat <- predict.bites(ttimes = covar_df$ttimes, pop = covar_df$pop, 
                              catch = covar_df$pop, names = covar_df$distcode,
                              beta_ttimes = i$beta_ttimes, beta_0 = i$beta_0, 
                              beta_pop = i$beta_pop, sigma_0 = i$sigma_0, 
                              known_alphas = known_alphas, pop_predict = i$pop_predict, 
                              intercept = i$intercept, trans = 1e5, known_catch = TRUE, 
                              nsims = 1000)
    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
    data.table(names = covar_df$names, group_names = covar_df$distcode,
               ttimes = covar_df$ttimes, pop = covar_df$pop, 
               catch = covar_df$catch, mean_bites = mean, sd_bites = sd, 
               pop_predict = i$pop_predict, intercept = i$intercept, scale = i$scale, 
               data_source = i$data_source)
          }

preds_mada %>%
  filter(data_source != "Moramanga") %>%
  group_by(group_names, data_source, scale, pop_predict, intercept) %>% 
  summarize_at(vars(contains("bites")), sum) %>%
  left_join(district_bites, by = c("group_names" = "distcode")) -> preds_mada_grouped
            
preds_mada %>%
  filter(data_source == "Moramanga") %>%
  select(names, group_names, data_source, scale, pop_predict, intercept, mean_bites, 
         sd_bites) %>%
  left_join(mora_bites,
            by = c("names" = "commcode")) -> preds_mora_grouped
preds_grouped <- bind_rows(preds_mora_grouped, preds_mada_grouped)
write.csv(preds_grouped, "output/preds/bites/fitted_grouped_all.csv", row.names = FALSE)
write.csv(preds_mada, "output/preds/bites/fitted_ungrouped_all.csv", row.names = FALSE)

# Out of fit ----------------------------------------------------------------------------------
# Use commune and district models to predict Moramanga data
model_means %>%
  filter(data_source == "National") -> mada_means

outfit_mora <- 
  foreach(i = iter(mada_means, by = "row"), j = icount(), 
          .combine = "rbind") %do% {
    
    print(j/nrow(mada_means)*100)

    if(i$intercept == "random") {
      known_alphas <-  as.numeric(i[, match(glue("alpha[{mora_bites$catch}]"), colnames(i))])
    } else {
      known_alphas <- rep(NA, nrow(mora_bites))
    }
    
    bite_mat <- predict.bites(ttimes = mora_bites$ttimes_wtd/60, pop = mora_bites$pop, 
                              catch = mora_bites$catch, names = mora_bites$commcode,
                              beta_ttimes = i$beta_ttimes, beta_0 = i$beta_0, 
                              beta_pop = i$beta_pop, sigma_0 = i$sigma_0, 
                              known_alphas = known_alphas, pop_predict = i$pop_predict, 
                              intercept = i$intercept, trans = 1e5, known_catch = TRUE, 
                              nsims = 1000)
    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
    data.table(names = mora_bites$commcode, 
               ttimes = mora_bites$ttimes_wtd/60, pop = mora_bites$pop, 
               catch = mora_bites$catch, mean_bites = mean, sd_bites = sd, 
               pop_predict = i$pop_predict, intercept = i$intercept, scale = i$scale, 
               data_source = i$data_source, type = "mora_outfit", observed = mora_bites$avg_bites)
  }

write.csv(outfit_mora, "output/preds/bites/outfit_mora.csv", row.names = FALSE)


# Use Moramanga model to predict district and commune model
model_means %>%
  filter(data_source == "Moramanga") -> mora_means
scale <- c("Commune", "District")

outfit_mada <- 
  foreach(k = 1:length(scale), .combine = "rbind") %:%
  foreach(i = iter(mora_means, by = "row"), j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(mora_means)*100)
    # i <- as.data.frame(mora_means[2, ])
    
    if(scale[k] == "District") {
      covar_df <- district_bites
      covar_df$names <- district_bites$distcode
    } else {
      covar_df <- comm_covars
      covar_df$names <- comm_covars$commcode
    }
    
    if(i$intercept == "random") {
      known_alphas <-  as.numeric(i[, match(glue("alpha[{covar_df$catch}]"), colnames(i))])
    } else {
      known_alphas <- rep(NA, nrow(covar_df))
    }
    
    bite_mat <- predict.bites(ttimes = covar_df$ttimes_wtd/60, pop = covar_df$pop, 
                              catch = covar_df$catch, names = covar_df$names,
                              beta_ttimes = i$beta_ttimes, beta_0 = i$beta_0, 
                              beta_pop = i$beta_pop, sigma_0 = i$sigma_0, 
                              known_alphas = known_alphas, pop_predict = i$pop_predict, 
                              intercept = i$intercept, trans = 1e5, known_catch = TRUE, 
                              nsims = 1000)
    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
    data.table(names = covar_df$names, group_names = covar_df$distcode,
               ttimes = covar_df$ttimes_wtd/60, pop = covar_df$pop, 
               catch = covar_df$catch, mean_bites = mean, sd_bites = sd, 
               pop_predict = i$pop_predict, intercept = i$intercept, scale = scale[k], 
               data_source = "National", type = "mada_outfit")
  }

# Left join with observed
outfit_mada %>%
  group_by(group_names, data_source, scale, pop_predict, intercept) %>% 
  summarize_at(vars(contains("bites")), sum, na.rm = TRUE) %>%
  left_join(district_bites, by = c("group_names" = "distcode")) -> outfit_grouped
write.csv(outfit_grouped, "output/preds/bites/outfit_grouped_mada.csv", row.names = FALSE)

# Session Info
out.session(path = "R/03_bitemodels/02_get_modpreds.R", filename = "output/log_local.csv")

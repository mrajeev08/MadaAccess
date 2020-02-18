# ------------------------------------------------------------------------------------------------ #
#' Scaling of incidence and how this might drive burden                          
# ------------------------------------------------------------------------------------------------ #

# libraries
library(rgdal)
library(data.table)
library(tidyverse)
library(foreach)
library(iterators)
library(triangle)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")

# Scaling factors ----------------------------------------------------------------------------
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

## Communes
pop <- mada_communes$pop
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 110/1e5
incidence_min <- 10/1e5
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ pop) ## use these and constrain
neg <- lm(neg_scale ~ pop) ## use these and constrain
neg_comm <- data.table(scaling = "neg", sfactor = neg$coefficients[2], intercept = incidence_max,
                       scale = "Commune", type = "---")
pos_comm <- data.table(scaling = "pos", sfactor = pos$coefficients[2], intercept = incidence_min,
                       scale = "Commune", type = "+++")

## Districts
pop <- mada_districts$pop
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ pop) ## use these and constrain
neg <- lm(neg_scale ~ pop) ## use these and constrain
neg_dist <- data.table(scaling = "neg", sfactor = neg$coefficients[2], intercept = incidence_max, 
                       scale = "District", type = "---")
pos_dist <- data.table(scaling = "pos", sfactor = pos$coefficients[2], intercept = incidence_min,
                       scale = "District", type = "+++")
scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)

# Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "random") -> model_means

# Burden predictions ---------------------------------------------------------------------

# Filter to only do ones for which travel times have changed
commune_master <- fread("output/ttimes/commune_maxcatch.gz")
setorder(commune_master, commcode, scenario)
commune_master[, diff_comm := ttimes_wtd - shift(ttimes_wtd, 1), by = commcode]
commune_master[, diff_dist := ttimes_wtd_dist - shift(ttimes_wtd_dist, 1), by = commcode]
comm_run <- commune_master[diff_comm != 0 | scenario %in% c(0, 1648)]
dist_run <- commune_master[diff_dist != 0 | scenario %in% c(0, 1648)]
rm(commune_master) # cleaning up memory!
gc()

params <- model_means[model_means$scale == "Commune", ]
bite_mat_comm <- predict.bites(ttimes = comm_run$ttimes_wtd/60, pop = comm_run$pop, 
                               catch = comm_run$catchment, names = comm_run$commcode, 
                               beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                               beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                               pop_predict = params$pop_predict, intercept = params$intercept, 
                               trans = 1e5, known_catch = FALSE, nsims = 1000)

params <- model_means[model_means$scale == "District", ]
bite_mat_dist <- predict.bites(ttimes = dist_run$ttimes_wtd_dist/60, pop = dist_run$pop, 
                               catch = dist_run$catchment, names = dist_run$commcode, 
                               beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                               beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                               pop_predict = params$pop_predict, intercept = params$intercept, 
                               trans = 1e5, known_catch = FALSE, nsims = 1000)

# Burden predictions --------------------------------------------------------------------------
multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(j = iter(scaling_df, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach', 'triangle', 'foreach', 'dplyr','tidyr')) %do% {
          
          print(j)
          if(j$scale == "Commune"){
            admin <- comm_run # these are data.tables so hopefully will not copy only point!
            bite_mat <- bite_mat_comm 
            ttimes <- admin$ttimes_wtd/60
          }
          
          if(j$scale == "District"){
            admin <- dist_run
            bite_mat <- bite_mat_dist
            ttimes <- admin$ttimes_wtd_dist/60
          }
          
          inc_scaled <- constrained_inc(slope = j$sfactor, intercept = j$intercept, pop = admin$pop, 
                                        max = 10/1e5, min = 110/1e5)

          all_mats <-  predict.deaths(bite_mat, pop = admin$pop,
                                      p_rab_min = 0.2, p_rab_max = 0.6,
                                      rho_max = 0.98, exp_scaled = inc_scaled,
                                      prob_death = 0.16, dist = "scaled")
          
          all_mats <- c(list(bites = bite_mat), all_mats)
          
          foreach(i = 1:length(all_mats), .combine = 'cbind') %do% {
            mat <- all_mats[[i]]
            labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
            mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
            sd <- apply(mat, 1, sd, na.rm = TRUE)
            upper <- mean + 1.96*sd/sqrt(ncol(mat))
            lower <- mean - 1.96*sd/sqrt(ncol(mat))
            out <- data.table(mean, upper, lower)
            names(out) <- labels
            out
          } -> admin_comm
          
          admin_comm <- data.table(names = admin$commcode, scenario = admin$scenario,
                                   ttimes = ttimes, pop = admin$pop, 
                                   catch = admin$catchment, j, 
                                   admin_comm)
          
          max_clinics <- max(admin$scenario[admin$scenario != 1648])
          
          admin_base <- admin_comm[scenario == 0] 
          
          admin_comm %>%
            complete(scenario = 1:max_clinics, names) %>% # from 1 to last
            group_by(names) %>%
            arrange(scenario) %>%
            fill(3:ncol(admin_comm), .direction = "down") -> admin_comm
          
          ## This should be true!
          admin_comm %>%
            group_by(scenario) %>%
            summarize(n_admin = n()) %>%
            summarize(all(n_admin == 1579)) # should be true for all of them!
          
          admin_comm %>%
            group_by(scenario) %>%
            summarize_at(vars(bites_mean:averted_lower, pop), sum, na.rm = TRUE) -> natl_preds
          
          natl_preds <- data.table(natl_preds, j)
          
          list(base = admin_base, natl = natl_preds)
          
    } -> burden_scaled_se

# Write out data
fwrite(burden_scaled_se[["base"]], "output/sensitivity/burden_baseline_scaled.gz") 
fwrite(burden_scaled_se[["natl"]], "output/sensitivity/burden_addclinics_scaled.gz") 

# Close out
file_path <- "R/06_sensitivity/04_scaling_se.R"
out.session(path = file_path, filename = "output/log_local.csv")


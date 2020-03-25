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

# Communes
pop <- mada_communes$pop - min(mada_communes$pop)
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 110/1e5
incidence_min <- 15/1e5
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
# pos <- lm(pos_scale ~ pop) # use these and constrain
# neg <- lm(neg_scale ~ pop) # use these and constrain
pos <- lm(pos_scale ~ 0 + pop, offset=rep(10/1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset=rep(110/1e5, length(pop)))
neg_comm <- data.table(scaling = "neg", sfactor = neg$coefficients[1], intercept = 110/1e5,
                       scale = "Commune", type = "---")
pos_comm <- data.table(scaling = "pos", sfactor = pos$coefficients[1], intercept = 10/1e5,
                       scale = "Commune", type = "+++")

# Districts
pop <- mada_districts$pop - min(mada_districts$pop)
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
# pos <- lm(pos_scale ~ pop) ## use these and constrain
# neg <- lm(neg_scale ~ pop) ## use these and constrain
pos <- lm(pos_scale ~ 0 + pop, offset=rep(10/1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset=rep(110/1e5, length(pop)))
neg_dist <- data.table(scaling = "neg", sfactor = neg$coefficients[1], intercept = 110/1e5, 
                       scale = "District", type = "---")
pos_dist <- data.table(scaling = "pos", sfactor = pos$coefficients[1], intercept = 10/1e5,
                       scale = "District", type = "+++")
scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)
write.csv(scaling_df, "output/sensitivity/scaling.csv", row.names = FALSE)

# Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates_adj_OD.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_means
model_means <- bind_rows(filter(model_means, data_source == "National", scale == "District",
                                intercept == "random"), 
                         filter(model_means, data_source == "Moramanga"))
model_ests %>%
  select(params, sd_adj, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = sd_adj, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_sds_adj
model_ests %>%
  select(params, SD, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = SD, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_sds_unadj
model_sds <- bind_rows(filter(model_sds_unadj, data_source == "National", scale == "District",
                              intercept == "random"), filter(model_sds_adj, 
                                                             data_source == "Moramanga"))

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

# With adjustment for overdispersion
par <- model_means[model_means$scale == "Commune", ]
sds <- model_sds[model_sds$scale == "Commune", ]
bite_mat_comm <- predict.bites(ttimes = comm_run$ttimes_wtd/60, pop = comm_run$pop, 
                               catch = comm_run$catchment, names = comm_run$commcode, 
                               beta_ttimes = par$beta_ttimes, beta_ttimes_sd = sds$beta_ttimes,
                               beta_0 = par$beta_0, beta_0_sd = sds$beta_0, 
                               beta_pop = 0, beta_pop_sd = sds$beta_pop, 
                               sigma_0 = par$sigma_0, known_alphas = NA, 
                               pop_predict = "flatPop", intercept = par$intercept, dist = TRUE,
                               trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")

# With district catchment effect
par <- model_means[model_means$scale == "District", ]
bite_mat_dist <- predict.bites(ttimes = dist_run$ttimes_wtd_dist/60, pop = dist_run$pop, 
                               catch = dist_run$catchment, names = dist_run$commcode, 
                               beta_ttimes = par$beta_ttimes, beta_0 = par$beta_0, 
                               beta_pop = 0, sigma_0 = par$sigma_0, known_alphas = NA, dist = FALSE,
                               pop_predict = par$pop_predict, intercept = par$intercept, 
                               trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")

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
            data_source <- "Moramanga" # since comparing Mora commune with National
          }
          
          if(j$scale == "District"){
            admin <- dist_run
            bite_mat <- bite_mat_dist
            ttimes <- admin$ttimes_wtd_dist/60
            data_source <- "National" # since comparing Mora commune with National
          }
          
          inc_scaled <- constrained_inc(slope = j$sfactor, intercept = j$intercept, 
                                        pop = admin$pop - min(admin$pop), 
                                        max = 110/1e5, min = 15/1e5)
          all_mats <-  predict.deaths(bite_mat, pop = admin$pop,
                                      p_rab_min = 0.2, p_rab_max = 0.6,
                                      rho_max = 0.98, exp_scaled = inc_scaled,
                                      prob_death = 0.16, dist = "triangle", inc = "scaled")
          
          all_mats <- c(list(bites = bite_mat), all_mats)
          
          foreach(i = 1:length(all_mats), .combine = 'cbind') %do% {
            mat <- all_mats[[i]]
            labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
            mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
            sd <- apply(mat, 1, sd, na.rm = TRUE)
            upper <- apply(mat, 1, quantile, 0.975)
            lower <- apply(mat, 1, quantile, 0.025)
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
          admin_base$data_source <- data_source
          
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
          
          natl_preds <- data.table(natl_preds, j, data_source)
          
          list(base = admin_base, natl = natl_preds)
          
    } -> burden_scaled_se

# Write out data
fwrite(burden_scaled_se[["base"]], "output/sensitivity/burden_baseline_scaled.gz") 
fwrite(burden_scaled_se[["natl"]], "output/sensitivity/burden_addclinics_scaled.gz") 

# Close out
file_path <- "R/06_sensitivity/04_scaling_se.R"
out.session(path = file_path, filename = "output/log_local.csv")


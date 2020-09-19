# ------------------------------------------------------------------------------
#' Run predictions from all candidate models to data
#' Models include travel times and distance as access metrics,
#' in addition to population
# ------------------------------------------------------------------------------

start <- Sys.time()

# Set-up -----------------------------------------------------------------------
library(data.table)
library(iterators)
library(tidyverse)
library(glue)
library(foreach)
select <- dplyr::select

# source scripts
source(here::here("R", "utils.R"))
source(here_safe("R/predict_functions.R"))

# source bite and model ests
district_bites <- fread(here_safe("analysis/out/bites/district_bites.csv"))
comm_covars <- fread(here_safe("analysis/out/bites/comm_covars.csv"))
mora_bites <- fread(here_safe("analysis/out/bites/mora_bites.csv"))
model_ests <- read.csv(here_safe("analysis/out/mods/estimates.csv"))

# Check convergence ------------------------------------------------------------
model_ests %>%
  select(data_source, scale, pop_predict, intercept, OD, val = psrf_upper) %>%
  mutate(type = "psrf_upper") -> psrf
model_ests %>%
  select(data_source, scale, pop_predict, intercept, OD, val = mpsrf) %>%
  mutate(type = "mpsrf") -> mpsrf
convergence <- bind_rows(mpsrf, psrf)
write_create(convergence,
             "analysis/out/stats/convergence.csv",
             write.csv,
             row.names = FALSE)

# Observed vs. Predicted for all models ----------------------------------------
# filter to unique combinations
model_ests %>%
  select(data_source, scale, pop_predict, intercept, OD) %>%
  distinct() -> mods

preds_mada <-
  foreach(pars = iter(mods, by = "row"), .combine = rbind) %do% {
    if (pars$data_source == "Moramanga") {
      covar_df <- mora_bites
      covar_df$names <- mora_bites$commcode
    } else {
      if (pars$scale == "District") {
        covar_df <- district_bites
        covar_df$names <- district_bites$distcode
      } else {
        covar_df <- comm_covars
        covar_df$names <- comm_covars$commcode
      }
    }

    # Also transform covar
    covar_df$ttimes <- covar_df$ttimes_wtd / 60


    posts <- as.data.frame(get_samps(
      pop_predict = pars$pop_predict,
      data_source = pars$data_source,
      intercept = pars$intercept,
      scale = pars$scale, suff = ifelse(pars$OD == TRUE, "_OD", ""),
      parent_dir = "analysis/out/mods/samps/", nsims = 1000
    ))

    if (pars$intercept == "random") {
      known_alphas <- posts[, grep("alpha", colnames(posts))] # should be ordered by catch #
    } else {
      known_alphas <- matrix(NA,
        nrow = nrow(covar_df),
        ncol = length(unique(covar_df$catch))
      )
    }

    bite_mat <- predict_bites(
      ttimes = covar_df$ttimes, pop = covar_df$pop,
      catch = covar_df$catch, names = covar_df$distcode,
      beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0,
      beta_pop = posts$beta_pop, sigma_0 = posts$sigma_0,
      sigma_e = posts$sigma_e, known_alphas = known_alphas,
      pop_predict = pars$pop_predict, OD = pars$OD,
      intercept = pars$intercept, trans = 1e5, known_catch = TRUE,
      par_type = "posterior", pred_type = "bites", nsims = 1000
    )

    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    upper <- apply(bite_mat, 1, quantile, prob = 0.975)
    lower <- apply(bite_mat, 1, quantile, prob = 0.025)
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
    data.table(
      names = covar_df$names, group_names = covar_df$distcode,
      ttimes = covar_df$ttimes, pop = covar_df$pop,
      catch = covar_df$catch, mean_bites = mean, sd_bites = sd,
      bites_upper = upper, bites_lower = lower,
      pop_predict = pars$pop_predict, intercept = pars$intercept, scale = pars$scale,
      data_source = pars$data_source, OD = pars$OD
    )
  }

preds_mada %>%
  filter(data_source != "Moramanga") %>%
  group_by(group_names, data_source, scale, pop_predict, intercept, OD) %>%
  summarize_at(vars(contains("bites")), sum) %>%
  left_join(district_bites, by = c("group_names" = "distcode")) -> preds_mada_grouped

preds_mada %>%
  filter(data_source == "Moramanga") %>%
  select(
    names, group_names, data_source, scale, OD, pop_predict, intercept, mean_bites,
    bites_upper, bites_lower, sd_bites
  ) %>%
  left_join(mora_bites,
    by = c("names" = "commcode")
  ) -> preds_mora_grouped

preds_grouped <- bind_rows(preds_mora_grouped, preds_mada_grouped)
write_create(preds_grouped, "analysis/out/mods/preds/fitted_grouped_all.csv",
             write.csv,
             row.names = FALSE)
write_create(preds_mada, "analysis/out/mods/preds/fitted_ungrouped_all.csv",
             write.csv,
             row.names = FALSE)

# Out of fit -------------------------------------------------------------------

# Use commune and district models to predict Moramanga data
natl_mods <- filter(mods, data_source == "National")

outfit_mora <-
  foreach(pars = iter(natl_mods, by = "row"), .combine = rbind) %do% {


    # Also transform covar
    mora_bites$ttimes <- mora_bites$ttimes_wtd / 60


    posts <- as.data.frame(get_samps(
      pop_predict = pars$pop_predict,
      data_source = pars$data_source,
      intercept = pars$intercept,
      scale = pars$scale, suff = ifelse(pars$OD == TRUE, "_OD", ""),
      parent_dir = "analysis/out/mods/samps/", nsims = 1000
    ))

    if (pars$intercept == "random") {
      known_alphas <- posts[, grep("alpha", colnames(posts))] # should be ordered by catch #
    } else {
      known_alphas <- matrix(NA,
        nrow = nrow(mora_bites),
        ncol = length(unique(mora_bites$catch))
      )
    }

    bite_mat <- predict_bites(
      ttimes = mora_bites$ttimes, pop = mora_bites$pop,
      catch = mora_bites$catch, names = mora_bites$distcode,
      beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0,
      beta_pop = posts$beta_pop, sigma_0 = posts$sigma_0,
      sigma_e = posts$sigma_e, known_alphas = known_alphas,
      pop_predict = pars$pop_predict, OD = pars$OD,
      intercept = pars$intercept, trans = 1e5, known_catch = TRUE,
      par_type = "posterior", pred_type = "bites", nsims = 1000
    )

    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    upper <- apply(bite_mat, 1, quantile, prob = 0.975)
    lower <- apply(bite_mat, 1, quantile, prob = 0.025)
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
    data.table(
      names = mora_bites$names, group_names = mora_bites$distcode,
      ttimes = mora_bites$ttimes, pop = mora_bites$pop,
      catch = mora_bites$catch, mean_bites = mean, sd_bites = sd,
      bites_upper = upper, bites_lower = lower,
      pop_predict = pars$pop_predict, intercept = pars$intercept,
      scale = pars$scale,
      data_source = pars$data_source, OD = pars$OD, type = "mora_outfit",
      observed = mora_bites$avg_bites
    )
  }

write_create(outfit_mora,
             "analysis/out/mods/preds/outfit_mora.csv",
             write.csv,
             row.names = FALSE)

# Use Moramanga model to predict district and commune model
mora_mods <- filter(mods, data_source == "Moramanga")
scale <- c("Commune", "District")
outfit_mada <-
  foreach(k = 1:length(scale), .combine = "rbind") %:%
  foreach(pars = iter(mora_mods, by = "row"), .combine = rbind) %do% {
    if (scale[k] == "District") {
      covar_df <- district_bites
      covar_df$names <- district_bites$distcode
    } else {
      covar_df <- comm_covars
      covar_df$names <- comm_covars$commcode
    }

    covar_df$ttimes <- covar_df$ttimes_wtd / 60

    posts <- as.data.frame(get_samps(
      pop_predict = pars$pop_predict,
      data_source = pars$data_source,
      intercept = pars$intercept,
      scale = pars$scale, suff = ifelse(pars$OD == TRUE, "_OD", ""),
      parent_dir = "analysis/out/mods/samps/", nsims = 1000
    ))

    known_alphas <- rep(NA, nrow(covar_df))

    bite_mat <- predict_bites(
      ttimes = covar_df$ttimes, pop = covar_df$pop,
      catch = covar_df$catch, names = covar_df$distcode,
      beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0,
      beta_pop = posts$beta_pop, sigma_0 = posts$sigma_0,
      sigma_e = posts$sigma_e, known_alphas = known_alphas,
      pop_predict = pars$pop_predict, OD = pars$OD,
      intercept = pars$intercept, trans = 1e5, known_catch = TRUE,
      par_type = "posterior", pred_type = "bites", nsims = 1000
    )

    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    upper <- apply(bite_mat, 1, quantile, prob = 0.975, na.rm = TRUE)
    lower <- apply(bite_mat, 1, quantile, prob = 0.025, na.rm = TRUE)
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)

    data.table(
      names = covar_df$names, group_names = covar_df$distcode,
      ttimes = covar_df$ttimes, pop = covar_df$pop,
      catch = covar_df$catch, mean_bites = mean, sd_bites = sd,
      bites_upper = upper, bites_lower = lower,
      pop_predict = pars$pop_predict, intercept = pars$intercept,
      OD = pars$OD, data_source = "National", type = "mada_outfit",
      scale = scale[k]
    )
  }

# Left join with observed
outfit_mada %>%
  group_by(group_names, data_source, scale, pop_predict, intercept, OD) %>%
  summarize_at(vars(contains("bites")), sum, na.rm = TRUE) %>%
  left_join(district_bites, by = c("group_names" = "distcode")) -> outfit_grouped

write_create(outfit_grouped,
             "analysis/out/mods/preds/outfit_grouped_mada.csv",
             write.csv,
             row.names = FALSE)

# Get expectations given a range of travel times and pop of 1e5 ----------------
# for flat pop models only
mods_flatPop <- filter(mods, pop_predict == "flatPop")
ttimes_plot <- seq(0, 15, by = 0.025)

expectations <-
  foreach(pars = iter(mods_flatPop, by = "row"), .combine = rbind) %do% {
    posts <- as.data.frame(get_samps(
      pop_predict = pars$pop_predict,
      data_source = pars$data_source,
      intercept = pars$intercept,
      scale = pars$scale, suff = ifelse(pars$OD == TRUE, "_OD", ""),
      parent_dir = "analysis/out/mods/samps/", nsims = 10000
    ))

    # at these travel times what are the expected ranges of bites predicted
    bite_mat <- predict_bites(
      ttimes = ttimes_plot, pop = 1e5,
      catch = 1, names = NA,
      beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0,
      beta_pop = posts$beta_pop, sigma_0 = posts$sigma_0,
      sigma_e = posts$sigma_e, known_alphas = NA,
      pop_predict = pars$pop_predict, OD = pars$OD,
      intercept = pars$intercept, trans = 1e5, known_catch = FALSE,
      par_type = "posterior", pred_type = "bites",
      nsims = 10000, error_n = 1
    )

    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    upper <- apply(bite_mat, 1, quantile, prob = 0.975, na.rm = TRUE)
    lower <- apply(bite_mat, 1, quantile, prob = 0.025, na.rm = TRUE)
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)

    data.table(
      ttimes = ttimes_plot, pop = 1e5, mean_bites = mean, sd_bites = sd,
      bites_upper = upper, bites_lower = lower,
      pop_predict = pars$pop_predict, intercept = pars$intercept, scale = pars$scale,
      data_source = pars$data_source, OD = pars$OD
    )

  }

write_create(expectations,
             "analysis/out/mods/preds/expectations.csv",
             write.csv,
             row.names = FALSE)

# Session Info
out_session(logfile = "logs/log_local.csv", start = start, ncores = 1)

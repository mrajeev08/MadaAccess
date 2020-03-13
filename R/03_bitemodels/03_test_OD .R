# ------------------------------------------------------------------------------------------------ #
#' Test for overdispersion for best fitting models
# ------------------------------------------------------------------------------------------------ #

# Set-up 
library(foreach)
library(data.table)
library(iterators)
library(tidyverse)
library(glue)
select <- dplyr::select

# source scripts
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")

# source bite and model ests
outliers <- c("MG31310", "MG32318")
district_bites <- fread("output/bites/district_bites.csv")
district_bites <- district_bites[!(distcode %in% outliers)]
comm_covars <- fread("output/bites/comm_covars.csv")
comm_covars <- comm_covars[!(distcode %in% outliers)]
mora_bites <- fread("output/bites/mora_bites.csv")
model_ests <- read.csv("output/mods/estimates.csv")

# Get mod mean param ests, degrees of freedom -----------------------------------------------
model_ests %>%
  select(params, Mean, pop_predict, intercept, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means


model_ests %>%
  group_by(pop_predict, intercept, scale, data_source) %>%
  summarize(df = n()) -> model_dfs

# Check for overdispersion --------------------------------------------------------------------

# first get expectation 
exp_bites <- 
  foreach(i = iter(model_means, by = "row"), .combine = rbind) %do% {
    
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
    
    exp_bites <- predict.bites.fixed(ttimes = covar_df$ttimes, pop = covar_df$pop, 
                                     catch = covar_df$pop, names = covar_df$distcode,
                                     beta_ttimes = i$beta_ttimes, beta_0 = i$beta_0, 
                                     beta_pop = i$beta_pop, 
                                     known_alphas = known_alphas, pop_predict = i$pop_predict, 
                                     intercept = i$intercept, trans = 1e5)
    
    data.table(names = covar_df$names, group_names = covar_df$distcode,
               ttimes = covar_df$ttimes, pop = covar_df$pop, 
               catch = covar_df$catch, exp_bites = exp_bites,
               pop_predict = i$pop_predict, intercept = i$intercept, scale = i$scale, 
               data_source = i$data_source)
  }

exp_bites %>%
  filter(data_source != "Moramanga") %>%
  group_by(group_names, data_source, scale, pop_predict, intercept) %>% 
  summarize_at(vars(exp_bites), sum) %>%
  left_join(district_bites, by = c("group_names" = "distcode")) -> exp_preds_mada

exp_bites %>%
  filter(data_source == "Moramanga") %>%
  select(names, group_names, data_source, scale, pop_predict, intercept, exp_bites) %>%
  left_join(mora_bites,
            by = c("names" = "commcode")) -> exp_preds_mora
exp_preds <- bind_rows(exp_preds_mada, exp_preds_mora)

# get sum of squared errors and adjust the sds by the sqrt of the od factor
exp_preds %>%
  left_join(model_dfs) %>%
  mutate(z = (exp_bites - avg_bites)/sqrt(exp_bites),
         nobs = case_when(data_source %in% "National" ~ nrow(district_bites),
                          data_source %in% "Moramanga" ~ nrow(mora_bites))) %>%
  group_by(data_source, scale, pop_predict, intercept) %>%
  summarize(od = sum(z^2)/(nobs[1] - df[1])) -> model_ods

model_ests %>%
  filter(OD == FALSE) %>%
  left_join(model_ods) %>%
  mutate(sd_adj = SD*sqrt(od)) -> model_ests_adj

write.csv(model_ests_adj, "output/mods/estimates_adj_OD.csv")


# Predictions given OD in params --------------------------------------------------------------
ttimes_plot <- seq(0, 15, by = 0.05)

model_ests_adj %>%
  select(params, Mean, pop_predict, intercept, data_source,
         scale) %>%
  filter()
  spread(key = params, value = Mean, fill = 0) %>%
  mutate(n = case_when(data_source == "Moramanga" ~ 61, 
                       data_source == "National" ~ 82)) -> model_means

model_ests_adj %>%
  select(params, sd_adj, pop_predict, intercept, data_source,
         scale) %>%
  spread(key = params, value = SD, fill = 0) %>%
  mutate(n = case_when(data_source == "Moramanga" ~ 61, 
                       data_source == "National" ~ 82)) -> model_SDs

# Session Info
out.session(path = "R/03_bitemodels/03_test_OD.R", filename = "output/log_local.csv")


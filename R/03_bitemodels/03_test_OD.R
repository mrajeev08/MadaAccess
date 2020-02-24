# ------------------------------------------------------------------------------------------------ #
#' Test for overdispersion for best fitting models                                                                            
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

# source bite and model ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")
model_ests <- read.csv("output/mods/estimates.csv")


# Filter to mods w/ no OD and check convergence -----------------------------------------------
model_ests %>%
  filter(OD == TRUE) %>%
  select(params, Mean, pop_predict, intercept, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means


model_ests %>%
  filter(OD == TRUE) %>%
  group_by(pop_predict, intercept, scale, data_source) %>%
  summarize(df = n()) -> model_dfs

# Check convergence
model_ests %>%
  filter(OD == TRUE) %>%
  select(data_source, scale, pop_predict, intercept, val = psrf_upper) %>%
  mutate(type = "psrf_upper") -> psrf
model_ests %>%
  filter(OD == TRUE) %>%
  select(data_source, scale, pop_predict, intercept, val = mpsrf) %>%
  mutate(type = "mpsrf") -> mpsrf
convergence_noOD<- bind_rows(mpsrf, psrf)
write.csv(convergence_noOD, "output/stats/convergence_noOD.csv")
max(convergence_noOD$val) # all converged



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

# get sum of squared errors and compare to the expected variance
exp_preds %>%
  left_join(model_dfs) %>%
  mutate(z = (exp_bites - avg_bites)/sqrt(exp_bites),
         nobs = case_when(data_source %in% "National" ~ nrow(district_bites),
                          data_source %in% "Moramanga" ~ nrow(mora_bites))) %>%
  group_by(data_source, scale, pop_predict, intercept) %>%
  summarize(od = sqrt(sum(z^2)/(nobs[1] - df[1]))) -> model_ods

model_ests %>%
  left_join(model_ods) %>%
  mutate(sd_adj = SD*od) -> model_ests_adj

ggplot(data = filter(model_ests_adj, intercept == "fixed"), aes(x = data_source, y = Mean, 
                                                                color = scale)) +
  geom_pointrange(aes(ymax = Mean + sd_adj, ymin = Mean - sd_adj)) +
  coord_flip() +
  facet_wrap(pop_predict ~ params, scales = "free", ncol = 2)

ggplot(data = filter(model_ests_adj, intercept == "random", !grepl("alpha", params, fixed = TRUE)), 
       aes(x = data_source, y = Mean, color = scale)) +
  geom_pointrange(aes(ymax = Mean + sd_adj, ymin = Mean - sd_adj)) +
  coord_flip() +
  facet_wrap(pop_predict ~ params, scales = "free", ncol = 2)

ggplot(data = exp_preds, aes(x = ttimes_wtd, y = exp_bites/pop*1e5)) + geom_point()

# adjust param sds accordingly and plot
# fixed
# random effect (sep fig for alpha's too!)

# output df of adjusted param estimates

# Make dataframe of how we want to predict (and do them in one simplified loop)

# Filter OD models to those that pass this test -----------------------------------------------

# Get priors and posteriors (ggmcmc) (plot) ---------------------------------------------------

# Get predictions fitted ----------------------------------------------------------------------

# Get predictions out of fit ------------------------------------------------------------------

# Get predictions expected rel with ttimes ----------------------------------------------------


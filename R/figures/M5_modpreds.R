# ------------------------------------------------------------------------------------------------ #
#' Run models of bite incidence using spatial covariates 
#' Models include travel times and distance as access metrics, in addition to population 
# ------------------------------------------------------------------------------------------------ #

# source bite ests
source("R/functions/out.session.R")

# libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)
library(data.table)

# source bite ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")

# Estimates
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(n = case_when(data_source == "Moramanga" ~ nrow(mora_bites), 
                       data_source == "National" ~ nrow(district_bites))) -> model_means

model_ests %>%
  select(params, SD, pop_predict, intercept, data_source,
         scale) %>%
  spread(key = params, value = SD, fill = 0) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(n = case_when(data_source == "Moramanga" ~ nrow(mora_bites), 
                       data_source == "National" ~ nrow(district_bites))) -> model_SDs

# Fig M3.A
# Predictions of bite incidence per 100k
ttimes_plot <- seq(0, 15, by = 0.05)
preds <- foreach(i = 1:nrow(model_means), .combine = "bind_rows") %do% {
  
  mean_ttimes <- model_means$beta_ttimes[i]
  intercept <- model_means$beta_0[i]
  sigma <- model_means$sigma_0[i]
  
  if (model_means$intercept[i] == "fixed") {
    ci_ttimes <- model_SDs$beta_ttimes[i]*1.96 # 95% CI of B_ttimes
    ci_intercept <- model_SDs$beta_0[i]*1.96 # 95% CI of B_0
    upper <- exp((mean_ttimes + ci_ttimes)*ttimes_plot + (intercept + ci_intercept))*1e5
    lower <- exp((mean_ttimes - ci_ttimes)*ttimes_plot + (intercept - ci_intercept))*1e5
  }
    
  if (model_means$intercept[i] == "random") {
    ci_intercept_upper <- intercept + sigma*1.96
    ci_intercept_lower <- intercept - sigma*1.96
    upper <- exp(mean_ttimes*ttimes_plot + ci_intercept_upper)*1e5
    lower <- exp(mean_ttimes*ttimes_plot + ci_intercept_lower)*1e5
  }
  
  preds <- exp(mean_ttimes*ttimes_plot + intercept)*1e5

  as.data.frame(list(preds = preds, upper = upper, lower = lower,
                     ttimes = ttimes_plot, scale = model_means$scale[i], 
                     data_source = model_means$data_source[i],
                     intercept = model_means$intercept[i]))
}

district_bites$source <- "National"
mora_bites$source <- "Moramanga"
observed <- bind_rows(district_bites, mora_bites)

scale_levs <- c("Moramanga.Commune", "National.Commune", "National.District")
scale_labs <- c("Moramanga", "Commune", "District")
model_cols <- c("#F2300F", "#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

modpreds <- ggplot(data = filter(preds, intercept == "random" | data_source == "Moramanga"), 
       aes(x = ttimes, y = preds, color = interaction(data_source, scale))) +
  geom_point(data = observed, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5, 
                                  shape = source), color = "black", alpha = 0.5, size = 1.5,
             stroke = 1.2, inherit.aes = FALSE) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = interaction(data_source, scale)),
              color = NA, alpha = 0.25) +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k") +
  cowplot::theme_minimal_grid()

ggsave("figs/main/modpreds.tiff", modpreds, dpi = 300, height = 4, width = 6)
ggsave("figs/main/modpreds.jpeg", modpreds, height = 4, width = 6)


####################################################################################################
##' Run models of bite incidence using spatial covariates 
##' Details: Models include travel times and distance as access metrics, in addition to population 
##' Author: Malavika Rajeev 
####################################################################################################
## source bite ests
source("R/02_bitedata/03_estimate_biteinc.R") # either source this or output bite data
# probs want to output bite data in order to pull into other things
source("R/functions/predict_bites.R")
source("R/functions/summarize_samps.R")
## libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)

## Estimates
model_ests <- read.csv("output/bitemod_results.csv")
model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(n = case_when(data_source == "Moramanga" ~ 61, 
                       data_source == "National" ~ 82)) -> model_means

model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(n = case_when(data_source == "Moramanga" ~ 61, 
                       data_source == "National" ~ 82)) -> model_SDs

## Fig M3.A 
## Param estimates
samps <- get.samps(parent_dir = "output/samps/", 
                   files = list.files("output/samps", recursive = TRUE))
ggplot(data = filter(samps, pop_predict == "flatPop"), 
       aes(x = interaction(data_source, scale), y = beta_access, fill = intercept)) +
  geom_violin() +
  scale_fill_manual(values = c("turquoise4", "slateblue4"))

ggplot(data = filter(samps, pop_predict == "flatPop"), 
       aes(x = interaction(data_source, scale), y = beta_0, fill = intercept)) +
  geom_violin() +
  scale_fill_manual(values = c("turquoise4", "slateblue4"))

## Fig M3.B
## Predictions of bite incidence per 100k
ttimes_plot <- seq(0, 15, by = 0.05)
preds <- foreach(i = 1:nrow(model_means), .combine = "bind_rows") %do% {
  mean_access <- model_means$beta_access[i]
  ci <- model_SDs$beta_access[i]/sqrt(model_SDs$n[i])
  
  if (model_means$intercept[i] == "random") {
    ci = 0
  }
  
  intercept <- model_means$beta_0[i]
  sigma <- model_means$sigma_0[i]
  
  preds <- exp(mean_access*ttimes_plot + intercept)*1e5
  upper <- exp((mean_access + ci)*ttimes_plot + (intercept + sigma*1.96))*1e5
  lower <- exp((mean_access - ci)*ttimes_plot + (intercept - sigma*1.96))*1e5
  
  as.data.frame(list(preds = preds, upper = upper, lower = lower,
                     ttimes = ttimes_plot, scale = model_means$scale[i], 
                     data_source = model_means$data_source[i],
                     intercept = model_means$intercept[i]))
}

bites_by_ttimes$source <- "National"
morabites_by_ttimes$source <- "Moramanga"
observed <- bind_rows(bites_by_ttimes, morabites_by_ttimes)
scale_labs <- c("Moramanga", "Commune", "District")
names(scale_labs) <- c("Moramanga.Commune", "National.Commune", "National.District")
M3.A <- ggplot(data = filter(preds, intercept == "random" | data_source == "Moramanga"), 
       aes(x = ttimes, y = preds, color = interaction(data_source, scale))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = interaction(data_source, scale)),
              color = NA, alpha = 0.25) +
  scale_color_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale", 
                     labels = scale_labs) +
  scale_fill_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale",
                    labels = scale_labs) +
  labs(x = "Travel times (hrs)", y = "Predicted bites per 100k", tag = "A") +
  theme(text = element_text(size=20))

## Predictions to data: M3B
preds_grouped <- read.csv("output/preds/fitted_grouped_all.csv")
M3.B <- ggplot(data = filter(preds_grouped, 
                             intercept == "random" | data_source == "Moramanga" & pop_predict == "flatPop"), 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = interaction(data_source, scale))) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  scale_color_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale", 
                     labels = scale_labs) +
  expand_limits(y = c(0, max(log(preds_grouped$avg_bites)))) +
  labs(x = "Log(observed bites)", y = "Log(predicted bites)", tag = "B") +
  theme(text = element_text(size=20))

figM3 <- M3.A | M3.B 
ggsave("figs/M3.jpeg", figM3, device = "jpeg", height = 8, width = 12)


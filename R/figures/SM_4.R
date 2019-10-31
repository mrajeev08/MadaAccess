####################################################################################################
##' Figures of models 
##' Details: deets 
##' Author: author 
##' Date started:  date started 
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

## Estimates of access: read in samples and do density plots! 

## Fitted predictions
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

## Out of fit predictions
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
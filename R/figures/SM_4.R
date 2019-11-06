####################################################################################################
##' Figures of models 
##' Details: deets 
##' Author: author 
##' Date started:  date started 
####################################################################################################
## source bite ests
rm(list = ls())
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
write.csv(dic_ranks, "output/model_dic.csv")

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

scale_labs <- c("Moramanga", "Commune", "District")
names(scale_labs) <- c("Moramanga.Commune", "National.Commune", "National.District")

S4.1 <- ggplot(convergence, aes(x = type, y = val, color = interaction(data_source, scale))) + 
  geom_boxplot() +
  scale_color_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale", 
                     labels = scale_labs) +
  ylim(c(0.98, 1.02)) +
  facet_grid(pop_predict ~ intercept, scales = "free_x", drop = TRUE) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey") +
  scale_x_discrete(labels= c("psrf_upper" = "Indiviudal covariate", "mpsrf" = "Multivariate")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 20)) +
  labs(x = "Type", y = "Scale reduction factor")
ggsave("figs/S4.1.jpeg", S4.1, device = "jpeg", height = 10, width = 8)

model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means

## Fitted predictions
preds_grouped <- read.csv("output/preds/fitted_grouped_all.csv")
preds_grouped$mod_intercept <- preds_grouped$intercept
S4.2 <- ggplot(data = filter(preds_grouped), 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = interaction(data_source, scale))) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale", 
                     labels = scale_labs) +
  facet_grid(pop_predict ~ mod_intercept, scales = "free_x", drop = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  xlab("log(Observed bites)") +
  ylab("log(Predicted bites)") +
  theme(text = element_text(size = 20))
ggsave("figs/S4.2.jpeg", S4.2, device = "jpeg", height = 10, width = 8)

## Out of fit predictions: using District and commune models to predict Moramanga data
outfit_mora <- read.csv("output/preds/outfit_mora.csv")
outfit_mora$mod_intercept <- outfit_mora$intercept
S4.3A <- ggplot(data = outfit_mora, 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = interaction(data_source, scale))) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale", 
                     labels = scale_labs) +
  facet_grid(pop_predict ~ mod_intercept, scales = "free_x", drop = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  labs(x = "log(Observed bites) per 100k", y = "log(Predicted bites) per 100k", 
       tag = "A") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 20))

## Out of fit predictions: using Moramanga estimates to predict district data 
## for both commune and district mods
outfit_mada <- read.csv("output/preds/outfit_grouped_mada.csv")
outfit_mada$mod_intercept <- outfit_mada$intercept
S4.3B <- ggplot(data = outfit_mada, 
       aes(x = log(avg_bites + 0.1), y = log(bites_mean + 0.1), 
           color = interaction(data_source, scale))) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("#cc7722", "#004b49", "darkred"), name = "Scale", 
                     labels = scale_labs) +
  facet_grid(pop_predict ~ mod_intercept, scales = "free_x") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  labs(x = "log(Observed bites) per 100k", y = "log(Predicted bites) per 100k", 
       tag = "B") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 20))

S4.3 <- (S4.3A | S4.3B) + plot_layout(widths = c(2, 1))

ggsave("figs/S4.3.jpeg", S4.3, device = "jpeg", height = 8, width = 12)

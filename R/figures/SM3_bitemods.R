####################################################################################################
##' Figures of models 
##' Details: deets 
##' Author: author 
##' Date started:  date started 
####################################################################################################
## source 
rm(list = ls())
source("R/functions/utils.R")

## libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)
library(cowplot)
select <- dplyr::select

## Estimates
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means

## convergence plots
model_ests %>%
  select(data_source, scale, pop_predict, intercept, val = psrf_upper) %>%
  mutate(type = "psrf_upper") -> psrf
model_ests %>%
  select(data_source, scale, pop_predict, intercept, val = mpsrf) %>%
  mutate(type = "mpsrf") -> mpsrf
convergence <- bind_rows(mpsrf, psrf)

scale_levs <- c("Moramanga.Commune", "National.Commune", "National.District")
scale_labs <- c("Moramanga", "Commune", "District")
model_cols <- c("#F2300F", "#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

S3.1 <- ggplot(convergence, aes(x = type, y = val, color = interaction(data_source, scale))) + 
  geom_boxplot() +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  ylim(c(0.98, 1.02)) +
  facet_grid(pop_predict ~ intercept, scales = "free_x", drop = TRUE) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey") +
  scale_x_discrete(labels= c("psrf_upper" = "Indiviudal covariate", "mpsrf" = "Multivariate")) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Type", y = "Scale reduction factor")
ggsave("figs/supplementary/S3.1.jpeg", S3.1, device = "jpeg", height = 10, width = 8)

## Fitted predictions
preds_grouped <- read.csv("output/preds/bites/fitted_grouped_all.csv")
preds_grouped$mod_intercept <- preds_grouped$intercept
S3.2 <- ggplot(data = filter(preds_grouped), 
       aes(x = log(avg_bites + 0.1), y = log(mean_bites + 0.1), 
           color = interaction(data_source, scale))) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  facet_grid(pop_predict ~ mod_intercept, scales = "free_x", drop = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  xlab("log(Observed bites)") +
  ylab("log(Predicted bites)") +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5))
      
ggsave("figs/supplementary/S3.2.jpeg", S3.2, device = "jpeg", height = 10, width = 8)

## Out of fit predictions
outfit_mora <- read.csv("output/preds/bites/outfit_mora.csv")
outfit_mada <- read.csv("output/preds/bites/outfit_grouped_mada.csv")

## Trying nested facet labels
outfit_mada$type <- "National"
outfit_mora$type <- "Moramanga"
outfit_mora$avg_bites <- outfit_mora$observed
outfit_all <- bind_rows(outfit_mada, outfit_mora)
outfit_all$mod_intercept <- factor(outfit_all$intercept)
levels(outfit_all$mod_intercept) <- list("Fixed intercept" = "fixed", "Random intercept" = "random")


type_labs <- c("National" = "underline('National data predicted \n by Moramanga models')", 
  "Moramanga" = "underline('Moramanga data predicted by National models')")

S3.3 <- ggplot(data = outfit_all, 
                aes(x = log(avg_bites + 0.1), y = log(mean_bites + 0.1), 
                    color = interaction(data_source, scale))) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  facet_nested(pop_predict ~ type + mod_intercept, scales = "free",
               labeller = labeller(type = as_labeller(type_labs, label_parsed))) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  labs(x = "log(Observed bites) per 100k", y = "log(Predicted bites) per 100k") +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text.x = element_text(margin = margin(t = 15)))

ggsave("figs/supplementary/S3.3.jpeg", S3.3, device = "jpeg", height = 8, width = 12)

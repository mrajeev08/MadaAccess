# ------------------------------------------------------------------------------------------------ #
#' Candidate models of bite incidence
#' + sensitivity of model selection to correction for undersubmission of forms
# ------------------------------------------------------------------------------------------------ #

# source 
source("R/functions/out.session.R")
source("R/functions/nested_facets.R")

# libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(rgdal)
library(patchwork)
library(cowplot)
library(ggmcmc)
library(data.table)
select <- dplyr::select

# source bite ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")

# correlations between pop + travel times ----------------------------------------------------
comm_covars$scale <- "Commune"
district_bites$scale <- "District"
covars <- bind_rows(district_bites, comm_covars)
covars$scale <- factor(covars$scale, levels = c("District", "Commune"))

S3.1_corrs <- 
  ggplot(data = covars, aes(x = ttimes_wtd/60, y = pop, color = scale)) +
  geom_point() +
  facet_wrap(~scale, scales = "free") +
  labs(x = "Travel times (hrs)", y = "Population") +
  theme_minimal_grid() +
  scale_color_manual(values = c("#35274A", "#0B775E"), guide = "none")

ggsave("figs/supplementary/S3.1_corrs.jpeg", S3.1_corrs, device = "jpeg", height = 5, width = 7)

# candidate models of bite incidence --------------------------------------------------
scale_levs <- c("Moramanga.Commune", "National.Commune", "National.District")
scale_labs <- c("Moramanga", "Commune", "District")
model_cols <- c("#F2300F", "#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

# Estimates
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source, scale, OD) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means

# Fitted predictions
preds_grouped <- read.csv("output/preds/bites/fitted_grouped_all.csv")
preds_grouped$mod_intercept <- preds_grouped$intercept
fitted_preds <- ggplot(data = filter(preds_grouped), 
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
      
ggsave("figs/supplementary/S3.2_fitted.jpeg", fitted_preds, device = "jpeg", height = 10, width = 8)

# Out of fit predictions
outfit_mora <- read.csv("output/preds/bites/outfit_mora.csv")
outfit_mada <- read.csv("output/preds/bites/outfit_grouped_mada.csv")

# Trying nested facet labels
outfit_mada$type <- "National"
outfit_mora$type <- "Moramanga"
outfit_mora$avg_bites <- outfit_mora$observed
outfit_all <- bind_rows(outfit_mada, outfit_mora)
outfit_all$mod_intercept <- factor(outfit_all$intercept)
levels(outfit_all$mod_intercept) <- list("Fixed intercept" = "fixed", "Random intercept" = "random")

type_labs <- c("National" = "underline('National data predicted \n by Moramanga models')", 
  "Moramanga" = "underline('Moramanga data predicted by National models')")

outfit_preds <- ggplot(data = outfit_all, 
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

ggsave("figs/supplementary/S3.3_outfit.jpeg", outfit_preds, device = "jpeg", height = 8, width = 12)

# Comparing expected relationships between ttimes & reported bites ------------------------------
preds <- read.csv("output/preds/bites/expectations.csv") # Expectations
preds <- filter(preds, interaction(data_source, OD) != "Moramanga.FALSE")

# data
district_bites$data_source <- "National"
mora_bites$data_source <- "Moramanga"
observed <- bind_rows(district_bites, mora_bites)
  
modpreds <- ggplot(data = filter(preds, intercept == "random" | OD == TRUE), 
                   aes(x = ttimes_wtd, y = exp_bites, color = interaction(data_source, scale))) +
  geom_ribbon(aes(ymin = exp_bites_lower, ymax = exp_bites_upper, 
                  fill = interaction(data_source, scale)),
              color = NA, alpha = 0.5) +
  geom_point(data = observed, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5, 
                                  shape = data_source), color = "black", alpha = 0.5, size = 1.5,
             stroke = 1.2, inherit.aes = FALSE) +
  geom_line(size = 1.1) +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k") +
  cowplot::theme_minimal_grid() +
  facet_wrap(~OD, 
             labeller = as_labeller(c("FALSE" = "Random intercept", 
                                      "TRUE" = "Fixed intercept \n adjusted for \n overdispersion")))

ggsave("figs/supplementary/S3.4_ODcheck.jpeg", modpreds, device = "jpeg", height = 6, width = 6)

# Sensitivity of model ests to reporting cut-offs ---------------------------------------------
# Sensitivity analyses
model_se <- fread("output/sensitivity/model_se.csv")
bitedata_se <- fread("output/sensitivity/bitedata_se.csv")
preds_se <- fread("output/preds/bites/expectations_se.csv")

# Colors $ labs
scale_levs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 
cutoff_labs <- c("7" = "7 days", "15" = "15 days", "Inf" = "Uncorrected")

# Expected relationships
pred_inc_cutoffs <- ggplot(data = filter(preds_se, intercept == "random"), 
                  aes(x = ttimes_wtd, y = exp_bites, color = scale)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = exp_bites_lower, ymax = exp_bites_upper, fill = scale),
              color = NA, alpha = 0.25) +
  geom_point(data = bitedata_se, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5), 
             color = "black", shape = 1, alpha = 0.5) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  labs(x = "Travel times (hrs)", y = "Predicted bites per 100k") +
  theme(text = element_text(size= 20)) +
  facet_wrap(~ rep_cutoff, nrow = 1, labeller = labeller(rep_cutoff = cutoff_labs)) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("figs/supplementary/S3.5_cutoffs.jpeg", pred_inc_cutoffs, device = "jpeg", height = 6, 
       width = 8)

# Session Info
out.session(path = "R/figures/SM3_bitemods.R", filename = "output/log_local.csv")


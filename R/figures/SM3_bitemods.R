# ------------------------------------------------------------------------------------------------ #
#' Candidate models of bite incidence
#' + sensitivity of model selection to correction for undersubmission of forms
# ------------------------------------------------------------------------------------------------ #

# source 
source("R/functions/out.session.R")
source("R/functions/summarize_samps.R")

# libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)
library(cowplot)
select <- dplyr::select

# Estimates
model_ests <- read.csv("output/mods/estimates_poisOD.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means


# Part I: candidate models of bite incidence --------------------------------------------------

# convergence plots
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

psrfs <- ggplot(convergence, aes(x = type, y = val, color = interaction(data_source, scale))) + 
  geom_boxplot() +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  facet_grid(pop_predict ~ intercept, scales = "free_x", drop = TRUE) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey") +
  scale_x_discrete(labels= c("psrf_upper" = "Indiviudal covariate", "mpsrf" = "Multivariate")) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Type", y = "Scale reduction factor")
ggsave("figs/supplementary/psrfs.jpeg", psrfs, device = "jpeg", height = 10, width = 8)

# Fitted predictions
preds_grouped <- read.csv("output/preds/bites/fitted_grouped_all_OD.csv")
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
      
ggsave("figs/supplementary/fitted_preds.jpeg", fitted_preds, device = "jpeg", height = 10, width = 8)

# Out of fit predictions
outfit_mora <- read.csv("output/preds/bites/outfit_mora_OD.csv")
outfit_mada <- read.csv("output/preds/bites/outfit_grouped_mada_OD.csv")

# Trying nested facet labels
outfit_mada$type <- "National"
outfit_mora$type <- "Moramanga"
outfit_mora$avg_bites <- outfit_mora$observed
outfit_all <- bind_rows(outfit_mada, outfit_mora)
outfit_all$mod_intercept <- factor(outfit_all$intercept)
levels(outfit_all$mod_intercept) <- list("Fixed intercept" = "fixed", "Random intercept" = "random")

type_labs <- c("National" = "underline('National data predicted \n by Moramanga models')", 
  "Moramanga" = "underline('Moramanga data predicted by National models')")

source("R/functions/nested_facets.R")
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

# Plot priors and posteriors ------------------------------------------------------------------
all_samps <- get.samps(parent_dir = "output/mods/samps/poisOD_National/",
                       files = list.files("output/mods/samps/poisOD_National", recursive = TRUE))

all_samps %>%
  select(-contains("alpha")) %>%
  pivot_longer(c(beta_0:beta_ttimes, sigma_0, sigma_e)) -> samps

priors <- data.frame()

random_posts <- ggplot(data = filter(samps, intercept == "random"), 
                     aes(x = value, fill = scale)) +
  geom_density(alpha = 0.5) +
  # scale_fill_manual(values = model_cols, name = "Scale",
  #                  labels = scale_labs) +
  facet_wrap(name ~ pop_predict, scales = "free", drop = TRUE, ncol = 3)

fixed_posts <- ggplot(data = filter(samps, intercept == "fixed"), 
                       aes(x = value, fill = scale)) +
  geom_density(alpha = 0.5) +
  # scale_fill_manual(values = model_cols, name = "Scale",
  #                  labels = scale_labs) +
  facet_wrap(name ~ pop_predict, scales = "free", drop = TRUE, ncol = 3)

ggsave("figs/supplementary/random_posts.jpeg", random_posts)
ggsave("figs/supplementary/fixed_posts.jpeg", fixed_posts)

# Comparing predictions vs. data for fixed and random effect models
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
  mutate(n = case_when(data_source == "Moramanga" ~ 61, 
                       data_source == "National" ~ 82)) -> model_means

model_ests %>%
  select(params, SD, pop_predict, intercept, data_source,
         scale) %>%
  spread(key = params, value = SD, fill = 0) %>%
  mutate(n = case_when(data_source == "Moramanga" ~ 61, 
                       data_source == "National" ~ 82)) -> model_SDs

# Fig M3.A
# Predictions of bite incidence per 100k
ttimes_plot <- seq(0, 15, by = 0.05)
preds <- foreach(i = 1:nrow(model_means), .combine = "bind_rows") %do% {
  
  mean_ttimes <- model_means$beta_ttimes[i]
  intercept <- model_means$beta_0[i]
  sigma <- model_means$sigma_0[i]
  preds <- exp(mean_ttimes*ttimes_plot + intercept)*1e5
  
  if (model_means$intercept[i] == "fixed") {
    upper <- preds + 1.96*sqrt(preds)
    lower <- preds - 1.96*sqrt(preds)
  }
  
  if (model_means$intercept[i] == "random") {
    ci_intercept_upper <- intercept + sigma*1.96
    ci_intercept_lower <- intercept - sigma*1.96
    upper <- exp(mean_ttimes*ttimes_plot + ci_intercept_upper)*1e5
    upper <- upper + sqrt(upper)*1.96
    lower <- exp(mean_ttimes*ttimes_plot + ci_intercept_lower)*1e5
    lower <- lower + sqrt(lower)*1.96
  }
  
  as.data.frame(list(preds = preds, upper = upper, lower = lower,
                     ttimes = ttimes_plot, scale = model_means$scale[i], 
                     data_source = model_means$data_source[i],
                     intercept = model_means$intercept[i], pop_predict = model_means$pop_predict[i]))
}

district_bites$source <- "National"
mora_bites$source <- "Moramanga"
observed <- bind_rows(district_bites, mora_bites)

scale_levs <- c("Moramanga.Commune", "National.Commune", "National.District")
scale_labs <- c("Moramanga", "Commune", "District")
model_cols <- c("#F2300F", "#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

modpreds <- ggplot(data = filter(preds, pop_predict == "flatPop"), 
                   aes(x = ttimes, y = preds, color = interaction(data_source, scale))) +
  geom_point(data = observed, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5, 
                                  shape = source), color = "black", alpha = 0.5, size = 1.5,
             stroke = 1.2, inherit.aes = FALSE) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = interaction(data_source, scale)),
              color = NA, alpha = 0.25) +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  facet_wrap(~ intercept) +
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k") +
  cowplot::theme_minimal_grid()

# Sensitivity of model ests to reporting cut-offs ---------------------------------------------

##' Sensitivity analyses
model_se <- fread("output/sensitivity/model_se.csv")
bitedata_se <- fread("output/sensitivity/bitedata_se.csv")
preds_se <- fread("output/sensitivity/modpreds_se.csv")

##' Colors
scale_levs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 

# Predictions to data -----------------------------------------------------------------------
cutoff_labs <- c("15" = "15 days", "7" = "7 days",
                 "Inf" = "Raw data \n (uncorrected)")
preds_se$pop_intercept <- glue("{preds_se$pop_predict} \n {preds_se$intercept} intercept")

preds_cutoffs <- ggplot(data = preds_se, aes(x = log(avg_bites + 0.1), y = log(mean_bites + 0.1), 
                                       color = scale)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  scale_color_manual(values = model_cols, name = "Model scale") + 
  facet_grid(pop_intercept ~ rep_cutoff, scales = "free", 
             labeller = labeller(rep_cutoff = cutoff_labs)) +
  labs(x = "log(Observed bites)", y = "log(Predicted bites)") +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave("figs/supplementary/preds_cutoff_se.jpeg", preds_cutoffs, width = 10, height = 10)

## Expected relationship between travel times and bites per 100k
model_se %>%
  select(params, Mean, pop_predict, intercept, data_source,
         scale, reporting) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop", intercept == "random") %>%
  mutate(n = 82) -> model_means

model_se %>%
  select(params, Time.series.SE, pop_predict, intercept, data_source,
         scale, reporting) %>%
  spread(key = params, value = Time.series.SE, fill = 0) %>%
  filter(pop_predict == "flatPop", intercept == "random") %>%
  mutate(n = 82) -> model_SDs

ttimes_plot <- seq(0, 15, by = 0.05)
preds <- foreach(i = 1:nrow(model_means), .combine = "bind_rows") %do% {
  
  mean_ttimes <- model_means$beta_ttimes[i]
  intercept <- model_means$beta_0[i]
  sigma <- model_means$sigma_0[i]
  preds <- exp(mean_ttimes*ttimes_plot + intercept)*1e5
  
  if (model_means$intercept[i] == "fixed") {
    upper <- preds + 1.96*sqrt(preds)
    lower <- preds - 1.96*sqrt(preds)
  }
  
  if (model_means$intercept[i] == "random") {
    ci_intercept_upper <- intercept + sigma*1.96
    ci_intercept_lower <- intercept - sigma*1.96
    upper <- exp(mean_ttimes*ttimes_plot + ci_intercept_upper)*1e5
    upper <- upper + sqrt(upper)*1.96
    lower <- exp(mean_ttimes*ttimes_plot + ci_intercept_lower)*1e5
    lower <- lower + sqrt(lower)*1.96
  }
  
  as.data.frame(list(preds = preds, upper = upper, lower = lower,
                     ttimes = ttimes_plot, scale = model_means$scale[i], 
                     data_source = model_means$data_source[i],
                     intercept = model_means$intercept[i], 
                     pop_predict = model_means$pop_predict[i], rep_cutoff = model_means$reporting[i]))
}

pred_inc_cutoffs <- ggplot(data = preds, 
                  aes(x = ttimes, y = preds, color = scale)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scale),
              color = NA, alpha = 0.25) +
  geom_point(data = bitedata_se, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5), 
             color = "black", shape = 1, alpha = 0.5) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  labs(x = "Travel times (hrs)", y = "Predicted bites per 100k") +
  theme(text = element_text(size= 20)) +
  facet_wrap(~ rep_cutoff, scales = "free", ncol = 1, labeller = labeller(rep_cutoff = cutoff_labs)) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave("figs/supplementary/pred_inc_cutoffs.jpeg", width = 7, height = 10)




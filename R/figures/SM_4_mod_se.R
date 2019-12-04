####################################################################################################
##' Sensitivity of model results to assumptions of incidence 
##' Details: deets 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

##' Packages
library(data.table)
source("R/functions/summarize_samps.R")

##' Sensitivity analyses
model_se <- fread("output/mods/sensitivity.csv")

## Figures extra
## Param estimates
samps <- get.samps.se(parent_dir = "output/mods/samps/National_se/", 
                   files = list.files("output/mods/samps/National_se", recursive = TRUE))

ggplot(data = filter(samps, pop_predict == "flatPop"), 
       aes(x = intercept, y = beta_ttimes, fill = scale, color = scale)) +
  geom_violin() +
  scale_fill_manual(values = c("turquoise4", "slateblue4")) +
  scale_color_manual(values = c("turquoise4", "slateblue4")) +
  facet_grid(contact_cutoff ~ rep_cutoff)

ggplot(data = filter(samps, pop_predict == "flatPop"), 
       aes(x = intercept, y = beta_0, fill = scale, color = scale)) +
  geom_violin() +
  scale_fill_manual(values = c("turquoise4", "slateblue4")) +
  scale_color_manual(values = c("turquoise4", "slateblue4")) +
  facet_grid(contact_cutoff ~ rep_cutoff)

## Expected relationship between travel times 
## Predictions of bite incidence per 100k
model_se %>%
  select(params, Mean, pop_predict, intercept, data_source,
         scale, contacts, reporting) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(n = 82) -> model_means

model_se %>%
  select(params, Time.series.SE, pop_predict, intercept, data_source,
         scale, contacts, reporting) %>%
  spread(key = params, value = Time.series.SE, fill = 0) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(n = 82) -> model_SDs

ttimes_plot <- seq(0, 15, by = 0.05)
preds <- foreach(i = 1:nrow(model_means), .combine = "bind_rows") %do% {
  mean_ttimes <- model_means$beta_ttimes[i]
  ci <- model_SDs$beta_ttimes[i]/sqrt(model_SDs$n[i])
  
  if (model_means$intercept[i] == "random") {
    ci = 0
  }
  
  intercept <- model_means$beta_0[i]
  sigma <- model_means$sigma_0[i]
  
  preds <- exp(mean_ttimes*ttimes_plot + intercept)*1e5
  upper <- exp((mean_ttimes + ci)*ttimes_plot + (intercept + sigma*1.96))*1e5
  lower <- exp((mean_ttimes - ci)*ttimes_plot + (intercept - sigma*1.96))*1e5
  
  as.data.frame(list(preds = preds, upper = upper, lower = lower,
                     ttimes = ttimes_plot, scale = model_means$scale[i], 
                     contacts = model_means$contacts[i], reporting = model_means$reporting[i],
                     intercept = model_means$intercept[i]))
}

ggplot(data = filter(preds, intercept == "random"), 
       aes(x = ttimes, y = preds, color = scale)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill =scale),
              color = NA, alpha = 0.25) +
  scale_color_manual(values = c("turquoise4", "slateblue4"), name = "Scale") +
  scale_fill_manual(values = c("turquoise4", "slateblue4"), name = "Scale") +
  labs(x = "Travel times (hrs)", y = "Predicted bites per 100k", tag = "A") +
  theme(text = element_text(size=20)) +
  facet_grid(reporting ~ contacts, scales = "free_y")

## convergence plots
model_se %>%
  select(contacts, reporting, scale, pop_predict, intercept, val = psrf_upper) %>%
  mutate(type = "psrf_upper") -> psrf
model_se %>%
  select(contacts, reporting, scale, pop_predict, intercept, val = mpsrf) %>%
  mutate(type = "mpsrf") -> mpsrf
convergence <- bind_rows(mpsrf, psrf)

ggplot(filter(convergence, pop_predict == "flatPop"), aes(x = type, y = val, color = scale)) + 
  geom_boxplot() +  
  scale_color_manual(values = c("turquoise4", "slateblue4"), name = "Scale") +
  facet_grid(pop_predict ~ intercept, scales = "free_x", drop = TRUE) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey") +
  scale_x_discrete(labels= c("psrf_upper" = "Indiviudal covariate", "mpsrf" = "Multivariate")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 20)) +
  labs(x = "Type", y = "Scale reduction factor") +
  facet_grid(reporting ~ contacts, scales = "free_y")


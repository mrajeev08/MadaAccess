####################################################################################################
##' Sensitivity of model results to assumptions of incidence 
##' Details: deets 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

##' Packages
library(data.table)
library(tidyverse)
library(cowplot)
library(glue)
library(foreach)

##' Sensitivity analyses
model_se <- fread("output/sensitivity/model_se.csv")
bitedata_se <- fread("output/sensitivity/bitedata_se.csv")
preds_se <- fread("output/sensitivity/modpreds_se.csv")

##' Colors
scale_levs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 

##' Predictions to data
##' ------------------------------------------------------------------------------------------------
preds_se$contact_rep <- interaction(preds_se$reporting, preds_se$contacts)
cutoff_labs <- c("15.3" = "Corrected for \n underreporting & Cat I \n excluded", 
                 "Inf.3" = "Cat I excluded only", "15.Inf" = "Corrected for \n underreporting only", 
                 "Inf.Inf" = "None (raw data)")
preds_se$pop_intercept <- glue("{preds_se$pop_predict} \n {preds_se$intercept} intercept")

figS4.1 <- ggplot(data = preds_se, aes(x = log(avg_bites + 0.1), y = log(mean_bites + 0.1), 
                            color = scale)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 2) +
  scale_color_manual(values = model_cols, name = "Model scale") + 
  facet_grid(pop_intercept ~ contact_rep, scales = "free", 
             labeller = labeller(contact_rep = cutoff_labs)) +
  labs(x = "log(Observed bites)", y = "log(Predicted bites)") +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave("figs/supplementary/S4.1.jpeg", width = 10, height = 10)

## Expected relationship between travel times and bites per 100k
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

preds$contact_rep <- interaction(preds$reporting, preds$contacts)
cutoff_labs <- c("15.3" = "Corrected for underreporting \n Cat I excluded", 
                 "Inf.3" = "Cat I excluded only", "15.Inf" = "Corrected for underreporting \n only", 
                 "Inf.Inf" = "None (raw data)")
bitedata_se$contact_rep <- interaction(bitedata_se$rep_cutoff, bitedata_se$contact_cutoff)

figS4.2 <- ggplot(data = filter(preds, intercept == "random"), 
       aes(x = ttimes, y = preds, color = scale)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill =scale),
              color = NA, alpha = 0.25) +
  geom_point(data = bitedata_se, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5), 
             color = "black", shape = 1, alpha = 0.5) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  labs(x = "Travel times (hrs)", y = "Predicted bites per 100k") +
  theme(text = element_text(size=20)) +
  facet_wrap(~ contact_rep, scales = "free", ncol = 1, labeller = labeller(contact_rep = cutoff_labs)) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 
  

ggsave("figs/supplementary/S4.2.jpeg", width = 7, height = 10)



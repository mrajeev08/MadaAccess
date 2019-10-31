####################################################################################################
##' Run predictions from all candidate models
##' Details: Models include travel times and distance as access metrics, in addition to population 
##' Author: Malavika Rajeev 
####################################################################################################
## source bite ests
source("R/02_bitedata/03_estimate_biteinc.R") # either source this or output bite data
# probs want to output bite data in order to pull into other things
source("R/functions/predict_bites.R")
source("R/functions/utils.R")

## libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)

## Estimates
model_ests <- read.csv("output/bitemod_results.csv")
model_ests %>%
  select(params, Mean, covar_name, pop_predict, intercept, ctar_bump, summed, data_source,
         scale) %>%
  spread(key = params, value = Mean, fill = 0) -> model_means


##' Predict deaths 
##' ------------------------------------------------------------------------------------------------
mada_communes@data %>%
  select(group_name = distcode, district = ADM2_EN, names = ADM3_PCODE, commune = ADM3_EN,
         pop, long, lat, 
         catch_ttimes = ctch_ttwtd, ttimes = ttms_wtd, 
         catch_dist_cent = ctch_dsct, dist_cent,  catch_dist_wtd = ctch_dswtd, dist_wtd,
         ctar_in_district) %>%
  mutate(ttimes = ttimes/60) -> covar_df_commune
mada_districts@data %>%
  select(group_name = distcode, district = ADM2_EN,
         pop, long, lat, 
         catch_ttimes = ctch_ttwtd, ttimes = ttms_wtd, 
         catch_dist_cent = ctch_dsct, dist_cent,  catch_dist_wtd = ctch_dswtd, dist_wtd,
         ctar_in_district) %>%
  mutate(names = group_name, 
         ttimes = ttimes/60) -> covar_df_district

preds_burden <- 
  foreach(i = iter(model_means, by = "row"), j = icount(), .combine = "rbind") %do% {
    
    print(j/nrow(model_means)*100)
    
    if(i$scale == "Commune") {
      covar_df <- covar_df_commune
    } else {
      covar_df <- covar_df_district
    }
    
    catches <- as.numeric(droplevels(covar_df[, paste0("catch_", i$covar_name)]))
    
    predict.deaths(access = covar_df[, as.character(i$covar_name)], ctar_in = covar_df$ctar_in_district, 
                   pop = covar_df$pop, catch = catches, names = covar_df$names, 
                   group_name = covar_df$group_name, beta_access = i$beta_access, 
                   beta_ctar = 0, beta_0 = i$beta_0, beta_pop = i$beta_pop, 
                   sigma_0 = i$sigma_0, 
                   known_alphas = NA, 
                   covar_name = i$covar_name, pop_predict = i$pop_predict, intercept = i$intercept,
                   summed = i$summed, ctar_bump = i$ctar_bump, data_source = i$data_source, 
                   scale = i$scale,
                   trans = 1e5, known_catch = FALSE, 
                   p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
                   max_HDR = 25, min_HDR = 5, 
                   dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                   prob_death = 0.16, nsims = 1000)
  }

## Grouped to district
preds_burden %>%
  group_by(group_name, data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(bites_mean:averted_lower05, pop), sum, na.rm = TRUE) -> grouped_deaths

preds_burden %>%
  group_by(group_name, data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(starts_with("p_rabid"), starts_with("reporting"), starts_with("access")), mean, na.rm = TRUE) %>%
  left_join(grouped_deaths) -> grouped_deaths

to_plot <- filter(grouped_deaths, intercept == "random", pop_predict == "flatPop", 
                  scale == "Commune")
to_plot$scale_of_data <- "District"
comm_to_plot <- filter(preds_burden, intercept == "random", pop_predict == "flatPop", 
                       scale == "Commune")
comm_to_plot$scale_of_data <- "Commune"
burden_to_plot <- bind_rows(comm_to_plot, to_plot)

ggplot() +
  geom_point(data = burden_to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5, 
                 fill = access, shape = scale_of_data, alpha = scale_of_data,
                 size = scale_of_data, color = scale_of_data)) +
  scale_fill_gradient(low = alpha("#fdebef", 1), high = alpha("#e52b50", 1), 
                      name = "Travel times (hrs)")  +
  scale_shape_manual(values = c(22, 23)) +
  scale_alpha_manual(values = c(0.75, 1)) +
  scale_size_manual(values = c(1.5, 2)) +
  scale_color_manual(values = c("grey", "black")) +
  xlab("Districts (ordered by decreasing access)") +
  ylab("Predicted incidence of deaths") +
  labs(tag = "A") +
  geom_hline(aes(yintercept = sum(comm_to_plot$deaths_mean)/sum(comm_to_plot$pop)*1e5), linetype = 2, 
             color = "darkgrey") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) +
  coord_flip()

ggplot() +
  geom_point(data = comm_to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5,
                 fill = access), shape = 22, color = alpha("black", 0.1)) +
  geom_point(data = to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5,
                 fill = access), shape = 23, color = "grey", size = 2, stroke = 1.1) +
  scale_fill_gradient(low = alpha("#fdebef", 1), high = alpha("#e52b50", 1), 
                      name = "Travel times (hrs)") +
  xlab("Districts (ordered by \n decreasing access)") +
  ylab("Predicted incidence of deaths") +
  labs(tag = "A") +
  geom_hline(aes(yintercept = sum(to_plot$deaths_mean)/sum(to_plot$pop)*1e5), linetype = 2, 
             color = "darkgrey") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) +
  coord_flip()

M4.A <- ggplot() +
  geom_point(data = comm_to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5,
             fill = access), shape = 22, color = alpha("grey", 0.5)) +
  geom_point(data = to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5),
             color = "#6a274c", shape = 22, size = 2, stroke = 1.1) +
  scale_fill_gradient(low = alpha("#fdebef", 1), high = alpha("#e52b50", 1), 
                      name = "Travel times (hrs)") +
  xlab("Districts (ordered by \n decreasing access)") +
  ylab("Predicted incidence of deaths") +
  labs(tag = "A") +
  geom_hline(aes(yintercept = sum(to_plot$deaths_mean)/sum(to_plot$pop)*1e5), linetype = 2, 
             color = "darkgrey") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) +
  coord_flip()

M4.B <- ggplot() +
  geom_point(data = comm_to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = reporting_mean,
                 fill = access), shape = 22, color = alpha("grey", 0.5)) +
  geom_point(data = to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = reporting_mean),
             color = "#00a86b", shape = 22, size = 2, stroke = 1.1) +
  scale_fill_gradient(low = alpha("#f9fbf9", 1), high = alpha("#2e5133", 1), 
                      name = "Travel times (hrs)") +
  xlab("Districts (ordered by \n decreasing access)") +
  ylab("Average reporting") +
  labs(tag = "B") +
  geom_hline(aes(yintercept = mean(to_plot$reporting_mean)), linetype = 2, 
             color = "darkgrey") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) +
  coord_flip()

figM4 <- M4.A / M4.B
ggsave("figs/M4.pdf", figM4, device = "pdf", height = 12, width = 8)

grouped_deaths %>%
  group_by(data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(bites_mean:averted_lower05, pop), sum, na.rm = TRUE) -> national_deaths

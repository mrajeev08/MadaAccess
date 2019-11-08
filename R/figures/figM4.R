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

## Predicted burden
preds_burden <- read.csv("output/burden/baseline_burden.csv")

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

dist_to_plot <- filter(grouped_deaths, intercept == "random", pop_predict == "flatPop", 
                  scale == "District")
dist_to_plot$model <- "District"
comm_to_plot <- filter(preds_burden, intercept == "random", pop_predict == "flatPop", 
                       scale == "Commune")
comm_to_plot$model <- "Commune"
burden_to_plot <- bind_rows(comm_to_plot, dist_to_plot)

M4.A <- ggplot() +
  geom_hline(aes(yintercept = sum(comm_to_plot$deaths_mean)/sum(comm_to_plot$pop)*1e5), linetype = 1, 
             color = "#004b49", alpha = 0.75, size = 1.2) +
  geom_hline(aes(yintercept = sum(dist_to_plot$deaths_mean)/sum(dist_to_plot$pop)*1e5), linetype = 1, 
             color = "#cc7722", alpha = 0.75, size = 1.2) +
  geom_point(data = burden_to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5, 
                 fill = access, shape = model, alpha = model,
                 size = model, color = model, stroke = 1.1)) +
  scale_fill_viridis_c(option = "viridis", direction = 1,
                      name = "Travel times \n (hrs)", limits=c(0, 15), oob = scales::squish)  +
  scale_shape_manual(values = c(22, 23), name = "Model scale") +
  scale_alpha_manual(values = c(0.85, 1), name = "Model scale") +
  scale_size_manual(values = c(2.5, 3.5), name = "Model scale") +
  scale_color_manual(values = c("darkgrey", "black"), name = "Model scale") +
  labs(x = "Districts (ordered by \n increasing travel times)", 
       y = "Predicted incidence of \n deaths per 100k", tag = "A") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), text = element_text(size=20)) +
  coord_flip(clip = "off")

gg_commune <- fortify(mada_communes, region = "commcode")
gg_commune %>% 
  left_join(comm_to_plot, by = c("id" = "names")) -> gg_commune_plot
gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(dist_to_plot, by = c("id" = "group_name")) -> gg_district_plot

M4.B <- ggplot() +
  geom_polygon(data = gg_commune_plot,
               aes(x = long, y = lat, group = group, fill = deaths_mean/pop*1e5), 
               color = "white", size = 0.1) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "B") +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                      name = "Predicted incidence \n of deaths per 100k") +
  theme_void(base_size = 20)


M4.C <- ggplot() +
  geom_polygon(data = gg_district_plot,
               aes(x = long, y = lat, group = group, fill = deaths_mean/pop*1e5), 
               color = "white", size = 0.1) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "C") +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "Predicted incidence \n of deaths per 100k") +
  theme_void(base_size = 20)


figM4 <- (M4.A | ((M4.B / M4.C) + plot_layout(nrow = 2))) + plot_layout(widths = c(1, 2))
ggsave("figs/M4.jpeg", figM4, device = "jpeg", height = 14, width = 12)

## National deaths
preds_burden %>%
  group_by(data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(bites_mean:averted_lower05, pop), sum, na.rm = TRUE) -> preds_national
filter(preds_national, data_source == "National", pop_predict == "flatPop", 
       intercept == "random") -> check


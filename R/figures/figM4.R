####################################################################################################
##' Baseline burden predictions
##' Details: Predictions of baseline burden at admin level
##' Author: Malavika Rajeev 
####################################################################################################
## source bite ests
rm(list = ls())
source("R/functions/utils.R")

## libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(data.table)
library(rgdal)
library(patchwork)

## Predicted burden
burden_preds <- fread("output/preds/complete/burden_filled.csv")
burden_preds <- burden_preds[scenario == 0]
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")

## Grouped to district
burden_preds$distcode <- mada_communes$distcode[burden_preds$names]
burden_preds$commcode <- mada_communes$commcode[burden_preds$names]

burden_preds %>%
  filter(scale == "District") %>%
  group_by(distcode) %>% 
  summarize_at(vars(bites_mean:averted_lower, pop), sum, na.rm = TRUE) -> district_deaths
burden_preds %>%
  filter(scale == "District") %>%
  group_by(distcode) %>% 
  summarize_at(vars(starts_with("p_rabid"), starts_with("reporting"), starts_with("ttimes")), 
               mean, na.rm = TRUE) %>%
  left_join(district_deaths) %>%
  mutate(scale = "District") -> district_deaths
burden_to_plot <- bind_rows(burden_preds[scale == "Commune"], district_deaths)

burden_preds %>%
  group_by(scale) %>%
  summarize(natl_inc = sum(deaths_mean, na.rm = TRUE)/sum(pop, na.rm = TRUE)*1e5) -> natl_inc
  

M4.A <- ggplot() +
  geom_hline(data = natl_inc, aes(yintercept = natl_burden$natl_inc, color = scale), linetype = 1, 
             alpha = 0.75, size = 1.2) +
  geom_point(data = burden_to_plot, 
             aes(x = reorder(distcode, ttimes), y = deaths_mean/pop*1e5, 
                 fill = ttimes, shape = scale, alpha = scale,
                 size = scale, color = scale, stroke = 1.1)) +
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
  left_join(burden_preds[scale == "Commune"], by = c("id" = "commcode")) -> gg_commune_plot

gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(district_deaths, by = c("id" = "distcode")) -> gg_district_plot

M4.B <- ggplot() +
  geom_polygon(data = gg_commune_plot,
               aes(x = long, y = lat, group = group, fill = reporting_mean), 
               color = "white", size = 0.1) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "B") +
  scale_fill_viridis_c(breaks = c(0, 0.25, 0.5, 0.75, 1), limits=c(0, 1),
                       option = "magma", direction = 1, 
                      name = "Predicted reporting") +
  theme_void(base_size = 20)


M4.C <- ggplot() +
  geom_polygon(data = gg_district_plot,
               aes(x = long, y = lat, group = group, fill = reporting_mean), 
               color = "white", size = 0.1) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "C") +
  scale_fill_viridis_c(breaks = c(0, 0.25, 0.5, 0.75, 1), limits=c(0, 1),
                       option = "magma", direction = 1, 
                       name = "Predicted reporting") +
  theme_void(base_size = 20)


figM4 <- (M4.A | ((M4.B / M4.C) + plot_layout(nrow = 2))) + plot_layout(widths = c(1, 2))
ggsave("figs/M4.jpeg", figM4, device = "jpeg", height = 14, width = 12)

## National deaths
burden_preds %>%
  group_by(scale) %>%
  summarize(deaths_mean = sum(deaths_mean, na.rm = TRUE),
            deaths_upper = sum(deaths_upper, na.rm = TRUE), 
            deaths_lower = sum(deaths_lower, na.rm = TRUE)) -> natl_burden


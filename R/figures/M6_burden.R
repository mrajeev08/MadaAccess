# ------------------------------------------------------------------------------------------------ #
#' Baseline burden predictions      
#' Details: Predictions of baseline burden at admin level          
# ------------------------------------------------------------------------------------------------ #

source("R/functions/out.session.R")

# libraries
library(tidyverse)
library(data.table)
library(rgdal)
library(patchwork)
library(cowplot)

# Predicted burden
burden_preds <- fread("output/preds/burden_base.gz")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")
ctar_metadata <- fread("data/processed/clinics/ctar_metadata.csv")

# Grouped to district
burden_preds$distcode <- mada_communes$distcode[match(burden_preds$names, mada_communes$commcode)]
burden_preds$commcode <- burden_preds$names

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

# Colors and labs
scale_labs <- c("Commune", "District")
model_cols <- c("darkgrey", "black")
names(scale_labs) <- scale_labs
names(model_cols) <- scale_labs

compare_burden <- ggplot() +
  geom_hline(data = natl_inc, aes(yintercept = natl_inc, color = scale), linetype = 1, 
             alpha = 0.75, size = 1.2) +
  geom_point(data = burden_to_plot, 
                  aes(x = reorder(distcode, ttimes), y = deaths_mean/pop*1e5, 
                      fill = ttimes, shape = scale, alpha = scale,
                      size = scale, color = scale, stroke = 1.1)) +
  scale_fill_viridis_c(option = "viridis", direction = 1,
                      name = "Travel times \n (hrs)", limits=c(0, 15), oob = scales::squish)  +
  scale_shape_manual(values = c(22, 23), labels = scale_labs,
                     name = "Scale") +
  scale_alpha_manual(values = c(0.85, 1), labels = scale_labs,
                     name = "Scale") +
  scale_size_manual(values = c(2.5, 3.5), labels = scale_labs, 
                    name = "Scale") +
  scale_color_manual(values = model_cols, labels = scale_labs, 
                     name = "Scale") +
  labs(x = "Districts (ordered by \n increasing travel times)", 
       y = "Predicted incidence of \n deaths per 100k", tag = "A") +
  theme_minimal_hgrid() +
  theme(axis.text.y = element_blank()) +
  coord_flip(clip = "off")

gg_commune <- fortify(mada_communes, region = "commcode")
gg_commune %>% 
  left_join(burden_preds[scale == "Commune"], by = c("id" = "commcode")) -> gg_commune_plot

gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(district_deaths, by = c("id" = "distcode")) -> gg_district_plot

col_lim <- c(floor(min(gg_commune_plot$deaths_mean/gg_commune_plot$pop*1e5)),
             ceiling(max(gg_commune_plot$deaths_mean/gg_commune_plot$pop*1e5)))

comm_burden <- ggplot() +
  geom_polygon(data = gg_commune_plot,
               aes(x = long, y = lat, group = group, fill = deaths_mean/pop*1e5), 
               color = "white", size = 0.05) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "B") +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                      name = "Predicted incidence \n of deaths per 100k",
                      limits = col_lim) +
  theme_map() +
  coord_quickmap()


district_burden <- ggplot() +
  geom_polygon(data = gg_district_plot,
               aes(x = long, y = lat, group = group, fill = deaths_mean/pop*1e5), 
               color = "white", size = 0.05) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "C") +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "Predicted incidence \n of deaths per 100k",
                       limits = col_lim) +
  theme_map() +
  coord_quickmap()

burden_base <- (compare_burden | ((comm_burden / district_burden) + plot_layout(nrow = 2, guides = "collect"))) + plot_layout(widths = c(1, 2))
ggsave("figs/main/M6_burden_base.jpeg", burden_base, height = 14, width = 10)
ggsave("figs/main/M6_burden_base.tiff", burden_base, dpi = 300, device = "tiff", height = 12, width = 10, 
       compression = "lzw", type = "cairo")



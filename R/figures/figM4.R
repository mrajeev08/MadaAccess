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
preds_burden <- read.csv("output/preds/baseline_burden.csv")

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

M4.A <- ggplot() +
  geom_point(data = burden_to_plot, 
             aes(x = reorder_within(group_name, access, covar_name), y = deaths_mean/pop*1e5, 
                 fill = access, shape = scale_of_data, alpha = scale_of_data,
                 size = scale_of_data, color = scale_of_data)) +
  scale_fill_viridis_c(option = "viridis", 
                      name = "Travel times \n (hrs)")  +
  scale_shape_manual(values = c(22, 23), name = "Scale of prediction") +
  scale_alpha_manual(values = c(0.75, 1), name = "Scale of prediction") +
  scale_size_manual(values = c(1.5, 2), name = "Scale of prediction") +
  scale_color_manual(values = c("grey", "black"), name = "Scale of prediction") +
  labs(x = "Districts (ordered by \n increasing travel times)", 
       y = "Predicted incidence of \n deaths per 100k", tag = "A") +
  geom_hline(aes(yintercept = sum(comm_to_plot$deaths_mean)/sum(comm_to_plot$pop)*1e5), linetype = 2, 
             color = "darkgrey") +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), text = element_text(size=20)) +
  coord_flip()

gg_commune <- fortify(mada_communes, region = "ADM3_PCODE")
gg_commune %>% 
  left_join(comm_to_plot, by = c("id" = "names")) -> gg_commune_plot
gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(to_plot, by = c("id" = "group_name")) -> gg_district_plot

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

figM4 <- (M4.A | M4.B) + plot_layout(widths = c(1, 1.5))
ggsave("figs/M4.jpeg", figM4, device = "jpeg", height = 8, width = 12)

## National deaths
preds_burden %>%
  preds_burden %>%
  group_by(group_name, data_source, covar_name, scale, pop_predict, ctar_bump, 
           summed, intercept) %>% 
  summarize_at(vars(bites_mean:averted_lower05, pop), sum, na.rm = TRUE) -> grouped_deaths


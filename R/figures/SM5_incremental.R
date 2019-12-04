####################################################################################################
##' Plotting scenario shifts with ggridges
##' Details: Pulling in district and commune estimates of travel times as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list=ls())

##' Libraries and packages
library(data.table)
library(tidyverse)
library(rgdal)
library(ggridges)
library(patchwork)
source("R/functions/predict_bites.R")

## District travel times
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")
commune_master <- fread("output/ttimes/master_commune.csv")
district_master <- fread("output/ttimes/master_district.csv")
model_ests <- read.csv("output/mods/estimates.csv")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")

## Single catchment
district_master$scale <- "District"
commune_master$scale <- "Commune"
scenario_to_plot <- rbind(district_master, commune_master, fill = TRUE)

## colors 
scale_levs <- c("Commune", "District")
model_cols <- wesanderson::wes_palettes$Rushmore1[c(3, 4)]
names(model_cols) <- scale_levs 

## Mapping commune and districts where clinic added
## candidate points
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, lat = ycoor, long = xcoor) %>%
  mutate(candidate_id = 1:nrow(.) + 31) -> csbs

pts <- SpatialPoints(cbind(csbs$long, csbs$lat), 
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
csbs$commcode <- over(pts, mada_communes)$commcode
csbs$distcode <- over(pts, mada_districts)$distcode

commune_master %>%
  group_by(clinic_added) %>%
  summarize(when_added = min(scenario)) %>%
  left_join(csbs, by = c("clinic_added" = "candidate_id")) -> when_added
when_added %>%
  group_by(commcode) %>%
  summarize(when_added = min(when_added)) -> when_comm
when_added %>%
  group_by(distcode) %>%
  summarize(when_added = min(when_added)) -> when_dist
mada_communes$when_added <- when_comm$when_added[match(mada_communes$commcode, when_comm$commcode)]
# mada_communes$when_added[is.na(mada_communes$when_added)] <- 473
mada_districts$when_added <- when_dist$when_added[match(mada_districts$distcode, when_dist$distcode)]
# mada_districts$when_added[is.na(mada_districts$when_added)] <- 473

gg_commune <- fortify(mada_communes, region = "commcode")
gg_commune %>% 
  left_join(mada_communes@data, by = c("id" = "commcode")) -> gg_commune_plot

gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(mada_districts@data, by = c("id" = "distcode")) -> gg_district_plot

SN.1A <- ggplot() +
  geom_polygon(data = gg_district_plot,
               aes(x = long.x, y = lat.x, group = group, fill = when_added), 
               color = "white", size = 0.1) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "A") +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "When clinic added", na.value = "black") +
  theme_void(base_size = 20)

SN.1B <- ggplot() +
  geom_polygon(data = gg_commune_plot,
               aes(x = long.x, y = lat.x, group = group, fill = when_added), 
               color = "white", size = 0.1) +
  # geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
  #             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "B") +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "When clinic added", na.value = "black") +
  theme_void(base_size = 20)
SN.1 <- SN.1A | SN.1B
ggsave("figs/SN1.jpeg", SN.1, height = 10, width = 12)

## ggridges to show shifts in distribution of max catchments
SN2.A <- ggplot() +
  geom_density_ridges(data = scenario_to_plot[scenario %in% c(1, 100, 200, 300, 472, 
                                                              max(scenario_to_plot$scenario))], 
                      aes(x = prop_pop_catch, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  xlim(c(0, 1)) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Maximum proportion \n of population in clinic catchment",
       tag = "A")

## Shift in travel times
## ggridges to show shifts in distribution of max catchments
SN2.B <- ggplot() +
  geom_density_ridges(data = scenario_to_plot[scenario %in% c(1, 100, 200, 300, 472,
                                                              max(scenario_to_plot$scenario))], 
                      aes(x = weighted_times/60, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Travel times (hrs)",
       tag = "B")
SN.2<- SN2.A | SN2.B
ggsave("figs/SN2.jpeg", SN.2, height = 10, width = 10)

## Shift in incidence of deaths
admin_preds <- fread("output/preds/complete/burden_filled.csv")
vial_preds <- fread("output/preds/complete/vials_filled.csv")

SX.A <- ggplot() +
  geom_density_ridges(data = admin_preds[scenario %in% c(1, 100, 200, 300, 472,
                                                              max(admin_preds$scenario))], 
                      aes(x = bites_mean/pop*1e5, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Reported bites per 100k", tag = "A")

SX.B <- ggplot() +
  geom_density_ridges(data = admin_preds[scenario %in% c(1, 100, 200, 300, 472,
                                                         max(admin_preds$scenario))], 
                      aes(x = reporting_mean, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Reporting", tag = "B")

SX.C <- ggplot() +
  geom_density_ridges(data = admin_preds[scenario %in% c(1, 100, 200, 300, 472,
                                                         max(admin_preds$scenario))], 
                      aes(x = deaths_mean/pop*1e5, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Deaths per 100k", tag = "C")
SX <- SX.A | SX.B | SX.C
ggsave("figs/SX.jpeg", device = "jpeg", height = 10, width = 12)

## Shift in bites reported to catchment
admin_preds %>%
  group_by(scale, scenario, catch) %>%
  summarize_at(vars(starts_with("bites")), sum, na.rm = TRUE) -> catch_summ

ggplot() +
  geom_density_ridges(data = filter(catch_summ, scenario %in% c(1, 100, 200, 300, 472,
                                                         max(vial_preds$scenario))), 
                      aes(x = bites_mean, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  scale_x_continuous(trans = "log", breaks = c(0, 100, 1000, 10000)) +
  labs(y = "Number of clinics added", x = "Bites reported to clinic")


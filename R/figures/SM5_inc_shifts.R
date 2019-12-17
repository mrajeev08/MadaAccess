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
library(raster)
library(rasterVis)
select <- dplyr::select
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

##' Plotting where clinics are added 
##' ------------------------------------------------------------------------------------------------
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

pts <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(pts, mada_communes)$commcode
ctar_metadata$distcode <- over(pts, mada_districts)$distcode

ctar_metadata %>%
  select(CTAR, lat = LATITUDE, long = LONGITUDE, commcode, distcode) %>%
  mutate(clinic_added = 1:31, when_added = 0) %>%
  bind_rows(when_added) -> all_added
  
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

clinic_cols <- c('#f6eff7','#d0d1e6','#a6bddb','#67a9cf','#1c9099','#016c59')
clinic_brks <- c(0, 10, 50, 100, 200, 400, 472)
clinic_labs <- c("10", "50", "100", "200", "400", "472")
names(clinic_cols) <- clinic_labs

S5.1A <- ggplot() +
  geom_polygon(data = gg_district_plot,
               aes(x = long.x, y = lat.x, group = group, 
                   fill = cut(when_added, breaks = clinic_brks, labels = clinic_labs)), 
               color = NA) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "grey50",
             shape = 4, size = 2, stroke = 1.5) +
  labs(tag = "A") +
  scale_fill_manual(values = clinic_cols, name = "When clinic added", na.value = "darkblue", 
                    na.translate = FALSE) +
  guides(fill = "none")

S5.1B <- ggplot() +
  geom_polygon(data = gg_commune_plot,
               aes(x = long.x, y = lat.x, group = group, 
                   fill = cut(when_added, breaks = clinic_brks, labels = clinic_labs)), 
               color = NA) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE, shape = 4), color = "grey50",
             size = 2, stroke = 1.5) +
  labs(tag = "B") +
  scale_fill_manual(values = clinic_cols, name = "When clinic added", na.value = "darkblue", 
                    na.translate = FALSE) +
  scale_shape_identity(guide = "legend", name = "", labels = "Existing ARMC") 

S5.1 <- S5.1A | S5.1B
ggsave("figs/S5.1.pdf", S5.1, height = 5, width = 7)

##' Plots of how ttimes change 
##' ------------------------------------------------------------------------------------------------
scenario_to_plot %>%
  filter(scenario %in% c(0, 100, 200, 300, 472, max(scenario_to_plot$scenario)), 
         scale == "District") %>%
  pivot_wider(id_cols = distcode, names_from = scenario, values_from = weighted_times) %>%
  right_join(gg_district, by = c("distcode" = "id")) %>%
  pivot_longer(`0`:`1648`) -> gg_district_plot

scenario_to_plot %>%
  mutate(commcode = mada_communes$commcode[commune_id]) %>%
  filter(scenario %in% c(0, 100, 200, 300, 472, max(scenario_to_plot$scenario)), 
         scale == "Commune") %>%
  pivot_wider(id_cols = commcode, names_from = scenario, values_from = weighted_times) %>%
  right_join(gg_commune, by = c("commcode" = "id")) %>%
  pivot_longer(`0`:`1648`) -> gg_commune_plot

ttime_cols <- c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
ttime_breaks <- c(-0.1, 1, 2, 4, 6, 8, 10, 12, Inf)
ttime_labs <- c("< 1", "< 2", "< 4", "< 6", "< 8", "< 10", "< 15", "15 +")
names(ttime_cols) <- ttime_labs

gg_district_plot$scenario <- factor(gg_district_plot$name)
levels(gg_district_plot$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                      "300" = "300", "472" = "472", "max" = "1648")
gg_commune_plot$scenario <- factor(gg_commune_plot$name)
levels(gg_commune_plot$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                          "300" = "300", "472" = "472", "max" = "1648")

## Getting ttime rasters
steps <- c(100, 200, 300, 472, max(scenario_to_plot$scenario))
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
base_times <- raster("output/ttimes/baseline_ttimes.tif")
source("R/functions/ttime_functions.R")
library(gdistance)
library(foreach)
all_times <- base_times
all_pts <- filter(all_added, when_added == 0)
all_pts$scenario <- 0

foreach(i = 1:length(steps)) %do% {
  print(i)
  pts <- filter(all_added, when_added <= steps[i])
  pts$scenario <- steps[i]
  point_mat_base <- as.matrix(dplyr::select(pts, Y_COORD = long, X_COORD = lat))
  ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                            coords = point_mat_base, trans_matrix_exists = TRUE,
                            filename_trans = "data/processed/rasters/trans_gc_masked.rds")
  names(ttimes) <- steps[i]
  all_pts <- bind_rows(all_pts, pts)
  all_times <- stack(all_times, ttimes)
}

all_times_df <- as.data.frame(all_times, xy = TRUE) 
all_times_df %>%
  pivot_longer(baseline_ttimes:X1648) %>%
  mutate(value = ifelse(is.infinite(value), NA, value)) -> all_times_df
all_times_df$scenario <- factor(all_times_df$name)
levels(all_times_df$scenario) <- list("baseline" = "baseline_ttimes", "100" = "X100", "200" = "X200",
                                         "300" = "X300", "472" = "X472", "max" = "X1648")
all_pts$scenario <- factor(all_pts$scenario)
levels(all_pts$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                      "300" = "300", "472" = "472", "max" = "1648")
S5.2A <- ggplot() + 
  geom_raster(data = all_times_df, aes(x = x, y = y, 
                                       fill = cut(value/60, breaks = ttime_breaks, labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, guide = "none") +
  geom_point(data = all_pts, aes(x = long, y = lat), color = "grey50", alpha = 0.75,
             shape = ".") +
  facet_wrap(~ scenario, nrow = 1) +
  theme_void() +
  labs(tag = "A")

S5.2B <- ggplot() +
  geom_polygon(data = gg_commune_plot, aes(x = long, y = lat, group = group, 
                                            fill = cut(value/60, breaks = ttime_breaks, 
                                                       labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  geom_point(data = all_pts, aes(x = long, y = lat, shape = "A"), color = "grey50", alpha = 0.75) +  
  scale_shape_manual(values = ".", guide = "legend", name = "", labels = "ARMC") +
  facet_wrap(~ scenario, nrow = 1) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  labs(tag = "B")  

S5.2C <- ggplot() +
  geom_polygon(data = gg_district_plot, aes(x = long, y = lat, group = group, 
                                        fill = cut(value/60, breaks = ttime_breaks, 
                                                   labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, guide = "none") +
  geom_point(data = all_pts, aes(x = long, y = lat), color = "grey50", alpha = 0.75,
             shape = ".") +  
  facet_wrap(~ scenario, nrow = 1) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  labs(tag = "C")  

S5.2 <- S5.2A / S5.2B / S5.2C
ggsave("figs/S5.2.pdf", S5.2, height = 7, width = 7)

mysec
## Shift in travel times
## ggridges to show shifts in distribution of max catchments
S5.2A <- ggplot() +
  geom_density_ridges(data = scenario_to_plot[scenario %in% c(1, 100, 200, 300, 472,
                                                              max(scenario_to_plot$scenario))], 
                      aes(x = weighted_times/60, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Travel times (hrs)",
       tag = "A")

## ggridges to show shifts in distribution of max catchments
S5.2B <- ggplot() +
  geom_density_ridges(data = scenario_to_plot[scenario %in% c(1, 100, 200, 300, 472, 
                                                              max(scenario_to_plot$scenario))], 
                      aes(x = prop_pop_catch, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  xlim(c(0, 1)) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, 472, "max")) +
  labs(y = "Number of clinics added", x = "Maximum proportion \n of population in clinic catchment",
       tag = "B")

S5.2 <- S5.2A | S5.2B
ggsave("figs/S5.2.jpeg", S5.2, height = 10, width = 10)

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


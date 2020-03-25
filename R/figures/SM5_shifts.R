# ------------------------------------------------------------------------------------------------ #
#' Plotting scenario shifts        
#' Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

# Libraries and packages
library(data.table)
library(tidyverse)
library(rgdal)
library(ggridges)
library(patchwork)
library(raster)
library(rasterVis)
library(cowplot)
library(ggforce)
library(gdistance)
library(foreach)
select <- dplyr::select
source("R/functions/ttime_functions.R")
source("R/functions/out.session.R")
source("R/functions/bezier.R")

# Pull in data
ctar_metadata <- read.csv(file = "data/raw/ctar_metadata.csv")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
base_times <- raster("output/ttimes/base_ttimes.tif")
pop_1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")

# Commune/District preds
commune_maxcatch <- fread("output/ttimes/commune_maxcatch.gz")
district_maxcatch <- fread("output/ttimes/district_maxcatch.gz")
commune_allcatch <- fread("output/ttimes/commune_allcatch.gz")
district_allcatch <- fread("output/ttimes/district_allcatch.gz")

# Single catchment
district_maxcatch$scale <- "District"
commune_maxcatch$scale <- "Commune"
scenario_to_plot <- rbind(district_maxcatch, commune_maxcatch, fill = TRUE)
max_added <- sort(unique(scenario_to_plot$scenario), decreasing = TRUE)[2]
max_total <- max_added + 200

# colors 
scale_levs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 

# Clinics added ------------------------------------------------------------------------------
prop_df <- fread("output/ttimes/addclinics_prop_df.csv")
prop_df$step_added <- 1:nrow(prop_df)

# join w/ csb + ctar points
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")

# candidate points
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, lat = ycoor, long = xcoor) -> csbs

point_mat_all <- rbind(dplyr::select(ctar_metadata, long = LONGITUDE, lat = LATITUDE),
                       dplyr::select(csbs, long, lat))
point_mat_all$clinic_id <- 1:nrow(point_mat_all)
point_mat_all %>%
  left_join(prop_df) %>%
  mutate(prop_pop = case_when(is.na(prop_pop) & clinic_id < 32 ~ 0,
                              is.na(prop_pop) & clinic_id > 32 ~ 1e-12,
                              !is.na(prop_pop) ~ prop_pop),
         step_added = case_when(is.na(step_added) & clinic_id < 32 ~ 0,
                                is.na(step_added) & clinic_id > 32 ~ 923,
                                !is.na(step_added) ~ as.numeric(step_added))) -> point_mat_all

gg_commune <- fortify(mada_communes, region = "commcode")

S5.1_ARMCadded <- ggplot() +
  geom_polygon(data = gg_commune, aes(x = long, y = lat, group = group), fill = "black") +
  geom_point(data = filter(point_mat_all, step_added > 0),
             aes(x = long, y = lat, size = prop_pop, fill = step_added), color = "grey50", 
             alpha = 0.75, shape = 21) +
  geom_point(data = filter(point_mat_all, step_added == 0),
             aes(x = long, y = lat), shape = 3, size = 2, stroke = 1.2, color = "white") +
  coord_quickmap() +
  scale_fill_distiller(trans = "sqrt", direction = -1, palette = "PuRd",
                        breaks = c(1, 100, 200, 400, 923), name = "# clinic added") +
  scale_size_continuous(name = "Proportion of pop \n (travel times reduced < 3 hrs)") +
  theme_map()

ggsave("figs/supplementary/S5.1_ARMCadded.jpeg", S5.1_ARMCadded, width = 6, height = 8)

# Clinics added ------------------------------------------------------------------------------
bites_by_catch <- fread("output/preds/catch_preds.gz")
props_by_catch <- fread("output/preds/catch_props.csv")
burden_all <- fread("output/preds/burden_all.gz")
scenarios <- unique(bites_by_catch$scenario)
scenarios <- scenarios[scenarios %in% c(0, 100, 200, 400, sort(scenarios)[length(scenarios) - 1], 
                                        max(scenarios))]
bites_by_catch <- bites_by_catch[scenario %in% scenarios]
props_by_catch <- props_by_catch[scenario %in% scenarios]
burden_all <- burden_all[scenario %in% scenarios & scale != ""]

bites_by_catch %>%
  left_join(point_mat_all, by = c("catchment" = "clinic_id")) -> bites_by_catch # clinic points

# left join with props by catch
clinic_bites <- left_join(bites_by_catch, props_by_catch) # need to make sure this is joined with clinic id as well!
clinic_bites$line_size <- clinic_bites$prop_bites*clinic_bites$bites_mean
clinic_bites <- left_join(clinic_bites, select(mada_communes@data, commcode, long_cent, lat_cent))
bez_pts <- get.bezier.pts(from = data.frame(long = clinic_bites$long, 
                                            lat = clinic_bites$lat),
                          to = data.frame(long = clinic_bites$long_cent, 
                                          lat = clinic_bites$lat_cent), 
                          frac = 0.5, transform = function(x) sqrt(1/x)*0.1) 
clinic_bites$group <- 1:nrow(clinic_bites)
clinic_lines <- left_join(clinic_bites, bez_pts, by = c("group" = "group"))
clinic_lines %>%
  group_by(commcode, catchment) %>%
  arrange(index) -> clinic_lines

# Filter out commune points
pts <- SpatialPoints(cbind(bites_by_catch$long, bites_by_catch$lat), 
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
bites_by_catch$commcode_ctar <- over(pts, mada_communes)$commcode
clinic_bites$commcode_ctar <- bites_by_catch$commcode_ctar[match(clinic_bites$catchment,
                                                                 bites_by_catch$catchment)]
clinic_bites_incomm <- filter(clinic_bites, commcode_ctar == commcode)
clinic_bites <- filter(clinic_bites, commcode_ctar != commcode)
bites_by_catch <- left_join(bites_by_catch, select(mada_communes@data, commcode, long_cent, lat_cent),
                            by = c("commcode_ctar" = "commcode"))

# Get bite incidence @ comm level
gg_commune <- left_join(select(burden_all, names, scenario, scale, pop, bites_mean), gg_commune,
                        by = c("names" = "id"))

comm <- ggplot() +
  geom_polygon(data = filter(gg_commune, scale == "Commune"), 
               aes(x = long, y = lat, group = group, alpha = bites_mean/pop*1e5)) +
  geom_segment(data = filter(clinic_bites, scale == "Commune"), 
               aes(x = long, y = lat, xend = long_cent, yend = lat_cent, color = step_added),
                   # size = log(ceiling(prop_bites*bites_mean + 1.1))*0.1),
               alpha = 1) +
  geom_point(data = filter(bites_by_catch, scale == "Commune"),
             aes(x = long, y = lat, size = log(bites_mean + 1.1)*0.5, fill = step_added), color = "black",
                 shape  = 21, alpha = 0.75) +
  scale_size_identity(breaks = log(c(5, 50, 500, 5000, 10000) + 1.1)*0.5, 
                      labels = c(5, 50, 500, 5000, "10000 +"),
                      name = "Bites reported to clinic", guide = "legend") +
  scale_color_distiller(aesthetics = c("color", "fill"), name = "Step clinic added") +
  scale_alpha(name = "Bites per 100k", breaks = c(5, 100, 200, 500)) +
  coord_quickmap() +
  facet_wrap(~ scenario) +
  theme_map()

# Plots of how ttimes change ----------------------------------------------------------------
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

commune_maxcatch %>%
  group_by(clinic_added) %>%
  summarize(when_added = min(scenario)) %>%
  right_join(csbs, by = c("clinic_added" = "candidate_id")) %>%
  mutate(when_added = ifelse(is.na(when_added), 1648, when_added)) -> when_added

pts <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(pts, mada_communes)$commcode
ctar_metadata$distcode <- over(pts, mada_districts)$distcode

ctar_metadata %>%
  select(CTAR, lat = LATITUDE, long = LONGITUDE, commcode, distcode) %>%
  mutate(clinic_added = 1:31, when_added = 0) %>%
  bind_rows(when_added) -> all_added

gg_commune <- fortify(mada_communes, region = "commcode")
gg_commune %>% 
  left_join(mada_communes@data, by = c("id" = "commcode")) -> gg_commune_plot

gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(mada_districts@data, by = c("id" = "distcode")) -> gg_district_plot

clinic_cols <- c("#014636", "#016c59", "#02818a", "#3690c0", "#67a9cf", "#a6bddb", 
                 "#d0d1e6", "black")
clinic_brks <- c(0, 10, 50, 100, 200, 400, max_added, 1648, Inf)
clinic_labs <- c("10", "50", "100", "200", "400", paste(max_added), "max", "None added")
names(clinic_cols) <- clinic_labs

scenario_to_plot %>%
  filter(scenario %in% c(0, 100, 200, 300, max_added, max(scenario_to_plot$scenario)), 
         scale == "District") %>%
  pivot_wider(id_cols = distcode, names_from = scenario, values_from = ttimes_wtd) %>%
  right_join(gg_district, by = c("distcode" = "id")) %>%
  pivot_longer(`0`:`1648`) -> gg_district_plot

scenario_to_plot %>%
  filter(scenario %in% c(0, 100, 200, 300, max_added, max(scenario_to_plot$scenario)), 
         scale == "Commune") %>%
  pivot_wider(id_cols = commcode, names_from = scenario, values_from = ttimes_wtd) %>%
  right_join(gg_commune, by = c("commcode" = "id")) %>%
  pivot_longer(`0`:`1648`) -> gg_commune_plot

ttime_cols <- c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
ttime_breaks <- c(-0.1, 1, 2, 4, 6, 8, 10, 12, Inf)
ttime_labs <- c("< 1", "< 2", "< 4", "< 6", "< 8", "< 10", "< 15", "15 +")
names(ttime_cols) <- ttime_labs

gg_district_plot$scenario <- factor(gg_district_plot$name)
levels(gg_district_plot$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                      "300" = "300", "723" = "723", 
                                      "max (1648)" = "1648")
gg_commune_plot$scenario <- factor(gg_commune_plot$name)
levels(gg_commune_plot$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                          "300" = "300", "723" = "723", 
                                         "max (1648)" = "1648")

# Getting ttime rasters
steps <- c(0, 100, 200, 300, max_added, max(scenario_to_plot$scenario))
all_times <- base_times
all_pts <- filter(all_added, when_added == 0)
all_pts$scenario <- 0

foreach(i = 1:length(steps), .combine = 'bind_rows') %do% {
  print(i)
  pts <- filter(all_added, when_added <= steps[i])
  pts$scenario <- steps[i]
  point_mat_base <- as.matrix(dplyr::select(pts, Y_COORD = long, X_COORD = lat))
  ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                            coords = point_mat_base, trans_matrix_exists = TRUE,
                            filename_trans = "data/processed/rasters/trans_gc_masked.rds")
  names(ttimes) <- "ttimes"
  ttimes_df <- as.data.frame(ttimes, xy = TRUE)
  ttimes_df$scenario <- steps[i]
  ttimes_df$pop <- getValues(pop_1x1)
  all_pts <- bind_rows(all_pts, pts)
  ttimes_df
} -> all_times_df

all_times_df$scenario <- factor(all_times_df$scenario)
levels(all_times_df$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                      "300" = "300", "723" = "723", 
                                      "max (1648)" = "1648")
all_pts$scenario <- factor(all_pts$scenario)
levels(all_pts$scenario) <- list("baseline" = "0", "100" = "100", "200" = "200",
                                  "300" = "300", "723" = "723", 
                                 "max (1648)" = "1648")
all_times_df$ttimes <- ifelse(is.infinite(all_times_df$ttimes), NA, all_times_df$ttimes)
  
S5.2A <- ggplot() + 
  geom_raster(data = all_times_df, aes(x = x, y = y, 
                                       fill = cut(ttimes/60, breaks = ttime_breaks, labels = ttime_labs))) + 
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
  geom_point(data = all_pts, aes(x = long, y = lat), shape = ".", color = "grey50", alpha = 0.75) +  
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

# Save as pdf otherwise too slow! 
# ggsave("figs/supplementary/S5.2.jpeg", dpi = 300, S5.2, height = 7, width = 7)
ggsave("figs/supplementary/S5.2.tiff", S5.2, height = 7, width = 7, compression = "lzw",
       device = "tiff", dpi = 300, type = "cairo")
# ggsave("figs/supplementary/S5.2.pdf", S5.2, height = 7, width = 7)

# Histogram of proportion of people living within x hours ---------------------------------------
pop_total <- sum(getValues(pop_1x1), na.rm = TRUE)
all_times_df %>%
  filter(!is.na(pop), !is.infinite(ttimes), !is.na(ttimes)) %>%
  mutate(cut_times = cut(ttimes/60, breaks = c(-0.1, 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, Inf),
                         labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-8", "8-10",
                                    "10-12", "12-15", "15+"))) %>%
  group_by(cut_times, scenario) %>%
  summarize(prop_pop = sum(pop, na.rm = TRUE)/pop_total) -> prop_pop_ttimes
write.csv(prop_pop_ttimes, "output/stats/prop_pop_ttimes.csv", row.names = FALSE)

S5.3 <- ggplot(data = prop_pop_ttimes, aes(x = cut_times, y = prop_pop)) +
  geom_col() +
  ylim(c(0, 0.75)) +
  labs(x = "Travel times (hrs)", y = "Proportion of population") +
  facet_wrap(~scenario) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figs/supplementary/S5.3.jpeg", S5.3, height = 5, width = 7)

# Shifts in proportion of people in single catchment -----------------------------------------
# Single catchment
scenario_filtered <- filter(scenario_to_plot, scenario %in% c(0, 100, 200, 300, max_added,
                                                              max(scenario_to_plot$scenario)))
S5.4 <- ggplot(data = scenario_filtered, aes(x = prop_pop_catch, y = as.factor(scenario), fill = scale)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  scale_color_manual(values = model_cols) +
  scale_fill_manual(values = model_cols) +
  scale_y_discrete(labels = c("baseline", 100, 200, 300, max_added, "max (1648)")) +
  labs(y = "# Additional ARMC", x = "Proportion of population \n in catchment") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figs/supplementary/S5.4.jpeg", S5.4, height = 5, width = 7)

# Decreases in ttimes, bite incidence, and reporting ------------------------------------
scenario_to_plot$scenario[scenario_to_plot$scenario == max(scenario_to_plot$scenario)] <- max_total
scenario_to_plot %>%
  group_by(scenario, scale) %>%
  summarize(ttimes_mean = mean(ttimes_wtd/60), 
            ttimes_lower25 = quantile(ttimes_wtd/60, probs = 0.025),
            ttimes_upper75 = quantile(ttimes_wtd/60, probs = 0.975)) -> scenario_ttimes

admin_preds <- fread("output/preds/burden_all.gz")
admin_preds$scenario[admin_preds$scenario == max(admin_preds$scenario)] <- max_total
admin_preds  %>%
  filter(scale != "") %>%
  group_by(scenario, scale) %>%
  summarize(reporting_upper75 = quantile(reporting_mean, probs = 0.025),
            reporting_lower25 = quantile(reporting_mean, probs = 0.975),
            reporting_mean = mean(reporting_mean), 
            bites_upper75 = quantile(bites_mean/pop*1e5, probs = 0.025),
            bites_lower25 = quantile(bites_mean/pop*1e5, probs = 0.975), 
            bites_mean = mean(bites_mean/pop*1e5), 
            deaths_upper75 = quantile(deaths_mean/pop*1e5, probs = 0.025),
            deaths_lower25 = quantile(deaths_mean/pop*1e5, probs = 0.975),
            deaths_mean = mean(deaths_mean/pop*1e5)) -> trend_preds

preds_filtered <- filter(admin_preds, scenario %in% c(0, 100, 200, 400, max_added,
                                                      max(admin_preds$scenario)))

# Shifts in ttimes and bite incidence ------------------------------------
ttimes_dist <- ggplot(data = preds_filtered, 
                aes(x = ttimes, y = as.factor(scenario), fill = scale)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(y = "# Additional ARMC", x = "Travel times (hrs)", tag = "A") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bites_dist <- ggplot(data = preds_filtered, 
                aes(x = bites_mean/pop*1e5, y = as.factor(scenario), fill = scale)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(y = "", x = "Reported bites \n per 100k", tag = "B") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ttimes_mean <- ggplot(data = filter(scenario_ttimes, scenario != max_total), 
                      aes(x = scenario, y = ttimes_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ttimes_lower25, ymax = ttimes_upper75), color = NA, alpha = 0.35) +
  geom_point(data = filter(scenario_ttimes, scenario == max_total), 
             aes(x = scenario, y = ttimes_mean, color = scale)) +
  geom_pointrange(data = filter(scenario_ttimes, scenario == max_total),
                  aes(ymin = ttimes_lower25, ymax = ttimes_upper75, color = scale)) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Travel times (hrs)") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bites_mean <- ggplot(data = filter(trend_preds, scenario != max_total), 
                     aes(x = scenario, y = bites_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = bites_lower25, ymax = bites_upper75), color = NA, alpha = 0.35) +
  geom_point(data = filter(trend_preds, scenario == max_total), 
             aes(x = scenario, y = bites_mean, color = scale)) +
  geom_pointrange(data = filter(trend_preds, scenario == max_total),
                  aes(ymin = bites_lower25, ymax = bites_upper75, color = scale)) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Bites per 100k") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.5A <- (ttimes_dist | ttimes_mean) + plot_layout(widths = c(1.5, 1))
S5.5B <- (bites_dist | bites_mean) + plot_layout(widths = c(1.5, 1))

S5.5_ttimes_shifts <- S5.5A / S5.5B 
ggsave("figs/supplementary/S5.5_ttimes_shifts.jpeg", S5.5_ttimes_shifts, width = 7, height = 7)

# Shifts in reporting and death incidence ------------------------------------
reporting_dist <- ggplot() +
  geom_density_ridges(data = preds_filtered, 
                      aes(x = reporting_mean, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(y = "# Additional ARMC", x = "Reporting", tag = "A") +
  xlim(c(0, 1)) +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

deaths_dist <- ggplot() +
  geom_density_ridges(data = preds_filtered, 
                      aes(x = deaths_mean/pop*1e5, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(y = "", x = "Deaths per 100k", tag = "B") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

reporting_mean <- ggplot(data = filter(trend_preds, scenario != max_total), 
                aes(x = scenario, y = reporting_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = reporting_lower25, ymax = reporting_upper75), color = NA, alpha = 0.35) +
  geom_point(data = filter(trend_preds, scenario == max_total), 
             aes(x = scenario, y = reporting_mean, color = scale)) +
  geom_pointrange(data = filter(trend_preds, scenario == max_total),
                  aes(ymin = reporting_lower25, ymax = reporting_upper75, color = scale)) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Reporting") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

deaths_mean <- ggplot(data = filter(trend_preds, scenario != max_total), 
                aes(x = scenario, y = deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = deaths_lower25, ymax = deaths_upper75), color = NA, alpha = 0.35) +
  geom_point(data = filter(trend_preds, scenario == max_total), 
             aes(x = scenario, y = deaths_mean, color = scale)) +
  geom_pointrange(data = filter(trend_preds, scenario == max_total),
                  aes(ymin = deaths_lower25, ymax = deaths_upper75, color = scale)) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Deaths per 100k") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.6A <- (reporting_dist | reporting_mean) + plot_layout(widths = c(1.5, 1))
S5.6B <- (deaths_dist | deaths_mean) + plot_layout(widths = c(1.5, 1))

S5.6_reporting_shifts <- S5.5A / S5.5B 
ggsave("figs/supplementary/S5.6_reporting_shifts.jpeg", S5.6_reporting_shifts, width = 7, height = 7)

# write out stats
trend_preds %>%
  left_join(scenario_ttimes) -> trend_preds
write.csv(trend_preds, "output/stats/trend_preds.csv", row.names = FALSE)

# Shifts in clinic stats ----------------------------------------------------------------
commune_allcatch <- fread("output/ttimes/commune_allcatch.gz")
commune_allcatch %>%
  group_by(catchment, scenario) %>%
  summarize(pop_catch = sum(prop_pop_catch*pop)) %>%
  mutate(scale = "Commune") -> catch_pops

# Means of catchment pops, throughput, vials
catch_pops$scenario[catch_pops$scenario == max(catch_pops$scenario)] <- max_total
catch_pops  %>%
  filter(scale != "") %>%
  group_by(scenario, scale) %>%
  summarize(pops_lower = quantile(pop_catch, probs = 0.025),
            pops_upper = quantile(pop_catch, probs = 0.975),
            pops_mean = mean(pop_catch)) -> trend_catch_pops

# Other means 
bites_by_catch <- fread("output/preds/catch_preds.gz")
bites_by_catch %>%
  group_by(catchment, scenario, scale) %>%
  summarize(tp_mean = mean(tp_mean), 
            tp_upper = quantile(tp_mean, probs = 0.975),
            tp_lower = quantile(tp_mean, probs = 0.025),
            vials_mean = mean(vials_mean), 
            vials_upper = quantile(vials_mean, probs = 0.975),
            vials_lower = quantile(vials_mean, probs = 0.025)) -> catch_means

# Filtered scenarios
catch_pops_filtered <- filter(catch_pops, scenario %in% c(0, 100, 200, 400, max_added, max_total))
bites_by_catch_filtered <- filter(bites_by_catch, 
                                  scenario %in% c(0, 100, 200, 400, max_added, max_total))

# Catchment pops distribution
catchpop_dist <- ggplot() +
  geom_density_ridges(data = catch_pops_filtered, 
                      aes(x = pop_catch, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  scale_x_continuous(trans = "log", breaks = c(100, 1e3, 1e4, 1e5, 1e6)) +
  labs(y = "# Additional ARMC", x = "Catchment pop size", tag = "A") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

catchpop_mean <- ggplot(data = filter(trend_catch_pops, scenario != max_total), 
                         aes(x = scenario, y = pops_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = pops_lower, ymax = pops_upper), color = NA, alpha = 0.35) +
   geom_pointrange(data = filter(trend_catch_pops, scenario == max_total),
                  aes(ymin = pops_lower, ymax = pops_upper, color = scale)) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  scale_y_continuous(trans = "log", limits = c(1e3, 1e7), breaks = c(1e3, 1e4, 1e5, 1e6)) +
  labs(x = "# Additional ARMC", y = "Catchment pop size") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vials
vials_dist <- ggplot() +
  geom_density_ridges(data = bites_by_catch, 
                      aes(x = vials_mean, y = as.factor(scenario), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_y_discrete(labels = c("baseline", 100, 200, 400, max_added, "max (1648)")) +
  scale_x_continuous(trans = "sqrt", limits = c(0, 4e4), breaks = c(100, 1e3, 1e4, 2e4, 4e4)) +
  labs(y = "# Additional ARMC", x = "Catchment pop size", tag = "A") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vials_mean <- ggplot(data = filter(catch_means, scenario != max_total), 
                        aes(x = scenario, y = vials_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vials_lower, ymax = vials_upper), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(catch_means, scenario == max_total),
                  aes(ymin = vials_lower, ymax = vials_upper, color = scale)) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 300, 400, max_added, "max (1648)")) +
  scale_y_continuous(trans = "sqrt") +
  labs(x = "# Additional ARMC", y = "Catchment pop size") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dist of catchment pops, throughput, vials

# Catchment pops

# Throughput 

# Vials

# assemble it

# save it

# Commune only because district maxes at 1 clinic per district
master_commune <- fread("output/ttimes/master_commune.csv")
master_commune %>%
  group_by(base_catches, scenario) %>%
  summarize(catch_pop = sum(pop, na.rm = TRUE)) %>%
  mutate(scale = "Commune") %>%
  group_by(scenario) %>%
  summarize(catch_pop_upper75 = quantile(catch_pop, probs = 0.025),
            catch_pop_lower25 = quantile(catch_pop, probs = 0.975),
            catch_pop_mean = mean(catch_pop)) -> pop_by_catch_comm
pop_by_catch_comm$scenario[pop_by_catch_comm$scenario == max(pop_by_catch_comm$scenario)] <- max_total

# Plot means
S5.7A <- ggplot(data = filter(pop_by_catch_comm, scenario != max_total), 
                aes(x = scenario, y = catch_pop_mean), color = model_cols["Commune"]) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = catch_pop_lower25, ymax = catch_pop_upper75), color = NA,
              fill = model_cols["Commune"], alpha = 0.35) +
  geom_point(data = filter(pop_by_catch_comm, scenario == max_total), 
             aes(x = scenario, y = catch_pop_mean), color = model_cols["Commune"]) +
  geom_pointrange(data = filter(pop_by_catch_comm, scenario == max_total),
                  aes(ymin = catch_pop_lower25, ymax = catch_pop_upper75)) +
  scale_y_continuous(breaks = c(10000, 50000, 1e5, 2.5e5, 5e5, 7.5e5)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 300, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Catchment population size",
       tag = "A") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Output for pulling in stats
write.csv(pop_by_catch_comm, "output/stats/pop_by_catch.csv", row.names = FALSE)

# Throughput + vials
vial_preds <- fread("output/preds/complete/vials_filled.csv")
vial_preds %>%
  group_by(scenario, scale) %>%
  summarize(throughput_upper75 = quantile(throughput_mean, probs = 0.025, na.rm = TRUE),
            throughput_lower25 = quantile(throughput_mean, probs = 0.975, na.rm = TRUE),
            throughput_mean = quantile(throughput_mean, probs = 0.5, na.rm = TRUE),
            vials_upper75 = quantile(vials_mean, probs = 0.025, na.rm = TRUE),
            vials_lower25 = quantile(vials_mean, probs = 0.975, na.rm = TRUE),
            vials_mean = quantile(vials_mean, probs = 0.5, na.rm = TRUE)) -> vials_summed
vials_summed$scenario[vials_summed$scenario == max(vials_summed$scenario)] <- max_total
write.csv(vials_summed, "output/stats/vials_summed.csv", row.names = FALSE)

S5.7B <- ggplot(data = filter(vials_summed, scenario != max_total), 
                aes(x = scenario, y = throughput_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = throughput_lower25, ymax = throughput_upper75), color = NA, alpha = 0.35) +
  geom_point(data = filter(vials_summed, scenario == max_total), 
             aes(x = scenario, y = throughput_mean, color = scale)) +
  geom_pointrange(data = filter(vials_summed, scenario == max_total),
                  aes(ymin = throughput_lower25, ymax = throughput_upper75, color = scale)) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_linetype_discrete(guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 300, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Daily clinic throughput",
       tag = "B") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.7C <- ggplot(data = filter(vials_summed, scenario != max_total), 
                aes(x = scenario, y = vials_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vials_lower25, ymax = vials_upper75), color = NA, alpha = 0.35) +
  geom_point(data = filter(vials_summed, scenario == max_total), 
             aes(x = scenario, y = vials_mean, color = scale)) +
  geom_pointrange(data = filter(vials_summed, scenario == max_total),
                  aes(ymin = vials_lower25, ymax = vials_upper75, color = scale)) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_linetype_discrete(guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, max_added, max_total), 
                     labels = c("baseline", 100, 200, 300, 400, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Annual vial demand",
       tag = "C") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.7 <- S5.7A / S5.7B / S5.7C + plot_layout(guides = "collect")
ggsave("figs/supplementary/S5.7.jpeg", S5.7, height = 12, width = 6)

# Session Info
out.session(path = "R/SM5?.R", filename = "output/log_local.csv")


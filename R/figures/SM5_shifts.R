# ------------------------------------------------------------------------------------------------ #
#' Plotting scenario shifts        
#' Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

start <- start()

# Libraries and packages
library(data.table)
library(tidyverse)
library(rgdal)
library(ggridges)
library(patchwork)
library(raster)
library(rasterVis)
library(cowplot)
library(glue)
library(gdistance)
library(foreach)
select <- dplyr::select
source("R/functions/ttime_functions.R")
source("R/functions/out.session.R")

# Pull in data
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
csbs <- read.csv("data/processed/clinics/csb2.csv", stringsAsFactors = FALSE)

mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")

friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
base_times <- raster("output/ttimes/base_ttimes.tif")
pop_1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")

clins_per_comm <- nrow(read.csv("data/processed/clinics/clinic_per_comm.csv"))
prop_df <- fread("output/ttimes/addclinics_prop_df.csv")
max_added <- nrow(prop_df)
max_csb <- nrow(csbs)
scenario_labs <- c("Baseline\n (N = 31)", glue("1 per district\n (+ {114 - 31})"), "+ 200", "+ 400", 
                   "+ 600", glue("+ {max_added}"), 
                   glue("1 per commune\n (+ {clins_per_comm - 31})"),
                   glue("All CSB II\n (+ {max_csb})"))
scenario_levs <- c(0, 114.5, 200, 400, 600, max_added, max_added + 150.5, max_added + 200)
names(scenario_labs) <- scenario_levs

# Commune/District preds
commune_maxcatch <- fread("output/ttimes/commune_maxcatch.gz")
commune_maxcatch %>%
  mutate(scenario_num = 
           case_when(!(scenario %in% c("max", "armc_per_dist", "armc_per_comm")) ~ as.numeric(scenario),
                     scenario == "max" ~ max_added + 200, scenario == "armc_per_comm" ~ max_added + 150.5,
                     scenario == "armc_per_dist" ~ 114.5)) -> commune_maxcatch
district_maxcatch <- fread("output/ttimes/district_maxcatch.gz")
district_maxcatch %>%
  mutate(scenario_num = 
           case_when(!(scenario %in% c("max", "armc_per_dist", "armc_per_comm")) ~ as.numeric(scenario),
                     scenario == "max" ~ max_added + 200, scenario == "armc_per_comm" ~ max_added + 150.5,
                     scenario == "armc_per_dist" ~ 114.5)) -> district_maxcatch

# Single catchment
district_maxcatch$scale <- "District"
commune_maxcatch$scale <- "Commune"
scenario_to_plot <- rbind(data.table(district_maxcatch), data.table(commune_maxcatch), fill = TRUE)

# Scale
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

# Clinics added ------------------------------------------------------------------------------
prop_df$step_added <- 1:nrow(prop_df)

# candidate points
point_mat_all <- rbind(dplyr::select(ctar_metadata, long = LONGITUDE, lat = LATITUDE, clinic_id),
                       dplyr::select(csbs, long, lat, clinic_id))

point_mat_all %>%
  left_join(prop_df) %>%
  mutate(prop_pop = case_when(is.na(prop_pop) & clinic_id < 32 ~ 0,
                              is.na(prop_pop) & clinic_id > 32 ~ 1e-12,
                              !is.na(prop_pop) ~ prop_pop),
         step_added = case_when(is.na(step_added) & clinic_id < 32 ~ 0,
                                !is.na(step_added) ~ as.numeric(step_added))) -> point_mat_all

gg_commune <- fortify(mada_communes, region = "commcode")

S5.1_ARMCadded <- ggplot() +
  geom_polygon(data = gg_commune, aes(x = long, y = lat, group = group), fill = "black") +
  geom_point(data = filter(point_mat_all, is.na(step_added)),
             aes(x = long, y = lat), color = "white",
             alpha = 0.5, shape = 3) +
  geom_point(data = filter(point_mat_all, step_added > 0),
             aes(x = long, y = lat, size = prop_pop, fill = step_added), color = "grey50", 
             alpha = 0.75, shape = 21) +
  geom_point(data = filter(point_mat_all, step_added == 0),
             aes(x = long, y = lat), shape = 3, size = 2, stroke = 1.2, color = "white") +
  coord_quickmap() +
  scale_fill_distiller(trans = "sqrt", direction = -1, palette = "PuRd", na.value = "grey50",
                       breaks = c(1, 100, 200, 400, max(point_mat_all$step_added, na.rm = TRUE)),
                       labels = c(1, 100, 300, 400, "720 (max added)"),
                                  name = "Step clinic added") +
  scale_size_continuous(name = "Proportion of pop \n for which travel times reduced)") +
  theme_map()

ggsave("figs/supplementary/S5.1_ARMCadded.jpeg", S5.1_ARMCadded, width = 6, height = 8)

# Plots of how ttimes change ----------------------------------------------------------------
gg_commune <- fortify(mada_communes, region = "commcode")
gg_district <- fortify(mada_districts, region = "distcode")

scenario_to_plot %>%
  filter(scenario_num %in% scenario_levs, 
         scale == "District") %>%
  pivot_wider(id_cols = distcode, names_from = scenario_num, values_from = ttimes_wtd) %>%
  right_join(gg_district, by = c("distcode" = "id")) %>%
  pivot_longer(2:(length(scenario_levs) + 1), names_to = "scenario") -> gg_district_plot

scenario_to_plot %>%
  filter(scenario_num %in% scenario_levs, 
         scale == "Commune") %>%
  pivot_wider(id_cols = commcode, names_from = scenario_num, values_from = ttimes_wtd) %>%
  right_join(gg_commune, by = c("commcode" = "id")) %>%
  pivot_longer(2:(length(scenario_levs) + 1), names_to = "scenario") -> gg_commune_plot

ttime_cols <- c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
ttime_breaks <- c(-0.1, 1, 2, 4, 6, 8, 10, 12, Inf)
ttime_labs <- c("< 1", "< 2", "< 4", "< 6", "< 8", "< 10", "< 15", "15 +")
names(ttime_cols) <- ttime_labs

# Getting ttime rasters
commune_maxcatch %>%
  group_by(clinic_added) %>%
  summarize(when_added = min(scenario_num)) %>%
  right_join(csbs, by = c("clinic_added" = "clinic_id")) %>%
  mutate(when_added = ifelse(is.na(when_added), max_added + 200, when_added)) -> when_added
ctar_metadata %>%
  select(CTAR, lat = LATITUDE, long = LONGITUDE, commcode, distcode) %>%
  mutate(clinic_added = 1:31, when_added = 0) %>%
  bind_rows(when_added) -> all_added
steps <- c(0, 200, 400, 600, max_added, max_added + 200)
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

# special scenarios
clin_per_comm <- raster("output/ttimes/clin_per_comm_ttimes.tif")
names(clin_per_comm) <- "ttimes"
clin_per_comm <- as.data.frame(clin_per_comm, xy = TRUE)
clin_per_comm$scenario <- max_added + 150.5
clin_per_comm$pop <- getValues(pop_1x1)
clin_per_dist <- raster("output/ttimes/clin_per_dist_ttimes.tif")
names(clin_per_dist) <- "ttimes"
clin_per_dist <- as.data.frame(clin_per_dist, xy = TRUE)
clin_per_dist$scenario <- 114.5
clin_per_dist$pop <- getValues(pop_1x1)

pts_per_comm <- read.csv("data/processed/clinics/clinic_per_comm.csv")
pts_per_comm$scenario <- max_added + 150.5
pts_per_dist<- read.csv("data/processed/clinics/clinic_per_dist.csv")
pts_per_dist$scenario <- 114.5
all_pts <- bind_rows(all_pts, pts_per_comm, pts_per_dist)

all_times_df <- bind_rows(clin_per_dist, clin_per_comm, all_times_df)
all_times_df$scenario <- factor(all_times_df$scenario)
all_times_df$ttimes <- ifelse(is.infinite(all_times_df$ttimes), NA, all_times_df$ttimes)

S5.2A <- ggplot() + 
  geom_raster(data = all_times_df, aes(x = x, y = y, 
                                       fill = cut(ttimes/60, breaks = ttime_breaks, 
                                                  labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, guide = "none") +
  geom_point(data = all_pts, aes(x = long, y = lat), color = "grey50", alpha = 0.75,
             shape = ".") +
  facet_wrap(~ scenario, nrow = 1, labeller = as_labeller(scenario_labs)) +
  theme_void() +
  coord_cartesian(clip = "off") +
  labs(tag = "A") +
  theme(strip.text.x = element_text(hjust = 0.5, margin = margin(0.1, 0.1, 0.1, 0.1, "cm")))

S5.2B <- ggplot() +
  geom_polygon(data = gg_commune_plot, aes(x = long, y = lat, group = group, 
                                           fill = cut(value/60, breaks = ttime_breaks, 
                                                      labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  geom_point(data = all_pts, aes(x = long, y = lat), shape = ".", color = "grey50", alpha = 0.75) +  
  facet_wrap(~ scenario, nrow = 1, labeller = as_labeller(scenario_labs)) +
  theme_void() +
  coord_cartesian(clip = "off") +
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
  facet_wrap(~ scenario, nrow = 1, labeller = as_labeller(scenario_labs)) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(strip.text.x = element_blank()) +
  labs(tag = "C")  

S5.2 <- S5.2A / S5.2B / S5.2C

# Save as tiff otherwise too slow! 
ggsave("figs/supplementary/S5.2_map_shifts.tiff", S5.2, height = 8, width = 10, compression = "lzw",
       device = "tiff", dpi = 300, type = "cairo")

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
  facet_wrap(~ scenario, labeller = as_labeller(scenario_labs),
             nrow = 2) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figs/supplementary/S5.3_prop_ttimes.jpeg", S5.3, height = 7, width = 8)

# Shifts in proportion of people in single catchment -----------------------------------------
# Single catchment
scenario_filtered <- filter(scenario_to_plot, scenario_num %in% scenario_levs)
ggplot(data = scenario_filtered, aes(x = prop_pop_catch, y = factor(scenario_num), fill = scale)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_y_discrete(labels = scenario_labs) +
  labs(y = "# Additional ARMC", x = "Proportion of population \n in catchment") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> S5.4

ggsave("figs/supplementary/S5.4_prop_catch.jpeg", S5.4, height = 5, width = 7)

# Decreases in ttimes, bite incidence, and reporting ------------------------------------
scenario_to_plot %>%
  group_by(scenario_num, scenario, scale) %>%
  summarize(ttimes_mean = mean(ttimes_wtd/60), 
            ttimes_lower25 = quantile(ttimes_wtd/60, probs = 0.025),
            ttimes_upper75 = quantile(ttimes_wtd/60, probs = 0.975)) -> scenario_ttimes

admin_preds <- fread("output/preds/burden_all.gz")
admin_preds  %>%
  mutate(scenario_num = case_when(!(scenario %in% c("max", "armc_per_dist", 
                                                    "armc_per_comm")) ~ as.numeric(scenario),
                     scenario == "max" ~ max_added + 200, scenario == "armc_per_comm" ~ max_added + 150.5,
                     scenario == "armc_per_dist" ~ 114.5)) -> admin_preds
admin_preds %>%   
  filter(scale != "") %>%
  group_by(scenario_num, scenario, scale) %>%
  summarize(reporting_upper75 = quantile(reporting_mean, probs = 0.025),
            reporting_lower25 = quantile(reporting_mean, probs = 0.975),
            reporting_mean = mean(reporting_mean), 
            bites_upper75 = quantile(bites_mean/pop*1e5, probs = 0.025),
            bites_lower25 = quantile(bites_mean/pop*1e5, probs = 0.975), 
            bites_mean = mean(bites_mean/pop*1e5), 
            deaths_upper75 = quantile(deaths_mean/pop*1e5, probs = 0.025),
            deaths_lower25 = quantile(deaths_mean/pop*1e5, probs = 0.975),
            deaths_mean = mean(deaths_mean/pop*1e5)) -> trend_preds

preds_filtered <- filter(admin_preds, scenario_num %in% scenario_levs)

# Shifts in ttimes and bite incidence ------------------------------------
ttimes_dist <- ggplot(data = preds_filtered, 
                      aes(x = ttimes, y = as.factor(scenario_num), fill = scale)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  labs(y = "# Additional ARMC", x = "Travel times (hrs)", tag = "A") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bites_dist <- ggplot(data = preds_filtered, 
                     aes(x = bites_mean/pop*1e5, y = as.factor(scenario_num), fill = scale)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  labs(y = "# Additional ARMC", x = "Reported bites \n per 100k", tag = "B") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ttimes_mean <- ggplot(data = filter(scenario_ttimes, !(scenario %in% c("max", "armc_per_comm", 
                                                                       "armc_per_dist"))), 
                      aes(x = scenario_num, y = ttimes_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ttimes_lower25, ymax = ttimes_upper75), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(scenario_ttimes, scenario %in% c("max", "armc_per_comm", 
                                                            "armc_per_dist")), 
             aes(x = scenario_num, y = ttimes_mean, ymin = ttimes_lower25, ymax = ttimes_upper75,
                 color = scale, shape = scenario), position = position_dodge(width = 50)) +
  scale_fill_manual(aesthetics = c("color", "fill"), values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  scale_shape_discrete(guide = "none") + 
  labs(x = "# Additional ARMC", y = "Travel times (hrs)") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bites_mean <- ggplot(data = filter(trend_preds, !(scenario %in% c("max", "armc_per_comm", 
                                                                       "armc_per_dist"))), 
                      aes(x = scenario_num, y = bites_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = bites_lower25, ymax = bites_upper75), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(trend_preds, scenario %in% c("max", "armc_per_comm", 
                                                                 "armc_per_dist")), 
                  aes(x = scenario_num, y = bites_mean, ymin = bites_lower25, ymax = bites_upper75,
                      color = scale, shape = scenario), position = position_dodge(width = 50)) +
  scale_fill_manual(aesthetics = c("color", "fill"), values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  scale_shape_discrete(labels = c("max" = "All CSB II \n (+ 1648)",
                                  "armc_per_dist" = "1 per district\n (+ 83)",
                                  "armc_per_comm" = "1 per commune\n (+ 1375)"), 
                       name = "Additional\n scenarios") + 
  labs(x = "# Additional ARMC", y = "Bites per 100k") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.5A <- (ttimes_dist | ttimes_mean) 
S5.5B <- (bites_dist | bites_mean) 

S5.5_ttimes_shifts <- S5.5A / S5.5B + plot_layout(guides = "collect") 
ggsave("figs/supplementary/S5.5_ttimes_shifts.jpeg", S5.5_ttimes_shifts, width = 10, height = 10)

# Shifts in reporting and death incidence ------------------------------------
reporting_dist <- ggplot() +
  geom_density_ridges(data = preds_filtered, 
                      aes(x = reporting_mean, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  labs(y = "# Additional ARMC", x = "Reporting", tag = "A") +
  xlim(c(0, 1)) +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

deaths_dist <- ggplot() +
  geom_density_ridges(data = preds_filtered, 
                      aes(x = deaths_mean/pop*1e5, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  labs(y = "# Additional ARMC", x = "Deaths per 100k", tag = "B") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

reporting_mean <- ggplot(data = filter(trend_preds, !(scenario %in% c("max", "armc_per_comm", 
                                                  "armc_per_dist"))), 
                         aes(x = scenario_num, y = reporting_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = reporting_lower25, ymax = reporting_upper75), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(trend_preds, scenario %in% c("max", "armc_per_comm", 
                                                             "armc_per_dist")), 
                  aes(x = scenario_num, y = reporting_mean, 
                      ymin = reporting_lower25, ymax = reporting_upper75, color = scale, 
                      shape = scenario), position = position_dodge(width = 50)) +
  scale_fill_manual(aesthetics = c("color", "fill"), values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  scale_shape_discrete(guide = "none") + 
  labs(x = "# Additional ARMC", y = "Reporting") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

deaths_mean <- ggplot(data = filter(trend_preds, !(scenario %in% c("max", "armc_per_comm", 
                                                                   "armc_per_dist"))), 
                      aes(x = scenario_num, y = deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = deaths_lower25, ymax = deaths_upper75), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(trend_preds, scenario %in% c("max", "armc_per_comm", 
                                                             "armc_per_dist")), 
                  aes(x = scenario_num, y = deaths_mean, 
                      ymin = deaths_lower25, ymax = deaths_upper75, color = scale, 
                      shape = scenario), position = position_dodge(width = 50)) +  
  scale_fill_manual(aesthetics = c("color", "fill"), values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  scale_shape_discrete(labels = c("max" = "All CSB II \n (+ 1648)",
                                  "armc_per_dist" = "1 per district\n (+ 83)",
                                  "armc_per_comm" = "1 per commune\n (+ 1375)"), name = "Additional\n scenarios") + 
  labs(x = "# Additional ARMC", y = "Deaths per 100k") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.6A <- (reporting_dist | reporting_mean) 
S5.6B <- (deaths_dist | deaths_mean) 
S5.6_reporting_shifts <- S5.6A / S5.6B + plot_layout(guides = "collect") 
ggsave("figs/supplementary/S5.6_reporting_shifts.jpeg", S5.6_reporting_shifts, width = 10, height = 10)

# write out stats
trend_preds %>%
  left_join(scenario_ttimes) -> trend_preds
write.csv(trend_preds, "output/stats/trend_preds.csv", row.names = FALSE)

# Shifts in clinic stats ----------------------------------------------------------------
commune_allcatch <- fread("output/ttimes/commune_allcatch.gz")
commune_allcatch %>%
  mutate(scenario_num = 
           case_when(!(scenario %in% c("max", "armc_per_dist", "armc_per_comm")) ~ as.numeric(scenario),
                     scenario == "max" ~ max_added + 200, scenario == "armc_per_comm" ~ max_added + 150.5,
                     scenario == "armc_per_dist" ~ 114.5)) %>%
  group_by(catchment, scenario, scenario_num) %>%
  summarize(pop_catch = sum(prop_pop_catch*pop)) %>%
  mutate(scale = "Commune") -> catch_pops

# Means of catchment pops, throughput, vials
catch_pops  %>%
  filter(scale != "") %>%
  group_by(scenario, scenario_num, scale) %>%
  summarize(pops_lower = quantile(pop_catch, probs = 0.025),
            pops_upper = quantile(pop_catch, probs = 0.975),
            pops_mean = mean(pop_catch)) -> trend_catch_pops

bites_by_catch <- fread("output/preds/catch_preds.gz")
bites_by_catch %>%
  mutate(scenario_num = 
           case_when(!(scenario %in% c("max", "armc_per_dist", "armc_per_comm")) ~ as.numeric(scenario),
                     scenario == "max" ~ max_added + 200, scenario == "armc_per_comm" ~ max_added + 150.5,
                     scenario == "armc_per_dist" ~ 114.5)) -> bites_by_catch
bites_by_catch %>%
  group_by(scenario, scenario_num, scale) %>%
  summarize(tp_upper = quantile(tp_mean, probs = 0.975),
            tp_lower = quantile(tp_mean, probs = 0.025),
            tp_mean = mean(tp_mean), 
            vials_upper = quantile(vials_mean, probs = 0.975),
            vials_lower = quantile(vials_mean, probs = 0.025),
            vials_mean = mean(vials_mean)) -> catch_means
catch_trends <- left_join(trend_catch_pops, catch_means)
write.csv(catch_trends, "output/stats/trend_catches.csv", row.names = FALSE)

# Filtered scenarios
catch_pops_filtered <- filter(catch_pops, scenario_num %in% scenario_levs)
bites_by_catch_filtered <- filter(bites_by_catch, 
                                  scenario_num %in% scenario_levs)

# Catchment pops distribution
catchpop_dist <- ggplot() +
  geom_density_ridges(data = catch_pops_filtered, 
                      aes(x = pop_catch, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale", guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  scale_x_continuous(trans = "log", breaks = c(100, 1e3, 1e4, 1e5, 1e6)) +
  labs(y = "# Additional ARMC", x = "Catchment pop size", tag = "A") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

catchpop_mean <- ggplot(data = filter(trend_catch_pops, !(scenario %in% c("max", "armc_per_comm", 
                                                                          "armc_per_dist"))), 
                        aes(x = scenario_num, y = pops_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = pops_lower, ymax = pops_upper), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(trend_catch_pops, scenario %in% c("max", "armc_per_comm", 
                                                             "armc_per_dist")), 
                  aes(ymin = pops_lower, ymax = pops_upper, color = scale, shape = scenario),
                  position = position_dodge(width = 50)) +
  scale_fill_manual(aesthetics = c("color", "fill"), values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  scale_y_continuous(trans = "log", limits = c(1e3, 1e7), breaks = c(1e3, 1e4, 1e5, 1e6)) +
  scale_shape(guide = "none") +
  labs(x = "# Additional ARMC", y = "Catchment pop size (average)") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

catchpop_A <- catchpop_dist | catchpop_mean

# Vials
vials_dist <- ggplot() +
  geom_density_ridges(data = bites_by_catch_filtered, 
                      aes(x = vials_mean, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale", 
                    guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  scale_x_continuous(trans = "sqrt", limits = c(0, 4e4), breaks = c(100, 1e3, 1e4, 2e4, 4e4)) +
  labs(y = "# Additional ARMC", x = "Annual clinic vial demand", tag = "B") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vials_mean <- ggplot(data = filter(catch_means, !(scenario %in% c("max", "armc_per_comm", 
                                                                  "armc_per_dist"))), 
                     aes(x = scenario_num, y = vials_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vials_lower, ymax = vials_upper), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(catch_means, scenario %in% c("max", "armc_per_comm", 
                                                             "armc_per_dist")),
                  aes(y = vials_mean, ymin = vials_lower, ymax = vials_upper, color = scale, 
                      shape = scenario)) +
  scale_fill_manual(aesthetics =c ("color", "fill"), 
                    values = model_cols, labels = scale_labs, name = "Scale") +
  scale_shape_discrete(labels = c("max" = "All CSB II \n (+ 1648)",
                                  "armc_per_dist" = "1 per district\n (+ 83)",
                                  "armc_per_comm" = "1 per commune\n (+ 1375)"), name = "Additional\n scenarios") + 
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  labs(x = "# Additional ARMC", y = "Annual clinic vial demand (average)") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vials_B <- vials_dist | vials_mean

# Throughput
tp_dist <- ggplot() +
  geom_density_ridges(data = bites_by_catch_filtered, 
                      aes(x = tp_mean, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale", 
                    guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  scale_x_continuous(trans = "sqrt") +
  labs(y = "# Additional ARMC", x = "Daily clinic throughput", tag = "C") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tp_mean <- ggplot(data = filter(catch_means, !(scenario %in% c("max", "armc_per_comm", 
                                                               "armc_per_dist"))), 
                  aes(x = scenario_num, y = tp_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = tp_lower, ymax = tp_upper), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(catch_means, scenario %in% c("max", "armc_per_comm", 
                                                               "armc_per_dist")),
                  aes(ymin = tp_lower, ymax = tp_upper, color = scale, shape = scenario)) +
  scale_color_manual(aesthetics = c("color", "fill"),
                     values = model_cols, guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 400, max_added, max_added + 200), 
                     labels = c("Baseline", 100, 200, 400, max_added, "All CSB 2\n (+ 1648)")) +
  scale_shape(guide = "none") +
  scale_y_continuous(trans = "log", breaks = c(0, 0.5, 1, 10, 20, 40, 60)) +
  labs(x = "# Additional ARMC", y = "Daily clinic throughput (average)") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tp_C <- tp_dist | tp_mean

S5.7_clinic_shifts <- catchpop_A / vials_B / tp_C 
ggsave("figs/supplementary/S5.7_clinic_shifts.jpeg", S5.7_clinic_shifts, height = 12, width = 10)

# Bites by clinics added ----------------------------------------------------------------------
point_mat_all <- bind_rows(dplyr::select(ctar_metadata, long = LONGITUDE, lat = LATITUDE, clinic_id),
                       dplyr::select(csbs, long, lat, clinic_id),
                       pts_per_comm, pts_per_dist)
pts <- SpatialPoints(cbind(point_mat_all$long, point_mat_all$lat), 
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
point_mat_all$commcode_ctar <- over(pts, mada_communes)$commcode
point_mat_all$distcode_ctar <- over(pts, mada_communes)$distcode
point_mat_all %>%
  select(long, lat, clinic_id, commcode_ctar, distcode_ctar) %>%
  distinct() -> point_mat_all

# Get # of communes/districts with a clinic
bites_by_catch %>%
  left_join(point_mat_all,
            by = c("catchment" = "clinic_id")) -> bites_by_catch # clinic points
bites_by_catch %>%
  group_by(scenario, scenario_num, scale) %>%
  summarize(ncomms = length(unique(commcode_ctar)),
            ndists = length(unique(distcode_ctar))) -> clinics_per_admin
fwrite(clinics_per_admin, "output/stats/clinic_per_admin.csv")

# Prop of bites in catchment
props_by_catch <- fread("output/preds/catch_props.csv")
props_by_catch %>%
  mutate(scenario_num = case_when(!(scenario %in% c("max", "armc_per_dist", 
                                                    "armc_per_comm")) ~ as.numeric(scenario),
                                  scenario == "max" ~ max_added + 200, scenario == "armc_per_comm" ~ max_added + 150.5,
                                  scenario == "armc_per_dist" ~ 114.5)) -> props_by_catch
bites_by_catch <- filter(bites_by_catch, scenario_num %in% scenario_levs)
props_by_catch <- filter(props_by_catch, scenario_num %in% scenario_levs)

# left join with props by catch
clinic_bites <- left_join(bites_by_catch, props_by_catch) # need to make sure this is joined with clinic id as well!
clinic_bites <- left_join(clinic_bites, select(mada_communes@data, commcode, long_cent, lat_cent),
                            by = c("commcode" = "commcode"))
clinic_bites$line_size <- clinic_bites$prop_bites*clinic_bites$bites_mean

# Proportion of bites within admin units ------------------------------------------------------
clinic_bites$distcode <- substring(clinic_bites$commcode, 1, 7)
clinic_bites_incomm <- filter(clinic_bites, commcode_ctar == commcode)
clinic_bites %>%
  filter(distcode_ctar == distcode) %>%
  select(bites = line_size, bites_total = bites_mean, distcode, catchment, scale,
         scenario, scenario_num) %>%
  group_by(catchment, scenario_num, scale) %>%
  summarize(prop_bites = sum(bites)/bites_total[1]) -> clinic_bites_indist

# Commune scale
prop_comm <- ggplot() +
  geom_density_ridges(data = clinic_bites_incomm, 
                      aes(x = prop_bites, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_y_discrete(labels = scenario_labs) +
  scale_x_continuous(limits = c(0, 1.01)) +
  labs(y = "# Additional ARMC", x = "Proportion of bites \n reported from \n same commune as ARMC", 
       tag = "A") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop_dist <- ggplot() +
  geom_density_ridges(data = clinic_bites_indist, 
                      aes(x = prop_bites, y = as.factor(scenario_num), fill = scale), 
                      alpha = 0.5, color = NA) +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale", guide = "legend") +
  scale_y_discrete(labels = scenario_labs) +
  scale_x_continuous(limits = c(0, 1.01)) +
  labs(y = "", x = "Proportion of bites \n reported from \n same district as ARMC", tag = "B") +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S5.8_prop <- prop_comm / prop_dist + plot_layout(guides = "collect") & theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"))
ggsave("figs/supplementary/S5.8_prop_in_admin.jpeg", S5.8_prop, height = 8, width = 8)

# Session Info
out.session(path = "R/SM5_shifts.R", filename = "output/log_local.csv", start = start)

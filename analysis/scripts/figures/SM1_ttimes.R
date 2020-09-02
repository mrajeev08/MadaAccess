# ------------------------------------------------------------------------------------------------ #
#' SM 1. GIS inputs and comparison to data        
# ------------------------------------------------------------------------------------------------ #

source("R/functions/out.session.R")
start <- Sys.time()

# Libraries
library(data.table)
library(tidyverse)
library(sf)
library(lubridate)
library(raster)
library(patchwork)
library(ggridges)
library(cowplot)
library(ggforce)
select <- dplyr::select
source("R/functions/bezier.R")

# Read in data
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
mada_communes <- st_read("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- st_read("data/processed/shapefiles/mada_districts_simple.shp")

# Input rasters
friction <- raster("data/processed/rasters/friction_mada_masked.tif")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")

# Plotting GIS inputs (friction + pop) --------------------------------------------------------
friction_df <- as.data.frame(friction, xy = TRUE)
pop_df <- as.data.frame(pop1x1, xy = TRUE)

friction <- ggplot() + 
  geom_raster(data = friction_df, aes(x, y, fill = friction_mada_masked)) +
  geom_point(data = ctar_metadata, aes(x = long, y = lat), color = "black", 
             shape = 4,
             stroke = 2) +
  scale_fill_distiller(palette = "Purples", na.value = "white", direction = 1,
                       name = "Friction value \n (travel speed \n minutes per meter)",
                       trans = "log", labels = function(x) round(x, 2)) +
  theme_map() +
  labs(tag = "A") +
  coord_quickmap()

# population
pop_cols <- c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e')
pop_breaks <- c(1e-6, 1, 5, 10, 100, 1e4, 1e5, 1e6)
pop_labs <- c("< 1", "1 - 5", "5 - 10", "10 - 100", "100 - 1000", "1000 - 10000", "10000 +")
names(pop_cols) <- pop_labs

pop <- ggplot() + 
  geom_raster(data = pop_df, aes(x, y, fill = cut(wp_2015_1x1, breaks = pop_breaks, 
                                                  labels = pop_labs))) + 
  scale_fill_manual(values = pop_cols, na.translate = FALSE, 
                    name = bquote('Pop per'~km^2),
                    drop = FALSE, na.value = "white") +
  theme_map() +
  theme(text = element_text(size = 14)) +
  labs(tag = "B") +
  coord_quickmap()

S1.1_inputs <- friction | pop
ggsave("figs/supplementary/S1.1_inputs.jpeg", S1.1_inputs, device = "jpeg", height = 8, width = 12)

# Raw data: groundtruthing ----------------------------------------------------------------------
gtruth <- read.csv("output/ttimes/gtruth_ttimes.csv")
baseline <- fread("output/ttimes/base/grid_df.gz")

# Moramanga self-reported data
gtruth %>%
  filter(type == "commune_wtd", !is.na(ttimes_reported)) %>%
  group_by(commune) %>%
  mutate(nobs = n()) %>%
  filter(nobs > 2) -> gtruth_mora_raw
baseline %>%
  filter(commcode %in% gtruth_mora_raw$commcode) %>%
  mutate(commune = gtruth_mora_raw$commune[match(commcode, gtruth_mora_raw$commcode)],
         type = "Friction surface", 
         ttimes_reported = ttimes/60, 
         ttimes_est = gtruth_mora_raw$ttimes_est[match(commcode, 
                                                       gtruth_mora_raw$commcode)]) -> baseline_mora
gtruth_mora_raw %>%
  select(commune, commcode, ttimes_reported, ttimes_est) %>%
  mutate(type = "Self-reported") %>%
  bind_rows(select(baseline_mora, commcode, commune, 
                   ttimes_reported, ttimes_est, type)) -> gtruth_mora_comp

mora_raw <- ggplot(data = gtruth_mora_comp, 
                   aes(x = ttimes_reported + 0.01, y = reorder(commune, ttimes_est), fill = type)) +
  geom_density_ridges(alpha = 0.75, color = "NA",  aes(point_color = type), 
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.5, height = 0),
                      point_shape = '|', point_size = 4, point_alpha = 1,
                      scale = 1) +
  scale_discrete_manual(aesthetics = "point_color", values = c(NA, "darkred"), 
                        name = "Source of estimate") +
  scale_fill_manual(values = c("red", NA), name = "Source of estimate") +
  scale_x_continuous(trans = "sqrt", breaks = c(0, 1, 2, 4, 8, 16, 24, 48, 72)) +
  theme_minimal_hgrid() +
  labs(x = "Travel times (hrs)", y = "Communes ordered \n by travel times", 
       tag = "A") +
  theme(axis.text.y = element_blank())

# IPM data
gtruth %>%
  filter(type == "point") -> gtruth_IPM
bez_pts <- get.bezier.pts(from = data.frame(long = gtruth_IPM$from_long, lat = gtruth_IPM$from_lat),
                          to = data.frame(long = gtruth_IPM$to_long, lat = gtruth_IPM$to_lat), 
                          frac = 0.5, transform = function(x) sqrt(1/x)*0.3)
gtruth_IPM$group <- 1:nrow(gtruth_IPM)
bez_pts$ttimes <- gtruth_IPM$ttimes_reported[match(bez_pts$group, gtruth_IPM$group)]

IPM_raw <- ggplot() +
  geom_sf(data = mada_districts, fill = "darkgrey", color = "darkgrey") + 
  geom_bezier2(data = bez_pts, aes(x = long, y = lat, group = group, color = ttimes,
                                   size = ifelse(index == 1, 0.15, 2))) +
  geom_point(data = filter(bez_pts, index != 2), aes(x = long, y = lat, color = ttimes), 
             alpha = 0.5) +
  scale_size_identity(guide = "none") +
  scale_color_viridis_c(breaks = c(1, 4, 8, 12, 16), name = "Travel times \n (hrs driven)") +
  theme_map()

S1.2_raw <- mora_raw + IPM_raw + plot_layout(widths = c(1.5, 1))
ggsave("figs/supplementary/S1.2_raw.jpeg", S1.2_raw, width = 10, height = 7)

# Mode of travel ------------------------------------------------------------------------------
gtruth %>%
  filter(type != "point", !is.na(mode)) %>%
  group_by(mode) %>%
  summarize(ttimes_est = mean(ttimes_est, na.rm = TRUE), 
            ttimes_reported = mean(ttimes_reported, na.rm = TRUE),
            nobs = n()) -> mode_sum
gtruth %>%
  filter(type != "point", !is.na(mode)) %>%
  mutate(mode = case_when(mode == "Pus-pus" ~ "Bicycle rickshaw", 
                          mode != "Pus-pus" ~ mode)) -> gtruth_mora

S1.3_modes <- ggplot(data = gtruth_mora, aes(x = mode, y = ttimes_reported)) + 
  geom_boxplot(outlier.color = NA) +
  scale_y_continuous(trans = "sqrt", breaks = c(0, 1, 5, 10, 16, 24, 48, 72)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5) +
  labs(x = "Mode of transport", y = "Reported travel times (hrs)") + 
  theme_minimal_hgrid() +
  coord_flip()
ggsave("figs/supplementary/S1.3_modes.jpeg", S1.3_modes, height = 6, width = 6)

# Comparing different metrics of access -------------------------------------------------------
# Plots comparing ttimes_wtd / unwtd / distance @ commune/district/grid scales
gtruth %>%
  filter(type == "commune_wtd", !is.na(ttimes_reported)) %>%
  group_by(commcode) %>%
  summarize(mean = mean(ttimes_reported), 
            nobs = n(), 
            max = max(ttimes_reported, na.rm = TRUE), min = min(ttimes_reported, na.rm = TRUE),
            upper = quantile(ttimes_reported, probs = 0.975), 
            lower = quantile(ttimes_reported, probs = 0.0275)) %>%
  left_join(select(gtruth, type, commcode, ttimes_est, ttimes_un, distance)) %>%
  unique() -> mora_means

gtruth %>%
  filter(type == "point") %>%
  mutate(nobs = 5, mean = ttimes_reported) %>%
  bind_rows(mora_means) -> all
all$ttimes_wtd <- ifelse(all$type == "commune_wtd", all$ttimes_est, NA)
all$ttimes_un <- ifelse(all$type == "point", all$ttimes_est, all$ttimes_un)

all %>%
  select(mean, ttimes_wtd, ttimes_un, distance, type, nobs) %>%
  pivot_longer(cols = c(ttimes_wtd, ttimes_un, distance)) -> lm_comp

# facet label names
facet_labs <- c("A) Distance (km)", "B) Travel times (hrs) \n unweighted", 
                "C) Travel times (hrs) \n weighted by population")
names(facet_labs) <- c("distance", "ttimes_un", "ttimes_wtd")

S1.4_lms <- ggplot(data = lm_comp, 
                   aes(x = value, y = mean, color = type, shape = type), alpha = 0.75) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = type, fill = type)) +
  scale_shape_discrete(name = "Type of estimate", 
                      labels = c("Mean reported travel times \n at commune level", 
                                 "Driving times from \n point-to-point")) + 
  scale_color_manual(values = c("#665565", "#CC575F"), 
                     name = "Type of estimate", 
                     labels = c("Mean reported travel times \n at commune level", 
                                "Driving times from \n point-to-point")) +
  scale_fill_manual(values = c("#665565", "#CC575F"), 
                     name = "Type of estimate", 
                     labels = c("Mean reported travel times \n at commune level", 
                                "Driving times from \n point-to-point")) +
  facet_wrap( ~ name, scales = "free", drop = TRUE, labeller = labeller(name = facet_labs)) +
  labs(x = "Estimated metric", y = "Observed metric") +
  theme_minimal_grid()

ggsave("figs/supplementary/S1.4_lms.jpeg", S1.4_lms, device = "jpeg", height = 10, width = 12)

points <- c(NA, summary(lm(mean ~ ttimes_un, data =filter(all, type == "point")))$r.squared,
            summary(lm(mean ~ log(distance), data =filter(all, type == "point")))$r.squared)
communes <- c(summary(lm(mean ~ ttimes_wtd, data =filter(all, type == "commune_wtd")))$r.squared, 
              summary(lm(mean ~ ttimes_un, data =filter(all, type == "commune_wtd")))$r.squared,
              summary(lm(mean ~ log(distance), data =filter(all, type == "commune_wtd")))$r.squared)  
metric <- c("Weighted travel times (hrs)", "Unweighted travel times (hrs)", "Distance (km)")
comp <- data.frame(metric, points, communes)
write.csv(comp, "output/stats/access_met_r2.csv")

# Plotting GIS outputs (ttimes @ grid/district/commune) ------------------------------------------
ttime_cols <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e', 
                '#7a0177', '#49006a')
ttime_breaks <- c(-0.1, 1, 2, 3, 4, 6, 8, 10, 15, Inf)
ttime_labs <- c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6",  "6 - 8", "8 - 10", "10 - 15", "15 +")
names(ttime_cols) <- ttime_labs

ttimes_district <- ggplot() +
  geom_sf(data = mada_districts, color = NA,
          aes(fill = cut(ttimes_wtd/60, breaks = ttime_breaks, labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, na.value = "white") +
  geom_point(data = ctar_metadata, aes(x = long, y = lat), color = "darkgrey", shape = 4,
             stroke = 2) +
  theme_map() +
  labs(tag = "A")

ttimes_commune <- ggplot() +
  geom_sf(data = mada_communes, color = NA,
               aes(fill = cut(ttimes_wtd/60, breaks = ttime_breaks, labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, na.value = "white") +
  geom_point(data = ctar_metadata, aes(x = long, y = lat), color = "darkgrey", shape = 4,
             stroke = 2) +
  theme_map() +
  labs(tag = "B")

S1.5_outputs <- (ttimes_district | ttimes_commune) + plot_layout(guides = "collect")

ggsave("figs/supplementary/S1.5_outputs.jpeg", S1.5_outputs, device = "jpeg", height = 10, width = 12)

#' Saving session info
out.session(path = "R/figures/SM1_basetimes.R", filename = "output/log_local.csv", start = start)

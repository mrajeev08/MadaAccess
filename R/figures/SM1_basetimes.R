# ------------------------------------------------------------------------------------------------ #
#' SM 1. GIS inputs and comparison to data        
# ------------------------------------------------------------------------------------------------ #

# Libraries
library(data.table)
library(tidyverse)
library(rgdal)
library(lubridate)
library(raster)
library(patchwork)
library(ggridges)
library(cowplot)
library(ggforce)
select <- dplyr::select
source("R/functions/bezier.R")

# Read in data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")

# Input rasters
friction <- raster("data/processed/rasters/friction_mada_masked.tif")
base_times <- raster("output/ttimes/base_ttimes.tif")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")

# Shapefiles to dataframes
gg_communes <- fortify(mada_communes, region = "commcode")
gg_communes %>%
  left_join(mada_communes@data, by = c("id" = "commcode")) -> gg_communes
gg_districts <- fortify(mada_districts, region = "distcode")
gg_districts %>%
  left_join(mada_districts@data, by = c("id" = "distcode")) -> gg_districts

# Plotting GIS inputs (friction + pop) --------------------------------------------------------
friction_df <- as.data.frame(friction, xy = TRUE)
pop_df <- as.data.frame(pop1x1, xy = TRUE)

friction <- ggplot() + 
  geom_raster(data = friction_df, aes(x, y, fill = friction_mada_masked)) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", 
             shape = 4,
             stroke = 2) +
  scale_fill_distiller(palette = "Purples", na.value = "white", direction = -1,
                       name = "Friction value \n (travel speed in minutes/meter)") +
  theme_map() +
  labs(tag = "A") +
  coord_quickmap()

# population
pop_cols <- c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e')
pop_breaks <- c(1e-6, 1, 5, 10, 100, 1e4, 1e5, 1e6)
pop_labs <- c("< 1", "1-5", "5 - 10", "10 - 100", "100 - 1000", "1000 - 10000", "10000 +")
names(pop_cols) <- pop_labs

pop <- ggplot() + 
  geom_raster(data = pop_df, aes(x, y, fill = cut(wp_2015_1x1, breaks = pop_breaks, 
                                                  labels = pop_labs))) + 
  scale_fill_manual(values = pop_cols, na.translate = FALSE, name = "Pop per km^2",
                    drop = FALSE, na.value = "white") +
  theme_map() +
  theme(text = element_text(size = 14)) +
  labs(tag = "B") +
  coord_quickmap()

inputs <- friction | pop
ggsave("figs/supplementary/inputs.jpeg", inputs, device = "jpeg", height = 8, width = 12)

# Plotting GIS outputs (ttimes @ grid/district/commune) ------------------------------------------
ttime_cols <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e', 
                '#7a0177', '#49006a')
ttime_breaks <- c(-0.1, 1, 2, 3, 4, 6, 8, 10, 15, Inf)
ttime_labs <- c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6",  "6 - 8", "8 - 10", "10 - 15", "15 +")
names(ttime_cols) <- ttime_labs

base_df <- as.data.frame(base_times, xy = TRUE)

ttimes_grid <- ggplot() + 
  geom_raster(data = base_df, aes(x, y, 
                                  fill = cut(base_ttimes/60, breaks = ttime_breaks, 
                                             labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, na.value = "white", guide = "none") +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", 
             shape = 4,
             stroke = 2) +
  theme_map() +
  labs(tag = "A") +
  coord_quickmap()

ttimes_commune <- ggplot() +
  geom_polygon(data = gg_communes, aes(x = long, y = lat, group = group, 
                                       fill = cut(ttimes_wtd/60, breaks = ttime_breaks,
                                                  labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, na.value = "white") +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", shape = 4,
             stroke = 2) +
  theme_map() +
  labs(tag = "B") +
  coord_quickmap()

ttimes_district <- ggplot() +
  geom_polygon(data = gg_districts, aes(x = long, y = lat, group = group, 
                                        fill = cut(ttimes_wtd/60, breaks = ttime_breaks, 
                                                   labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, na.value = "white") +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", shape = 4,
             stroke = 2) +
  theme_map() +
  labs(tag = "C") +
  coord_quickmap()

outputs <- (ttimes_grid | ttimes_commune | ttimes_district) + plot_layout(guides = "collect")

ggsave("figs/supplementary/outputs.jpeg", outputs, device = "jpeg", height = 10, width = 12)


# Raw data: groundtruthing ----------------------------------------------------------------------
gtruth <- read.csv("output/ttimes/gtruth_ttimes.csv")
baseline <- fread("output/ttimes/baseline_grid.gz")

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
  scale_x_continuous(trans = "sqrt", breaks = c(0.5, 1, 5, 10, 16, 24, 48, 72)) +
  theme_minimal_hgrid() +
  labs(x = "Travel times (hrs)", y = "Communes ordered \n by travel times") +
  theme(axis.text.y = element_blank())

baseline %>%
  filter(commcode %in% gtruth_mora_raw$commcode) %>%
  mutate(commune = gtruth_mora_raw$commune[match(commcode, gtruth_mora_raw$commcode)],
         type = "Friction surface", 
         ttimes_est = ttimes/60) -> baseline_mora

ggplot() +
  geom_density_ridges_gradient(data = baseline_mora, 
                               aes(x = ttimes_est, y = reorder(commune, ttimes_est), 
                                   fill = stat(x)), alpha = 0.75, color = "NA") +
  geom_density_ridges(data = gtruth_mora_raw, aes(x = ttimes_reported, 
                                                  y = reorder(commune, ttimes_est)),
                      fill = NA, color = NA, alpha = 1, point_color = "white", 
                      jittered_points = TRUE, point_alpha = 1) +
  scale_x_continuous(trans = "sqrt", breaks = c(0, 1, 2, 4, 8, 12, 24, 48, 72)) +
  scale_fill_viridis_c(trans = "sqrt", breaks = c(0, 1, 2, 4, 8, 12, 24, 48, 72))

library(scales)
breaks <- c(0, 1, 2, 4, 8, 12, 24, 48, 72, Inf)
cols <- viridis_pal()(length(breaks) - 1)
names(cols) <- c("0 - 1", "1 - 2", "2 - 4", "4 - 8", "12 - 24", "24 - 48", "48 - 72", "72+")
gtruth_mora_raw$ttimes_col <- cut(gtruth_mora_raw$ttimes_reported, breaks = breaks, 
                                   labels = names(cols))
gtruth_mora_raw$ttimes_col <- cols[match(gtruth_mora_raw$ttimes_col, names(cols))]

ggplot() +
  geom_density_ridges_gradient(data = baseline_mora, 
                               aes(x = ttimes_est, y = reorder(commune, ttimes_est), 
                                   fill = stat(x)), alpha = 0.75, color = "NA") +
  geom_density_ridges(data = gtruth_mora_raw, 
                      aes(x = ttimes_reported, y = reorder(commune, ttimes_est), 
                          point_color = ttimes_reported),
                      alpha = 0.75, color = NA, fill = NA,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.5, height = 0),
                      point_shape = '|', point_size = 4, point_alpha = 1) +
  scale_x_continuous(trans = "sqrt", breaks = c(0, 1, 2, 4, 8, 12, 24, 48, 72)) +
  scale_fill_viridis_c(aesthetics = c("point_color", "fill"), trans = "sqrt", 
                       breaks = c(0, 1, 2, 4, 8, 12, 24, 48, 72))

  
ggsave("figs/supplementary/mora_raw.jpeg", mora_raw, device = "jpeg", height = 10, width = 12)

# IPM data
gtruth %>%
  filter(type == "point") -> gtruth_IPM
bez_pts <- get.bezier.pts(from = data.frame(long = gtruth_IPM$from_long, lat = gtruth_IPM$from_lat),
                          to = data.frame(long = gtruth_IPM$to_long, lat = gtruth_IPM$to_lat), 
                          frac = 0.5, transform = function(x) sqrt(1/x)*0.2)
gtruth_IPM$group <- 1:nrow(gtruth_IPM)
bez_pts$ttimes <- gtruth_IPM$ttimes_reported[match(bez_pts$group, gtruth_IPM$group)]

IPM_raw <- ggplot() +
  geom_polygon(data = gg_districts, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = "darkgrey") + 
  geom_bezier2(data = bez_pts, aes(x = long, y = lat, group = group, color = ttimes)) +
  geom_point(data = filter(bez_pts, index != 2), aes(x = long, y = lat, color = ttimes)) +
  scale_color_viridis_c(breaks = c(1, 4, 8, 12, 16), name = "Travel times \n (hrs driven)") +
  coord_quickmap() +
  theme_map()
ggsave("figs/supplementary/IPM_raw.jpeg", IPM_raw, device = "jpeg", height = 6, width = 6)


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
facet_labs <- c("Distance (km)", "Travel times (hrs) \n unweighted", 
                "Travel times (hrs) \n weighted by population")
names(facet_labs) <- c("distance", "ttimes_un", "ttimes_wtd")

lm_comps <- ggplot(data = lm_comp, 
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
ggsave("figs/supplementary/lm_comps.jpeg", lm_comps, device = "jpeg", height = 10, width = 12)

points <- c(NA, summary(lm(mean ~ ttimes_un, data =filter(all, type == "point")))$r.squared,
            summary(lm(mean ~ log(distance), data =filter(all, type == "point")))$r.squared)
communes <- c(summary(lm(mean ~ ttimes_wtd, data =filter(all, type == "commune_wtd")))$r.squared, 
              summary(lm(mean ~ ttimes_un, data =filter(all, type == "commune_wtd")))$r.squared,
              summary(lm(mean ~ log(distance), data =filter(all, type == "commune_wtd")))$r.squared)  
metric <- c("Weighted travel times (hrs)", "Unweighted travel times (hrs)", "Distance (km)")
comp <- data.frame(metric, points, communes)
write.csv(comp, "output/stats/access_met_r2.csv")

# Mode of travel ------------------------------------------------------------------------------
gtruth %>%
  filter(type != "point", !is.na(mode)) %>%
  group_by(mode) %>%
  summarize(ttimes_est = mean(ttimes_est, na.rm = TRUE), 
            ttimes_reported = mean(ttimes_reported, na.rm = TRUE),
            nobs = n()) -> mode_sum
gtruth_mora <- filter(gtruth, type != "point", !is.na(mode))
ggplot(data = mode_sum, aes(x = mode, y = nobs, fill = ttimes_reported)) + geom_col()

travel_modes <- ggplot(data = gtruth_mora, aes(x = mode, y = ttimes_reported)) + 
  geom_boxplot(outlier.color = NA) +
  scale_y_continuous(trans = "sqrt", breaks = c(0, 1, 5, 10, 16, 24, 48, 72)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5) +
  labs(x = "Mode of transport", y = "Reported travel times (hrs)") + 
  theme_minimal_hgrid() +
  coord_flip()
ggsave("figs/supplementary/travel_modes.jpeg", travel_modes, height = 6, width = 6)

# Catchment maps (commune = fill & district = color ) ------------------------------------------
ctar_metadata$color <- c("#FCC56F","#004D43", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
                         "#21abcd", "#B79762", "#FFDBE5", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
                         "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
                         "#F2300F", "#FF2F80","#61615A", "#4A3B53", "#6B7900", "#00C2A0", "#FFAA92",
                         "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
ctar_metadata$fill <- ctar_metadata$color
catch_cols <- ctar_metadata$color
names(catch_cols) <- ctar_metadata$CTAR

catchments <- ggplot() +
  geom_polygon(data = gg_communes, aes(x = long, y = lat, group = group, fill = catchment), 
               color = NA, alpha = 0.5) + 
  geom_polygon(data = gg_districts, aes(x = long, y = lat, group = group, color = catchment), 
               fill = NA,  size = 1.2, alpha = 1) +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_color_manual(values = catch_cols, guide = "none") +
  coord_quickmap() +
  theme_map() +
  labs(tag = "A")

# Plotting proportion of within catch vs. outside catch 
baseline_dist <- select(mada_districts@data, pop_catch)
baseline_dist$scale <- "Districts"
baseline_comm <- select(mada_communes@data, pop_catch)
baseline_comm$scale <- "Communes"
catch_plot <- bind_rows(baseline_dist, baseline_comm)
catch_plot$scale <- factor(catch_plot$scale)
levels(catch_plot$scale) <- list("Districts" = "Districts", "Communes" = "Communes")

scale_levs <- c("Communes", "Districts")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 

catch_by_scale <- ggplot(data = catch_plot, aes(x = pop_catch, fill = scale)) +
  geom_histogram(alpha = 0.75, binwidth = 0.1, color = "white", size = 1) +
  facet_wrap(~scale, scales = "free_y") +
  scale_fill_manual(values = model_cols, guide = "none") +
  labs(x = "Proportion of population \n within assigned catchment", 
       y = "Number of \n admin units", tag = "B") +
  theme_half_open() +
  theme(strip.background = element_blank())

national <- fread("data/processed/bitedata/national.csv")
national %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = n()) %>%
  filter(!is.na(id_ctar), distcode %in% mada_districts$distcode) -> bites_dist

ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(ctar_coords, mada_communes)$commcode
ctar_metadata$distcode <- over(ctar_coords, mada_districts)$distcode
bites_dist$catch <- mada_districts$catchment[match(bites_dist$distcode, mada_districts$distcode)]
bites_dist$actual_catch <- ctar_metadata$id_ctar[match(bites_dist$catch, ctar_metadata$CTAR)]

bites_dist %>%
  group_by(id_ctar) %>%
  mutate(in_catch = ifelse(actual_catch == id_ctar, 1, 0)) %>%
  summarize(total = sum(count),
         bites_catch = sum(count[in_catch == 1], na.rm = TRUE), 
         prop_in_catch = bites_catch/total) %>%
  filter(total > 10) -> prop_dist

moramanga <- fread("data/processed/bitedata/moramanga.csv")
moramanga$catch <- mada_communes$catchment[match(moramanga$commcode, mada_communes$commcode)]
moramanga$actual_catch <- ctar_metadata$id_ctar[match(moramanga$catch, ctar_metadata$CTAR)]

moramanga %>%
  mutate(in_catch = ifelse(actual_catch == id_ctar, 1, 0)) %>%
  summarize(total = n(),
            bites_catch = sum(in_catch, na.rm = TRUE), 
            prop_in_catch = bites_catch/total) -> prop_mora

catch_by_data <- ggplot(data = prop_dist, aes(x = prop_in_catch)) +
  geom_histogram(breaks = c(seq(0, 1, by = 0.1)), color = "white", size = 3, fill = "#35274A",
                 alpha = 0.75) +
  geom_vline(xintercept = prop_mora$prop_in_catch, 
             color = "#0B775E", alpha = 0.75,
             linetype = 2) +
  labs(x = "Proportion of bites \n from within assigned catchment", y = "Number of clinics", 
       tag = "C") +
  theme_half_open()

## Output figure
catch_props <- catchments | (catch_by_scale / catch_by_data)
ggsave("figs/supplementary/catch_props.jpeg", catch_props, height = 7, width = 10)


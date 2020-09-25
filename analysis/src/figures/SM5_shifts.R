# ------------------------------------------------------------------------------
#' Plotting scenario shifts
#' Pulling in district and commune estimates of travel times as clinics are added
# ------------------------------------------------------------------------------

start <- Sys.time()
source(here::here("R", "utils.R"))

# Libraries and packages
library(data.table)
library(tidyverse)
library(sf)
library(ggridges)
library(patchwork)
library(raster)
library(cowplot)
library(glue)
library(gdistance)
library(foreach)
select <- dplyr::select
source("R/ttime_functions.R")
source("R/geom_raincloud.R")

# Pull in data
ctar_metadata <- read.csv(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
csbs <- fread(here_safe("data-raw/out/clinics/csb2.csv"))

mada_districts <- st_read(here_safe("analysis/out/shapefiles/mada_districts_simple.shp"))
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes_simple.shp"))

friction_masked <- raster(here_safe("data-raw/out/rasters/friction_mada_masked.tif"))
base_times <- raster(here_safe("analysis/out/ttimes/base/ttimes.tif"))
pop_1x1 <- raster(here_safe("data-raw/out/rasters/wp_2015_1x1.tif"))

# Commune/District preds
commune_maxcatch <- fread(here_safe("analysis/out/ttimes/addclinics/commpreds_max.csv"))
district_maxcatch <- fread(here_safe("analysis/out/ttimes/addclinics/distpreds_max.gz"))

# Added clinics & steps
clins_per_comm <- nrow(read.csv(here_safe("data-raw/out/clinics/clinic_per_comm.csv")))
clins_ranked <- fread("analysis/out/ttimes/addclinics/clinics_ranked.csv")
max_added <- sort(unique(commune_maxcatch$scenario), decreasing = TRUE)[2]
max_csb <- max(commune_maxcatch$scenario)
scenario_labs <- c(
  "Baseline\n (N = 31)", glue("1 per district\n (+ {114 - 31})"), "+ 200",
  "+ 600", glue("Max clinics added\n (+ {max_added})"),
  glue("All addtl CSB II\n (+ {max_csb})")
)
scenario_levs <- c(0, 114 - 31, 200, 600, max_added, max_csb)
names(scenario_labs) <- scenario_levs

# Filter to those scenarios
commune_maxcatch <- filter(commune_maxcatch, scenario %in% scenario_levs)
district_maxcatch <- filter(district_maxcatch, scenario %in% scenario_levs)

# Scale
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs
names(model_cols) <- scale_levs

# Which metric -----------------------------------------------------------------
tests <- fread(here_safe("analysis/out/ttimes/tests/test_preds.csv"))
tests %>%
  mutate(type = case_when(
    grepl("random", metric) ~ "random",
    !grepl("random", metric) ~ metric
  )) -> tests
mets <- c(
  "Mean travel times", "Proportion of pop", "Proportion of pop x \n mean travel times",
  "Random"
)
names(mets) <- c("mean_tt", "prop", "prop_wtd", "random")

ggplot(
  data = tests,
  aes(
    x = as.numeric(scenario), y = deaths_mean, color = type, linetype = type,
    group = metric
  ), alpha = 0.75
) +
  geom_line() +
  scale_color_manual(
    values = c("#66c2a5", "#fc8d62", "#e78ac3", "#8da0cb"),
    labels = mets, name = "Rank metric"
  ) +
  scale_linetype(labels = mets, name = "Rank metric") +
  labs(x = "# Additional ARMC", y = "Average annual deaths") +
  facet_wrap(~scale, nrow = 1) +
  theme_minimal_hgrid() +
  theme(
    panel.background = element_rect(fill = "gray92"),
    panel.grid.major = element_line(color = "white")
  ) -> S5.1_tests


write_create(S5.1_tests,
  here_safe("analysis/figs/supplementary/S5.1_tests.jpeg"),
  ggsave_it,
  width = 8, height = 6
)

# Clinics added ----------------------------------------------------------------
natl_preds <- fread(here_safe("analysis/out/preds/natl_preds.gz"))
natl_preds %>%
  filter(scale == "Commune") %>%
  mutate(scenario = as.numeric(scenario)) %>%
  arrange(scenario) %>%
  mutate(
    smoothed_deaths = predict(loess(deaths_mean ~ scenario, degree = 2, span = 0.1),
      new_data = scenario
    ),
    prop_reduction = smoothed_deaths / smoothed_deaths[scenario == 0],
    addtl_reduction = lag(prop_reduction) - prop_reduction
  ) %>%
  select(addtl_reduction, scenario, smoothed_deaths, deaths_mean, deaths_lower, deaths_upper) %>%
  right_join(clins_ranked) -> clins_ranked

step_cols <- rev(c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5"))
step_breaks <- c(0, 83, 200, 400, 600, 1000, max_added)
step_labs <- c(
  "+ 1 - 83", "+ 84 - 200", "+ 201 - 400", "+ 401 - 600", "+ 601 - 1000",
  glue("+ 1000 - {max_added}")
)
names(step_cols) <- step_labs

ggplot() +
  geom_sf(data = mada_communes, fill = "black", color = NA) +
  geom_point(
    data = clins_ranked,
    aes(
      x = long, y = lat, size = addtl_reduction,
      fill = cut(scenario, breaks = step_breaks, labels = step_labs)
    ), color = "grey50",
    alpha = 0.75, shape = 21
  ) +
  geom_point(
    data = ctar_metadata,
    aes(x = long, y = lat), shape = 3, size = 2, stroke = 1.2, color = "white"
  ) +
  geom_point(
    data = csbs[!(clinic_id %in% clins_ranked$clinic_id)],
    aes(x = long, y = lat), alpha = 0.5, shape = 3, color = "white"
  ) +
  scale_fill_manual(
    values = step_cols, na.translate = FALSE, name = "Step clinic added",
    drop = FALSE
  ) +
  scale_size_continuous(name = "Average reduction \n in burden") +
  theme_map() +
  guides(fill = guide_legend(override.aes = list(size = 3))) -> S5.2_map

ggplot(data = clins_ranked, aes(x = scenario)) +
  geom_line(aes(y = smoothed_deaths),
    color = "#0B775E",
    size = 4, alpha = 1
  ) +
  labs(x = "# Additional ARMC", y = "Average annual deaths \n (smoothed)") +
  theme_minimal_hgrid(font_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "gray92"),
    panel.grid.major = element_line(color = "white")
  ) -> S5.2_inset

layout <- c(
  patchwork::area(t = 1, l = 1, b = 5, r = 5),
  patchwork::area(t = 4, l = 5, b = 4, r = 5),
  patchwork::area(t = 1, l = 5, r = 5, b = 3)
)

S5.2_map + S5.2_inset + guide_area() +
  plot_layout(design = layout, guides = "collect") +
  theme(plot.margin = margin(25, 25, 10, 0)) -> S5.2_ARMCadded

write_create(S5.2_ARMCadded,
  here_safe("analysis/figs/supplementary/S5.2_ARMCadded.jpeg"), ggsave_it,
  width = 8, height = 8
)

# Plots of how ttimes change ----------------------------------
ttime_cols <- c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177")
ttime_breaks <- c(-0.1, 1, 2, 4, 6, 8, 10, 12, Inf)
ttime_labs <- c("< 1", "< 2", "< 4", "< 6", "< 8", "< 10", "< 15", "15 +")
names(ttime_cols) <- ttime_labs

# Getting ttime rasters
base_pts <- select(ctar_metadata, lat, long, clinic_id)
base_pts$scenario <- 0
all_pts <- rbind(base_pts, select(clins_ranked, lat, long, clinic_id, scenario))
scenario_pts <- base_pts

foreach(i = 1:length(scenario_levs), .combine = "bind_rows") %do% {
  pts <- filter(all_pts, scenario <= scenario_levs[i])
  pts$scenario <- scenario_levs[i]
  point_mat_base <- as.matrix(dplyr::select(pts, Y_COORD = long, X_COORD = lat))
  ttimes <- get_ttimes(
    friction = friction_masked, shapefile = mada_districts,
    coords = point_mat_base, trans_matrix_exists = TRUE,
    filename_trans = "data-raw/out/rasters/trans_gc_masked.rds"
  )
  names(ttimes) <- "ttimes"
  ttimes_df <- as.data.frame(ttimes, xy = TRUE)
  ttimes_df$scenario <- scenario_levs[i]
  ttimes_df$pop <- pop_1x1[]
  scenario_pts <- bind_rows(scenario_pts, pts)
  ttimes_df
} -> all_times_df

all_times_df$ttimes <- ifelse(is.infinite(all_times_df$ttimes), NA, all_times_df$ttimes)

# also pull in travel times @ commune and district level
mada_communes %>%
  select(commcode) %>%
  left_join(commune_maxcatch) -> comm_to_plot
mada_districts %>%
  select(distcode) %>%
  left_join(district_maxcatch) -> dist_to_plot

S5.3A <- ggplot() +
  geom_raster(data = all_times_df, aes(
    x = x, y = y,
    fill = cut(ttimes / 60,
      breaks = ttime_breaks,
      labels = ttime_labs
    )
  )) +
  scale_fill_manual(
    values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
    drop = FALSE, guide = "none"
  ) +
  geom_point(
    data = scenario_pts, aes(x = long, y = lat), color = "grey50", alpha = 0.75,
    shape = "."
  ) +
  facet_wrap(~scenario, nrow = 1, labeller = as_labeller(scenario_labs)) +
  theme_void() +
  coord_quickmap(clip = "off") +
  labs(tag = "A") +
  theme(strip.text.x = element_text(hjust = 0.5,
                                    margin = margin(0.1, 0.1, 0.1, 0.1, "cm")))

S5.3B <- ggplot() +
  geom_sf(
    data = comm_to_plot,
    aes(fill = cut(ttimes_wtd / 60, breaks = ttime_breaks, labels = ttime_labs)), color = NA
  ) +
  scale_fill_manual(
    values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
    drop = FALSE
  ) +
  geom_point(data = scenario_pts, aes(x = long, y = lat), shape = ".", color = "grey50", alpha = 0.75) +
  facet_wrap(~scenario, nrow = 1, labeller = as_labeller(scenario_labs)) +
  theme_void() +
  coord_sf(clip = "off") +
  theme(strip.text.x = element_blank()) +
  labs(tag = "B")

S5.3C <- ggplot() +
  geom_sf(
    data = dist_to_plot,
    aes(fill = cut(ttimes_wtd / 60, breaks = ttime_breaks, labels = ttime_labs)),
    color = NA
  ) +
  scale_fill_manual(
    values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
    drop = FALSE, guide = "none"
  ) +
  geom_point(
    data = scenario_pts, aes(x = long, y = lat), color = "grey50", alpha = 0.75,
    shape = "."
  ) +
  facet_wrap(~scenario, nrow = 1, labeller = as_labeller(scenario_labs)) +
  theme_void() +
  coord_sf(clip = "off") +
  theme(strip.text.x = element_blank()) +
  labs(tag = "C")

S5.3 <- S5.3A / S5.3B / S5.3C

# Save as tiff otherwise too slow!
write_create(S5.3,
             here_safe("analysis/figs/supplementary/S5.3_map_shifts.tiff"),
             ggsave_it,
  height = 8, width = 10, compression = "lzw",
  device = "tiff", dpi = 300, type = "cairo"
)

write_create(S5.3,
  here_safe("analysis/figs/supplementary/S5.3_map_shifts.jpeg"), ggsave_it,
  height = 8, width = 10
)

# Decreases in ttimes, bite incidence, and reporting ------------------------------------
admin_preds <- fread(here_safe("analysis/out/preds/admin_preds.gz"))
admin_preds_filtered <- filter(admin_preds, scenario %in% scenario_levs)

# Shifts in ttimes and bite incidence ------------------------------------
ggplot(
  data = admin_preds_filtered,
  aes(
    y = ttimes, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = admin_preds_filtered,
    aes(x = as.factor(scenario), y = ttimes),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    guide = "none"
  ) +
  scale_y_continuous(trans = "sqrt", breaks = c(0.5, 1, 3, 5, 10, 15)) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "# Additional ARMC", y = "Travel times (hrs)", tag = "A") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> ttimes_dist

ggplot(
  data = admin_preds_filtered,
  aes(
    y = bites_mean / pop * 1e5, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = admin_preds_filtered,
    aes(x = as.factor(scenario), y = bites_mean / pop * 1e5),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    name = "Scale"
  ) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "", y = "Reported bites \n per 100k", tag = "B") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank()
  ) -> bites_dist

S5.4 <- (ttimes_dist | bites_dist)

write_create(S5.4, here_safe("analysis/figs/supplementary/S5.4_ttimes_bites.jpeg"), ggsave_it,
  width = 8, height = 6
)

# Shifts in reporting and death incidence ------------------------------------
ggplot(
  data = admin_preds_filtered,
  aes(
    y = reporting_mean, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = admin_preds_filtered,
    aes(x = as.factor(scenario), y = reporting_mean),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    guide = "none"
  ) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "# Additional ARMC", y = "Reporting", tag = "A") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> reporting_dist

ggplot(
  data = admin_preds_filtered,
  aes(
    y = deaths_mean / pop * 1e5, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = admin_preds_filtered,
    aes(x = as.factor(scenario), y = deaths_mean / pop * 1e5),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    name = "Scale"
  ) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "", y = "Deaths per 100k", tag = "B") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank()
  ) -> deaths_dist

S5.5 <- (reporting_dist | deaths_dist)
write_create(S5.5,
  here_safe("analysis/figs/supplementary/S5.5_reporting_shifts.jpeg"),
  ggsave_it,
  width = 8, height = 6
)

# write out stats
admin_preds %>%
  group_by(scenario, scale) %>%
  summarize(
    ttimes_upper = quantile(ttimes, probs = 0.025),
    ttimes_lower = quantile(ttimes, probs = 0.975),
    ttimes_mean = mean(ttimes),
    reporting_upper = quantile(reporting_mean, probs = 0.025),
    reporting_lower = quantile(reporting_mean, probs = 0.975),
    reporting_mean = mean(reporting_mean),
    bites_upper = quantile(bites_mean / pop * 1e5, probs = 0.025),
    bites_lower = quantile(bites_mean / pop * 1e5, probs = 0.975),
    bites_mean = mean(bites_mean / pop * 1e5),
    deaths_upper = quantile(deaths_mean / pop * 1e5, probs = 0.025),
    deaths_lower = quantile(deaths_mean / pop * 1e5, probs = 0.975),
    deaths_mean = mean(deaths_mean / pop * 1e5)
  ) -> trend_preds
write.csv(trend_preds, "analysis/out/stats/trends_adminpreds.csv", row.names = FALSE)

# Shifts in clinic stats ----------------------------------------------------------------
commune_allcatch <- fread(here_safe("analysis/out/ttimes/addclinics/commpreds_all.csv"))
commune_allcatch %>%
  group_by(catchment, scenario) %>%
  summarize(pop_catch = sum(prop_pop_catch * pop_admin)) %>%
  mutate(scale = "Commune") -> catch_pops

bites_by_catch <- fread(here_safe("analysis/out/preds/catch_preds.gz"))

# Filtered scenarios
catch_pops_filtered <- filter(catch_pops, scenario %in% scenario_levs)
bites_by_catch_filtered <- filter(bites_by_catch, scenario %in% scenario_levs)

# Catchment pops distribution
ggplot(
  data = catch_pops_filtered,
  aes(
    y = pop_catch, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = catch_pops_filtered,
    aes(x = as.factor(scenario), y = pop_catch),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    guide = "none"
  ) +
  scale_y_continuous(trans = "log", breaks = c(100, 1e3, 1e4, 1e5, 1e6)) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "# Additional ARMC", y = "Catchment pop size", tag = "A") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> catchpop_dist

# Vials
ggplot(
  data = bites_by_catch_filtered,
  aes(
    y = vials_mean, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = bites_by_catch_filtered,
    aes(x = as.factor(scenario), y = vials_mean),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    guide = "none"
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(100, 500, 1e4, 5e4, 10e4)
  ) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "# Additional ARMC", y = "Annual clinic vial demand", tag = "B") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> vials_dist

# Throughput
ggplot(
  data = bites_by_catch_filtered,
  aes(
    y = tp_mean, x = as.factor(scenario), color = scale,
    fill = scale
  )
) +
  geom_flat_violin(
    position = position_nudge(x = 0.2, y = 0), adjust = 1,
    color = NA, alpha = 0.7
  ) +
  geom_boxplot(
    data = bites_by_catch_filtered,
    aes(x = as.factor(scenario), y = tp_mean),
    outlier.shape = NA, alpha = 0.5, width = 0.15
  ) +
  scale_fill_manual(
    aesthetics = c("color", "fill"), values = model_cols,
    name = "Scale"
  ) +
  scale_y_continuous(trans = "log", breaks = c(0.1, 0.5, 1, 5, 10, 50)) +
  scale_x_discrete(labels = scenario_labs) +
  labs(x = "", y = "Daily clinic throughput", tag = "C") +
  coord_flip() +
  theme_minimal_hgrid(color = "grey50") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank()
  ) -> tp_dist

S5.6_clinic_shifts <- catchpop_dist / (vials_dist | tp_dist) + plot_layout(heights = c(1, 2), guides = "collect")
write_create(S5.6_clinic_shifts,
  here_safe("analysis/figs/supplementary/S5.6_clinic_shifts.jpeg"),
  ggsave_it,
  height = 8, width = 8
)

# Catchment stats
# Means of catchment pops, throughput, vials
catch_pops %>%
  group_by(scenario, scale) %>%
  summarize(
    pops_lower = quantile(pop_catch, probs = 0.025),
    pops_upper = quantile(pop_catch, probs = 0.975),
    pops_mean = mean(pop_catch)
  ) -> trend_catch_pops

bites_by_catch %>%
  group_by(scenario, scale) %>%
  summarize(
    tp_upper = quantile(tp_mean, probs = 0.975),
    tp_lower = quantile(tp_mean, probs = 0.025),
    tp_mean = mean(tp_mean),
    vials_upper = quantile(vials_mean, probs = 0.975),
    vials_lower = quantile(vials_mean, probs = 0.025),
    vials_mean = mean(vials_mean)
  ) -> catch_means
catch_trends <- left_join(trend_catch_pops, catch_means)
write.csv(catch_trends, "analysis/out/stats/trend_catches.csv", row.names = FALSE)

# Flows of where bites reported to ----------------------
# Prop of bites in catchment
props_by_catch <- fread(here_safe("analysis/out/preds/prop_preds.gz"))
props_by_catch_filtered <- props_by_catch[scenario %in% scenario_levs]

# left join with props by catch
bites_by_catch_filtered <- left_join(bites_by_catch_filtered,
  select(scenario_pts, lat, long, clinic_id),
  by = c("catchment" = "clinic_id")
)
bites_by_catch_filtered$step_added <- clins_ranked$scenario[match(
  bites_by_catch_filtered$catchment,
  clins_ranked$clinic_id
)]
bites_by_catch_filtered %>%
  mutate(step_added = case_when(
    is.na(step_added) & catchment < 32 ~ 0,
    is.na(step_added) & scenario == max(scenario) ~ as.numeric(max_csb),
    !is.na(step_added) ~ step_added
  )) -> bites_by_catch_filtered

clinic_bites <- left_join(bites_by_catch_filtered, props_by_catch_filtered) # need to make sure this is joined with clinic id as well!
clinic_bites <- left_join(clinic_bites, select(mada_communes, commcode, long_cent, lat_cent),
  by = c("commcode" = "commcode")
)
clinic_bites$line_size <- clinic_bites$prop_bites_mean * clinic_bites$bites_mean

# Get bite incidence @ comm level
mada_communes %>%
  select(names = "commcode") %>%
  left_join(select(admin_preds_filtered, names, scenario, pop,
                   scale, bites_mean)) -> gg_commune


step_cols <- rev(c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"))
step_breaks <- c(-1, 0, 83, 200, 400, 800, 1000, max_added, max_csb)
step_labs <- c(
  "baseline", "+ 1 - 83", "+ 84 - 200", "+ 201 - 400", "+ 401 - 600", "+ 601 - 1200",
  glue("+ 1201 - {max_added}"), "All addtl CSB IIs"
)
names(step_cols) <- step_labs

S5.7_comm <- ggplot() +
  geom_sf(
    data = filter(gg_commune, scale == "Commune"),
    aes(alpha = bites_mean / pop * 1e5),
    fill = model_cols["Commune"], color = NA
  ) +
  geom_segment(
    data = filter(clinic_bites, scale == "Commune"),
    aes(
      x = long, y = lat, xend = long_cent, yend = lat_cent,
      color = cut(step_added, breaks = step_breaks, labels = step_labs),
      size = log(ceiling(prop_bites_mean * bites_mean + 1.1)) * 0.2
    ),
    alpha = 0.85
  ) +
  geom_point(
    data = filter(bites_by_catch_filtered, scale == "Commune"),
    aes(
      x = long, y = lat, size = log(bites_mean + 1.1) * 0.4,
      fill = cut(step_added, breaks = step_breaks, labels = step_labs)
    ), color = "black",
    shape = 21, alpha = 0.75
  ) +
  scale_size_identity(
    breaks = log(c(5, 50, 500, 5000) + 1.1) * 0.4,
    labels = c(5, 50, 500, "5000 +"),
    name = "Bites reported to clinic",
    guide = guide_legend(override.aes = list(alpha = 0.5, color = "grey50"))
  ) +
  scale_color_manual(
    values = step_cols, na.translate = FALSE, name = "Step clinic added",
    drop = FALSE, aesthetics = c("color", "fill")
  ) +
  scale_alpha(name = "Bites per 100k", breaks = c(5, 50, 100, 150)) +
  facet_wrap(~scenario, labeller = labeller(scenario = scenario_labs), nrow = 2) +
  theme_map() +
  guides(fill = guide_legend(override.aes = list(size = 3)))

write_create(S5.7_comm,
  here_safe("analysis/figs/supplementary/S5.7_comm_map.jpeg"),
  ggsave_it,
  height = 8, width = 8
)

S5.8_dist <- ggplot() +
  geom_sf(
    data = filter(gg_commune, scale == "District"),
    aes(alpha = bites_mean / pop * 1e5),
    fill = model_cols["District"], color = NA
  ) +
  geom_segment(
    data = filter(clinic_bites, scale == "District"),
    aes(
      x = long, y = lat, xend = long_cent, yend = lat_cent,
      color = cut(step_added, breaks = step_breaks, labels = step_labs),
      size = log(ceiling(prop_bites_mean * bites_mean + 1.1)) * 0.2
    ),
    alpha = 0.85
  ) +
  geom_point(
    data = filter(bites_by_catch_filtered, scale == "District"),
    aes(
      x = long, y = lat, size = log(bites_mean + 1.1) * 0.4,
      fill = cut(step_added, breaks = step_breaks, labels = step_labs)
    ), color = "black",
    shape = 21, alpha = 0.75
  ) +
  scale_size_identity(
    breaks = log(c(5, 50, 500, 5000) + 1.1) * 0.4,
    labels = c(5, 50, 500, "5000 +"),
    name = "Bites reported to clinic",
    guide = guide_legend(override.aes = list(alpha = 0.5, color = "grey50"))
  ) +
  scale_color_manual(
    values = step_cols, na.translate = FALSE, name = "Step clinic added",
    drop = FALSE, aesthetics = c("color", "fill")
  ) +
  scale_alpha(name = "Bites per 100k", breaks = c(5, 50, 100, 150)) +
  facet_wrap(~scenario, labeller = labeller(scenario = scenario_labs), nrow = 2) +
  theme_map() +
  guides(fill = guide_legend(override.aes = list(size = 3)))


write_create(
  S5.8_dist,
  here_safe("analysis/figs/supplementary/S5.8_dist_map.jpeg"),
  ggsave_it,
  height = 8, width = 8
)

# Session Info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)


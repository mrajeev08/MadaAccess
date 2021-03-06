# ------------------------------------------------------------------------------
#' Making maps for figure 1 of ctar and patient locations
#' network style figure for vizualizing where patients are reporting to in each
#' district
# ------------------------------------------------------------------------------

start <- Sys.time()
source(here::here("R", "utils.R"))

# Libraries
library(patchwork)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggforce)
library(sf)
library(data.table)
library(lubridate)
library(cowplot)
library(ggsn)
select <- dplyr::select

# Read in raw data (not processed)
national <- read.csv(here_safe("data-raw/out/bitedata/national.csv"))
moramanga <- read.csv(here_safe("data-raw/out/bitedata/moramanga.csv"))
ctar_metadata <- fread(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes_simple.shp"))
mada_districts <- st_read(here_safe("analysis/out/shapefiles/mada_districts_simple.shp"))
source("R/bezier.R")

# Read in data on estimated bite incidence
district_bites <- fread(here_safe("analysis/out/bites/district_bites.csv"))
mora_bites <- fread(here_safe("analysis/out/bites/mora_bites.csv"))

# matching up metadata ---------------------------------------------------------
# Centers submitting < 10 forms
no_data <- c(
  "Fianarantsoa", "Ambatomainty", "Ambovombe Androy", "Tsiroanomandidy",
  "Taolagnaro", "Mandritsara", "Antsiranana", "Marolambo", "Nosy be", "Sainte Marie", "Vangaindrano"
)
ctar_metadata$exclude <- 0
ctar_metadata$exclude[ctar_metadata$CTAR %in% no_data] <- 1

# Separate out colors from df for easier naming!
catch_cols <- c(
  "#FCC56F", "#004D43", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
  "#21abcd", "#B79762", "#FFDBE5", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
  "#578FB0", "#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF",
  "#F2300F", "#FF2F80", "#61615A", "#4A3B53", "#6B7900", "#00C2A0", "#FFAA92",
  "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2"
)
catch_fills <- catch_cols
names(catch_cols) <- ctar_metadata$CTAR
names(catch_fills) <- ctar_metadata$CTAR

catch_cols[ctar_metadata$CTAR %in% no_data] <- "grey50" # exclude ones with less than 10 forms

# Helper functions for sizing lines & circles uniformly
size_pts <- function(x) log(x) * 0.7
size_lines <- function(x) log(x) * 0.75

# Mapping raw data: National at district level ---------------------------------
# Get the # of bites reported from each district (total)
national %>%
  filter(
    year(date_reported) > 2013,
    year(date_reported) < 2018, type == "new", !is.na(distcode), !is.na(id_ctar)
  ) %>%
  group_by(distcode) %>%
  summarize(count = sum(no_patients, na.rm = TRUE)) %>%
  left_join(select(mada_districts,
    long = long_cent, lat = lat_cent, distcode = distcode,
    ctar = catchment
  )) %>%
  left_join(select(ctar_metadata, ctar = CTAR)) -> dist_pts

# Get the # of bites reported to each CTAR
national %>%
  group_by(id_ctar) %>%
  summarize(count = sum(no_patients, na.rm = TRUE)) %>%
  left_join(select(ctar_metadata, ctar = CTAR, distcode, id_ctar)) %>%
  left_join(select(mada_districts, distcode, long_cent, lat_cent)) %>%
  filter(!is.na(id_ctar)) -> ctar_pts

# Get the # of bites reported to each CTAR from each district
national %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = sum(no_patients, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(select(mada_districts, to_long = long_cent, to_lat = lat_cent, distcode)) %>%
  left_join(select(ctar_metadata, id_ctar, ctar = CTAR, ctar_distcode = distcode)) %>%
  left_join(select(st_drop_geometry(mada_districts),
    from_long = long_cent, from_lat = lat_cent,
    ctar_distcode = distcode
  )) %>%
  filter(!is.na(id_ctar)) -> ctar_todist_lines

# Fortified polygons for plotting
mada_districts %>%
  mutate(color = ctar_metadata$color[match(mada_districts$catchment, ctar_metadata$CTAR)]) %>%
  rename(ctar = catchment) -> mada_districts

# Bezier curves
bez_pts <- get_bezier_pts(
  from = data.frame(
    long = ctar_todist_lines$from_long,
    lat = ctar_todist_lines$from_lat
  ),
  to = data.frame(
    long = ctar_todist_lines$to_long,
    lat = ctar_todist_lines$to_lat
  ),
  frac = 0.8, transform = function(x) sqrt(1 / x) * 0.5
)
ctar_todist_lines$group <- 1:nrow(ctar_todist_lines)
ctar_todist_bez <- left_join(ctar_todist_lines, bez_pts, by = c("group" = "group"))
ctar_todist_bez %>%
  group_by(distcode, ctar) %>%
  arrange(index) -> ctar_todist_bez

ctar_metadata %>%
  left_join(select(mada_districts, long_cent, lat_cent, distcode)) -> ctar_all
ctar_todist_bez$dist_ctar <- ctar_metadata$distcode[match(
  ctar_todist_bez$ctar,
  ctar_metadata$CTAR
)]
ctar_todist_bez %>%
  group_by(ctar) %>%
  mutate(
    total_lines = n(),
    group = interaction(distcode, ctar)
  ) -> ctar_todist_bez

leg_pts <- get_bezleg(
  bbox = st_bbox(mada_districts), n_pts = 11, size_vec = c(100, 400, 1600, 5000),
  min_size = 5, offset_long = 0.4, offset_lat = -1.5
)
leg_pts$ptcol <- case_when(
  leg_pts$index == 3 ~ "Clinic",
  leg_pts$index == 1 ~ "District"
)
leg_cols <- c("black", "white")
names(leg_cols) <- c("Clinic", "District")
catch_fills <- c(catch_fills, leg_cols)

mada_map_A <- ggplot() +
  geom_sf(
    data = mada_districts,
    aes(fill = ctar),
    color = "white", alpha = 0.5
  ) +
  geom_bezier2(
    data = filter(ctar_todist_bez, count > 4, distcode != dist_ctar),
    aes(
      x = long, y = lat, group = fct_reorder(group, total_lines, .desc = TRUE),
      color = ctar, size = ifelse(index == 1, 0.05, size_lines(count))
    ), n = 1000,
    alpha = 0.8
  ) +
  geom_point(
    data = ctar_pts, aes(
      x = long_cent, y = lat_cent,
      size = size_pts(count), fill = ctar
    ),
    shape = 21, color = "black", stroke = 1.2
  ) +
  geom_point(
    data = dist_pts, aes(
      x = long, y = lat, fill = ctar,
      size = size_pts(count)
    ),
    shape = 21, color = "white", alpha = 1, stroke = 0.5
  ) +
  geom_point(
    data = filter(ctar_all, exclude == 1), aes(x = long_cent, y = lat_cent),
    shape = 1, stroke = 1, color = "black"
  ) +
  geom_bezier2(
    data = filter(leg_pts, type != "min"),
    aes(
      x = long, y = lat, group = group,
      size = ifelse(index == 3, 0.05, size_lines(sizes))
    ), n = 1000, color = "grey50",
    alpha = 0.8
  ) +
  geom_point(
    data = filter(leg_pts, index != 2),
    aes(
      x = long, y = lat, size = size_pts(sizes), color = ptcol,
      stroke = ptcol
    ), fill = "grey50",
    shape = 21
  ) +
  geom_text(
    data = filter(leg_pts, index != 2), aes(x = long, y = lat, label = sizes),
    hjust = 0, vjust = 1, angle = 45, nudge_y = 0.6
  ) +
  geom_text(
    data = filter(ungroup(leg_pts), long == min(long) | long == max(long)),
    aes(x = long, y = lat, label = c("District", "Clinic")), hjust = c(1, 1),
    angle = 90,
    nudge_y = -0.5
  ) +
  scale_discrete_manual(aes = "stroke", values = c(1.2, 0.5), guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  labs(tag = "A") +
  north(data = mada_districts, anchor = c(x = 52, y = -14.5), symbol = 9) +
  scalebar(
    data = mada_districts, dist = 100, dist_unit = "km",
    transform = TRUE, model = "WGS84", anchor = c(x = 52, y = -24.73),
    height = 0.01, angle = 45, hjust = 1, st.size = 3
  ) +
  theme_map(font_size = 10)


striking_img <- ggplot() +
  geom_sf(
    data = mada_districts,
    aes(fill = ctar),
    color = "white", alpha = 0.5
  ) +
  geom_bezier2(
    data = filter(ctar_todist_bez, count > 4, distcode != dist_ctar),
    aes(
      x = long, y = lat, group = fct_reorder(group, total_lines, .desc = TRUE),
      color = ctar, size = ifelse(index == 1, 0.05, size_lines(count))
    ), n = 1000,
    alpha = 0.8
  ) +
  geom_point(
    data = ctar_pts, aes(
      x = long_cent, y = lat_cent,
      size = size_pts(count), fill = ctar
    ),
    shape = 21, color = "black", stroke = 1.2
  ) +
  geom_point(
    data = dist_pts, aes(
      x = long, y = lat, fill = ctar,
      size = size_pts(count)
    ),
    shape = 21, color = "white", alpha = 1, stroke = 0.5
  ) +
  geom_point(
    data = filter(ctar_all, exclude == 1), aes(x = long_cent, y = lat_cent),
    shape = 1, stroke = 1, color = "black"
  ) +
  scale_discrete_manual(aes = "stroke", values = c(1.2, 0.5), guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  theme_map(font_size = 10)

# for plos
write_create(
  striking_img,
  here_safe("analysis/figs/striking_img.tiff"),
  ggsave_it,
  dpi = 400, height = 8.75, width = 7.5,
  compression = "lzw", type = "cairo"
)

# Mapping raw data: Moramanga at commune level ---------------------------------
moramanga %>%
  mutate(date_reported = ymd(date_reported)) %>%
  filter(
    date_reported >= "2016-10-01", date_reported < "2019-05-01",
    type == "new"
  ) %>%
  group_by(commcode) %>%
  summarize(count = sum(no_patients, na.rm = TRUE)) %>%
  left_join(select(mada_communes,
    to_long = long_cent,
    to_lat = lat_cent, commcode, ctar = catchment
  )) %>%
  left_join(select(ctar_metadata, ctar = CTAR)) -> comm_pts
comm_pts$from_long <- ctar_metadata$long[ctar_metadata$CTAR == "Moramanga"]
comm_pts$from_lat <- ctar_metadata$lat[ctar_metadata$CTAR == "Moramanga"]

# Get beziers
bez_pts <- get_bezier_pts(
  from = data.frame(long = comm_pts$from_long, lat = comm_pts$from_lat),
  to = data.frame(long = comm_pts$to_long, lat = comm_pts$to_lat),
  frac = 0.5, transform = function(x) sqrt(1 / x) * 0.1
)
comm_pts$group <- 1:nrow(comm_pts)
comm_bez <- left_join(comm_pts, bez_pts, by = c("group" = "group"))
comm_bez %>%
  group_by(commcode) %>%
  arrange(index) -> comm_bez

# Getting all all reporting communes
bounds <- st_bbox(mada_communes[mada_communes$commcode %in% comm_pts$commcode[comm_pts$count > 4], ])
comms_to_plot <- st_crop(mada_communes, bounds)
comms_to_plot <- filter(mada_communes, commcode %in% comms_to_plot$commcode)
mora_district <- filter(mada_districts, district == "Moramanga")

# Get legend
leg_pts <- get_bezleg(
  bbox = bounds,
  n_pts = 11, size_vec = c(100, 200, 400, 1600),
  min_size = 5, offset_long = -0.1, offset_lat = -0.65
)
leg_pts$ptcol <- case_when(
  leg_pts$index == 3 ~ "Clinic",
  leg_pts$index == 2 ~ "Commune",
  leg_pts$index == 1 ~ "Commune"
)
leg_cols <- c("black", "white")
names(leg_cols) <- c("Clinic", "Commune")
catch_fills <- c(catch_fills, leg_cols)

mora_map_B <- ggplot() +
  geom_sf(
    data = comms_to_plot,
    aes(fill = catchment),
    color = "white", alpha = 0.5
  ) +
  geom_sf(data = mora_district, color = "#4A3B53", fill = NA) +
  geom_point(
    data = filter(comm_pts, commcode == "MG33314010"),
    aes(x = to_long, y = to_lat, fill = ctar),
    shape = 21, size = size_pts(sum(comm_pts$count, na.rm = TRUE)), color = "black",
    stroke = 1.2
  ) +
  geom_bezier2(
    data = filter(comm_bez, commcode != "MG33314010", count > 4),
    aes(
      x = long, y = lat, group = commcode,
      size = ifelse(index == 1, 0.05, size_lines(count))
    ), n = 1000,
    color = catch_fills["Moramanga"], alpha = 0.8
  ) +
  geom_point(
    data = comm_pts, aes(
      x = to_long, y = to_lat, fill = ctar,
      size = size_pts(count)
    ),
    shape = 21, color = "white", alpha = 1
  ) +
  geom_bezier2(
    data = filter(leg_pts, type != "min"),
    aes(
      x = long, y = lat, group = group,
      size = ifelse(index == 3, 0.05, size_lines(sizes))
    ), n = 1000, color = "grey50",
    alpha = 0.8
  ) +
  geom_point(
    data = filter(leg_pts, index != 2),
    aes(
      x = long, y = lat, size = size_pts(sizes) * 0.8, color = ptcol,
      stroke = ptcol
    ), fill = "grey50",
    shape = 21
  ) +
  geom_text(
    data = filter(leg_pts, index != 2), aes(x = long, y = lat, label = sizes),
    hjust = 0, vjust = 1, angle = 45, nudge_y = 0.125
  ) +
  geom_text(
    data = filter(ungroup(leg_pts), long == min(long) | long == max(long)),
    aes(x = long, y = lat, label = c("Commune", "Clinic")), hjust = c(1, 1),
    angle = 90,
    nudge_y = -0.08
  ) +
  scale_discrete_manual(aes = "stroke", values = c(1.2, 0.5), guide = "none") +
  scale_fill_manual(values = catch_fills, na.value = "grey50", guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "B") +
  north(data = mada_districts, anchor = c(x = 49.5, y = -17.48), symbol = 9, scale = 0.02) +
  scalebar(
    data = mada_districts, dist = 20, dist_unit = "km",
    transform = TRUE, model = "WGS84", anchor = c(x = 49.5, y = -19.82),
    height = 0.0025, st.dist = 0.005, angle = 45, hjust = 1, st.size = 3
  ) +
  theme_map(font_size = 10) +
  coord_sf(clip = "off") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))



# Inset of Mora catchment in Mada ----------------------------------------------
mora_catch <- st_union(comms_to_plot)
mada_out <- st_union(mada_districts)

map_inset <- ggplot() +
  geom_sf(data = mada_out, fill = "grey50", alpha = 0.5) +
  geom_sf(data = mora_catch, fill = catch_fills["Moramanga"]) +
  theme_map(font_size = 12)

# Final figure 1 formatted for pos
layout_inset <- c(
  patchwork::area(t = 1, b = 5, l = 1, r = 6),
  patchwork::area(t = 1, b = 1, l = 1, r = 1)
)
layout_mora <- mora_map_B + map_inset + plot_layout(design = layout_inset)
figM3_top <- mada_map_A + layout_mora

# District bite incidence estimates --------------------------------------------
district_bites %>%
  select(
    group_name = distcode, pop, catchment, ttimes_wtd, avg_bites, min_bites, max_bites,
    sd_bites,
    nobs
  ) %>%
  mutate(
    dataset = "National",
    nobs = ifelse(is.na(nobs), 1, nobs)
  ) -> bites_plot

mora_bites %>%
  select(group_name = distcode, pop, catchment, ttimes_wtd, avg_bites) %>%
  group_by(group_name, catchment) %>%
  summarize(avg_bites = sum(avg_bites)) %>%
  filter(avg_bites > 10) %>%
  mutate(
    pop = mada_districts$pop[match(group_name, mada_districts$distcode)],
    ttimes_wtd = mada_districts$ttimes_wtd[match(group_name, mada_districts$distcode)],
    dataset = "Moramanga", nobs = 4
  ) %>%
  bind_rows(bites_plot) -> all_bites

size_labs <- c("1" = 1.5, "2" = 2, "3" = 2.5, "4" = 3)

bites_district <- ggplot(all_bites, aes(x = ttimes_wtd / 60, fill = catchment)) +
  geom_linerange(aes(ymin = min_bites / pop * 1e5, ymax = max_bites / pop * 1e5, color = catchment)) +
  geom_point(aes(
    y = avg_bites / pop * 1e5, shape = factor(dataset),
    size = factor(nobs)
  ), color = "grey50", alpha = 0.75) +
  scale_size_manual(
    values = size_labs, labels = names(size_labs),
    name = "Number of \nobservations:",
    guide = guide_legend(override.aes = list(shape = 21, fill = "grey"))
  ) +
  scale_shape_manual(values = c(25, 21), name = "Dataset:") +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_color_manual(values = catch_cols, guide = "none") +
  ylab("Annual bites per 100k \n at district scale") +
  xlab("Travel times (hrs)") +
  labs(tag = "C") +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "top", legend.box = "vertical")


# Commune bite incidence (Moramanga) -------------------------------------------
bites_commune <- ggplot(mora_bites, aes(x = ttimes_wtd / 60)) +
  geom_point(aes(y = avg_bites / pop * 1e5, fill = catchment),
             color = "grey50", shape = 25, size = 3,
             alpha = 0.75
  ) +
  scale_fill_manual(values = catch_cols, guide = "none") +
  ylab("Annual bites per 100k\n at commune scale") +
  xlab("Travel times (hrs)") +
  labs(tag = "D") +
  theme_minimal_hgrid(font_size = 10) +
  theme(legend.position = "top", legend.box = "vertical")

# final fig 3
figM3_bottom <- bites_district | bites_commune
figM3_bitemaps <- figM3_top / figM3_bottom + plot_layout(heights = c(3.5, 1))

# for inline figs
write_create(
  figM3_bitemaps,
  here_safe("analysis/figs/main/M3_bitemaps.jpeg"),
  ggsave_it,
  height = 10, width = 10
)
# for plos
write_create(
  figM3_bitemaps,
  here_safe("analysis/figs/main/M3_bitemaps.tiff"),
  ggsave_it,
  dpi = 300, height = 8.75, width = 7.5,
  compression = "lzw", type = "cairo"
)

# Save session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)


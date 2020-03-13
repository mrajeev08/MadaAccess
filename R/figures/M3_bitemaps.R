# ------------------------------------------------------------------------------------------------ #
#' Making maps for figure 1 of ctar and patient locations
#' Details: network style figure for vizualizing where patients are reporting to in each 
#' district
# ------------------------------------------------------------------------------------------------ #
 
# Libraries
library(patchwork)
library(tidyverse)
library(ggforce)
library(rgdal)
library(data.table)
library(lubridate)
library(cowplot)
library(rgeos)
select <- dplyr::select

# Read in raw data (not processed)
national <- read.csv("data/processed/bitedata/national.csv")
moramanga <- read.csv("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")
source("R/functions/bezier.R")

# matching up metadata -------------------------------------------------------------------------
ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(ctar_coords, mada_communes)$commcode
ctar_metadata$distcode <- over(ctar_coords, mada_districts)$distcode

# Centers submitting < 10 forms
no_data <- c("Fianarantsoa", "Ambatomainty", "Ambovombe Androy", "Tsiroanomandidy", 
             "Taolagnaro", "Mandritsara", "Antsiranana", "Marolambo", "Nosy be", "Sainte Marie", "Vangaindrano")
ctar_metadata$exclude <- 0
ctar_metadata$exclude[ctar_metadata$CTAR %in% no_data] <- 1

# Separate out colors from df for easier naming!
catch_cols <- c("#FCC56F","#004D43", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
                "#21abcd", "#B79762", "#FFDBE5", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
                "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
                "#F2300F", "#FF2F80","#61615A", "#4A3B53", "#6B7900", "#00C2A0", "#FFAA92",
                "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
catch_fills <- catch_cols
names(catch_cols) <- ctar_metadata$CTAR
names(catch_fills) <- ctar_metadata$CTAR

catch_cols[ctar_metadata$CTAR %in% no_data] <- "grey50" # exclude ones with less than 10 forms

# Helper functions for sizing lines & circles uniformly
size_pts <- function(x) log(x)
size_lines <- function(x) log(x)*0.75

# Mapping raw data: National at district level ---------------------------------------------
# Get the # of bites reported from each district (total)
national %>% 
  filter(year(date_reported) > 2013, 
         year(date_reported) < 2018, type == "new", !is.na(distcode), !is.na(id_ctar)) %>%
  group_by(distcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, long = long_cent, lat = lat_cent, distcode = distcode, 
                   ctar = catchment)) %>%
  left_join(select(ctar_metadata, ctar = CTAR)) -> dist_pts

# Get the # of bites reported to each CTAR              
national %>%
  group_by(id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(ctar_metadata, ctar = CTAR, distcode, id_ctar)) %>%
  left_join(select(mada_districts@data, distcode, long_cent, lat_cent)) %>%
  filter(!is.na(id_ctar)) -> ctar_pts

# Get the # of bites reported to each CTAR from each district 
national %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, to_long = long_cent, to_lat = lat_cent, distcode)) %>%
  left_join(select(ctar_metadata, id_ctar, ctar = CTAR, ctar_distcode = distcode)) %>%
  left_join(select(mada_districts@data, from_long = long_cent, from_lat = lat_cent, 
                    ctar_distcode = distcode)) %>%
  filter(!is.na(id_ctar)) -> ctar_todist_lines

# Fortified polygons for plotting
mada_districts$color <- ctar_metadata$color[match(mada_districts$catchment, ctar_metadata$CTAR)]
gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(select(mada_districts@data, distcode, color, ctar = catchment), 
            by = c("id" = "distcode")) -> gg_district

# Bezier curves
bez_pts <- get.bezier.pts(from = data.frame(long = ctar_todist_lines$from_long, 
                                                  lat = ctar_todist_lines$from_lat),
                              to = data.frame(long = ctar_todist_lines$to_long, 
                                                       lat = ctar_todist_lines$to_lat), 
                              frac = 0.8, transform = function(x) sqrt(1/x)*0.5) 
ctar_todist_lines$group <- 1:nrow(ctar_todist_lines)
ctar_todist_bez <- left_join(ctar_todist_lines, bez_pts, by = c("group" = "group"))
ctar_todist_bez %>%
  group_by(distcode, ctar) %>%
  arrange(index) -> ctar_todist_bez

ctar_metadata %>%
  left_join(select(mada_districts@data, long_cent, lat_cent, distcode)) -> ctar_all
ctar_todist_bez$dist_ctar <- ctar_metadata$distcode[match(ctar_todist_bez$ctar, 
                                                          ctar_metadata$CTAR)]
ctar_todist_bez %>%
  group_by(ctar) %>%
  mutate(total_lines = n(), 
         group = interaction(distcode, ctar)) -> ctar_todist_bez
  
leg_pts <- get.bezleg(bbox = bbox(mada_districts), n_pts = 11, size_vec = c(100, 400, 1600, 5000), 
                      min_size = 5, offset_long = 0.5, offset_lat = -1.5)
leg_pts$ptcol <- case_when(leg_pts$index == 3 ~ "ARMC", 
                           leg_pts$index == 1 ~ "District")
leg_cols <- c("black", "white")
names(leg_cols) <- c("ARMC", "District")
catch_fills <- c(catch_fills, leg_cols)

mada_map_A <- ggplot() +
  geom_polygon(data = gg_district, 
               aes(x = long, y = lat, group = group, fill = ctar), 
               color = "white", alpha = 0.5) +
  geom_bezier2(data = filter(ctar_todist_bez, count > 4, distcode != dist_ctar), 
               aes(x = long, y = lat, group = fct_reorder(group, total_lines, .desc = TRUE), 
                   color = ctar, size = ifelse(index == 1, 0.05, size_lines(count))), n = 1000, 
               alpha = 0.8) +
  geom_point(data = ctar_pts, aes(x = long_cent, y = lat_cent, 
                                  size = size_pts(count), fill = ctar), 
             shape = 21, color = "black", stroke = 1.2) +
  geom_point(data = dist_pts, aes(x = long, y = lat, fill = ctar,
                                  size = size_pts(count)*0.95),
             shape = 21, color = "white", alpha = 1, stroke = 0.5) +
  geom_point(data = filter(ctar_all, exclude == 1), aes(x = long_cent, y = lat_cent), 
             shape = 1, stroke = 1, color = "black") +
  geom_bezier2(data = filter(leg_pts, type != "min"),
               aes(x = long, y = lat, group = group,
                   size = ifelse(index == 3, 0.05, size_lines(sizes))), n = 1000, color = "grey50",
               alpha = 0.8) +
  geom_point(data = filter(leg_pts, index != 2),
             aes(x = long, y = lat, size = size_pts(sizes), color = ptcol,
                 stroke = ptcol), fill = "grey50",
             shape = 21) +
  geom_text(data = filter(leg_pts, index != 2), aes(x = long, y = lat, label = sizes),
            hjust = 0, vjust = 0, angle = 45, nudge_y = 0.3) +
  geom_text(data = filter(ungroup(leg_pts), long == min(long) | long == max(long)),
            aes(x = long, y = lat, label = c("District", "ARMC")), hjust = c(0, 1),
            nudge_x = c(0.5, -0.5)) +
  scale_discrete_manual(aes = "stroke", values = c(1.2, 0.5), guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  labs(tag = "A") +
  theme_minimal_hgrid() +
  theme_map() +
  coord_quickmap(clip = "off")

# Mapping raw data: Moramanga at commune level --------------------------------------------------
moramanga %>%
  mutate(date_reported = ymd(date_reported)) %>%
  filter(date_reported >= "2016-10-01", date_reported < "2019-05-01", 
         type == "new") %>%
  group_by(commcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_communes@data, to_long = long_cent, 
                   to_lat = lat_cent, commcode, ctar = catchment)) %>%
  left_join(select(ctar_metadata, ctar = CTAR)) -> comm_pts
comm_pts$from_long <- ctar_metadata$LONGITUDE[ctar_metadata$CTAR == "Moramanga"]
comm_pts$from_lat <- ctar_metadata$LATITUDE[ctar_metadata$CTAR == "Moramanga"]

gg_mora <- fortify(mada_communes[mada_communes$catchment == "Moramanga", ], region = "commcode")
gg_mora %>% 
  left_join(select(mada_communes@data, district, commcode, ctar = catchment), 
            by = c("id" = "commcode")) %>%
  left_join(select(comm_pts, commcode, count), by = c("id" = "commcode")) %>%
  filter(ctar == "Moramanga") -> gg_mora_plot

# Get beziers
bez_pts <- get.bezier.pts(from = data.frame(long = comm_pts$from_long, lat = comm_pts$from_lat),
                              to = data.frame(long = comm_pts$to_long, lat = comm_pts$to_lat), 
                              frac = 0.5, transform = function(x) sqrt(1/x)*0.1) 
comm_pts$group <- 1:nrow(comm_pts)
comm_bez <- left_join(comm_pts, bez_pts, by = c("group" = "group"))
comm_bez %>%
  group_by(commcode) %>%
  arrange(index) -> comm_bez

# Getting all all reporting communes
bounds <- bbox(mada_communes[mada_communes$commcode %in% comm_pts$commcode[comm_pts$count > 4], ])
bounds <- as(raster::extent(bounds), "SpatialPolygons")
raster::crs(bounds) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
comms_to_plot <- fortify(mada_communes[bounds, ], region = "commcode")
comms_to_plot %>%
  left_join(select(mada_communes@data, -lat_cent, -long_cent), by = c("id" = "commcode")) %>%
  mutate(ctar = ifelse(catchment == "Moramanga", "Moramanga", NA)) -> comms_to_plot
mora_district <- fortify(mada_districts[mada_districts$district == "Moramanga", ])

# Get legend
leg_pts <- get.bezleg(bbox = bbox(mada_communes[bounds, ]), 
                      n_pts = 11, size_vec = c(100, 200, 400, 1600), 
                      min_size = 5, offset_long = 0.25, offset_lat = -0.1)
leg_pts$ptcol <- case_when(leg_pts$index == 3 ~ "ARMC", 
                           leg_pts$index == 1 ~ "District")
leg_cols <- c("black", "white")
names(leg_cols) <- c("ARMC", "District")

mora_map_B <- ggplot() +
  geom_polygon(data = comms_to_plot, 
               aes(x = long, y = lat, group = group,
                   fill = ctar), 
               color = "white", alpha = 0.5) +
  geom_polygon(data = mora_district, 
               aes(x = long, y = lat, group = group), 
               color = "#4A3B53", fill = NA) +
  geom_point(data = filter(comm_pts, commcode == "MG33314010"), 
             aes(x = to_long, y = to_lat, fill = ctar), 
             shape = 21, size = size_pts(sum(comm_pts$count, na.rm = TRUE)), color = "black", 
             stroke = 1.2) +
  geom_bezier2(data = filter(comm_bez, commcode !="MG33314010", count > 4), 
               aes(x = long, y = lat, group = commcode, 
                   size = ifelse(index == 1, 0.05, size_lines(count))), n = 1000,
               color = catch_fills["Moramanga"], alpha = 0.8) +
  geom_point(data = comm_pts, aes(x = to_long, y = to_lat, fill = ctar,
                                  size = size_pts(count)*0.95),
             shape = 21, color = "white", alpha = 1) + 
  geom_bezier2(data = filter(leg_pts, type != "min"),
               aes(x = long, y = lat, group = group,
                   size = ifelse(index == 3, 0.05, size_lines(sizes))), n = 1000, color = "grey50",
               alpha = 0.8) +
  geom_point(data = filter(leg_pts, index != 2),
             aes(x = long, y = lat, size = size_pts(sizes), color = ptcol,
                 stroke = ptcol), fill = "grey50",
             shape = 21) +
  geom_text(data = filter(leg_pts, index != 2), aes(x = long, y = lat, label = sizes),
            hjust = 0, vjust = 0, angle = 45, nudge_y = 0.05) +
  geom_text(data = filter(ungroup(leg_pts), long == min(long) | long == max(long)),
            aes(x = long, y = lat, label = c("District", "ARMC")), hjust = c(0, 1),
            nudge_x = c(0.1, -0.1)) +
  scale_discrete_manual(aes = "stroke", values = c(1.2, 0.5), guide = "none") +
  scale_fill_manual(values = catch_fills, na.value = "grey50", guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "B") +
  theme_minimal_hgrid() +
  theme_map() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  coord_quickmap(clip = "off")

# Inset of Mora catchment in Mada --------------------------------------------------------------
mora_catch <- mada_communes[mada_communes$catchment == "Moramanga", ]
mora_catch <- fortify(gUnaryUnion(mora_catch))
mora_catch$ctar <- "Moramanga"
mada_out <- fortify(gUnaryUnion(mada_districts))

map_inset <-  ggplot() + 
  geom_polygon(data = mada_out, 
               aes(x = long, y = lat, group = group), color = "grey50", 
               alpha = 0.5) + 
  geom_polygon(data = mora_catch, aes(x = long, y = lat, group = group), 
               fill= catch_fills["Moramanga"]) +
  theme_minimal_hgrid() +
  theme_map() +
  coord_quickmap()

# Final figure 1 formatted for plos
layout_inset <- c(patchwork::area(t = 1, b = 5, l = 1, r = 6), 
                  patchwork::area(t = 1, b = 1, l = 1, r = 1))
layout_mora <- mora_map_B + map_inset + plot_layout(design = layout_inset)
figM3_bitemaps <- mada_map_A + layout_mora 

# for inline figs
ggsave("figs/main/M3_bitemaps.jpeg", figM3_bitemaps, height = 10, width = 10)
# for plos
ggsave("figs/main/M3_bitemaps.tiff", figM3_bitemaps, dpi = 300, height = 10, width = 10,
       compression = "lzw", type = "cairo")

# Save session info
out.session(path = "R/figures/M3_bitemaps.R", filename = "output/log_local.csv")
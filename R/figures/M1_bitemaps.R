###################################################################################################
##' Making maps for figure 1 of ctar and patient locations
##' Details: network style figure for vizualizing where patients are reporting to in each 
##' district
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries (not called in previous script)
library(patchwork)
library(tidyverse)
library(ggforce)
library(rgdal)
library(data.table)
library(lubridate)
library(cowplot)
library(rgeos)
select <- dplyr::select

## Read in raw data (not processed)
national <- read.csv("data/processed/bitedata/national.csv")
moramanga <- read.csv("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
source("R/functions/bezier.R")

##' N. Matching up metadata
##' ------------------------------------------------------------------------------------------------
ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(ctar_coords, mada_communes)$commcode
ctar_metadata$distcode <- over(ctar_coords, mada_districts)$distcode

## Centers with no data ## Change this to be so that only ones with zero forms!
no_data <- c("Fianarantsoa", "Ambatomainty", "Ambovombe Androy", "Tsiroanomandidy", 
             "Taolagnaro", "Mandritsara", 
             "Antsiranana", "Marolambo", "Nosy be", "Sainte Marie", "Vangaindrano")
ctar_metadata$exclude <- 0
ctar_metadata$exclude[ctar_metadata$CTAR %in% no_data] <- 1
ctar_metadata$color <- c("#FCC56F","#FFDBE5", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
                         "#99d8c9", "#B79762", "#004D43", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
                         "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
                         "#4A3B53", "#FF2F80","#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92",
                         "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")

ctar_metadata$fill <- ctar_metadata$color
ctar_metadata$color[ctar_metadata$CTAR %in% no_data] <- "grey50"

catch_cols <- ctar_metadata$color
names(catch_cols) <- ctar_metadata$CTAR
catch_fills <- ctar_metadata$fill
names(catch_fills) <- ctar_metadata$CTAR

##' 2. Mapping raw data: National at district level
##' ------------------------------------------------------------------------------------------------
##' Get the # of bites reported from each district (total)
national %>% 
  filter(year(date_reported) > 2013, 
         year(date_reported) < 2018, type == "new", !is.na(distcode), !is.na(id_ctar)) %>%
  group_by(distcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, long, lat, distcode = distcode, ctar = catchment)) %>%
  left_join(select(ctar_metadata, ctar = CTAR, color, fill)) -> dist_pts

##' Get the # of bites reported to each CTAR              
national %>%
  group_by(id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(ctar_metadata, ctar = CTAR, distcode, id_ctar, color, fill)) %>%
  left_join(select(mada_districts@data, distcode, long, lat)) %>%
  filter(!is.na(id_ctar)) -> ctar_pts

##' Get the # of bites reported to each CTAR from each district 
national %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, to_long = long, to_lat = lat, distcode)) %>%
  left_join(select(ctar_metadata, id_ctar, ctar = CTAR, color, fill, ctar_distcode = distcode)) %>%
  left_join(select(mada_districts@data, from_long = long, from_lat = lat, 
                    ctar_distcode = distcode)) %>%
  filter(!is.na(id_ctar)) -> ctar_todist_lines

## Fortified polygons for plotting
mada_districts$color <- ctar_metadata$color[match(mada_districts$catchment, ctar_metadata$CTAR)]
gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(select(mada_districts@data, distcode, color, ctar = catchment), 
            by = c("id" = "distcode")) -> gg_district

##' Opt 3: Bezier curves
control_pts <- get.bezier.pts(origin = data.frame(x = ctar_todist_lines$from_long, 
                                                  y = ctar_todist_lines$from_lat),
                              destination = data.frame(x = ctar_todist_lines$to_long, 
                                                       y = ctar_todist_lines$to_lat), 
                              frac = 0.8, transform = function(x) sqrt(1/x)*0.5) 

ctar_todist_lines %>%
  select(long = from_long, lat =from_lat, count, color, fill, distcode, ctar) %>%
  mutate(index = 1) -> origin
ctar_todist_lines %>%
  select(long = to_long, lat = to_lat, count, color, fill, distcode, ctar) %>%
  mutate(index = 3) -> end
control_pts %>%
  data.frame(.) %>%
  rename(long = out_x, lat = out_y) %>%
  mutate(index = 2) %>%
  bind_cols(select(ctar_todist_lines, count, color, fill, distcode, ctar)) -> mid

ctar_todist_bez <- do.call("bind_rows", list(origin, end, mid))
ctar_todist_bez %>%
  group_by(distcode, ctar) %>%
  arrange(index) -> ctar_todist_bez

ctar_metadata %>%
  left_join(select(mada_districts@data, long, lat, distcode)) -> ctar_all
ctar_todist_bez$dist_ctar <- ctar_metadata$distcode[match(ctar_todist_bez$ctar, 
                                                          ctar_metadata$CTAR)]
ctar_todist_bez %>%
  group_by(ctar) %>%
  mutate(total_lines = n(), 
         group = interaction(distcode, ctar)) -> ctar_todist_bez
  
figM1.A <- ggplot() +
  geom_polygon(data = gg_district, 
               aes(x = long, y = lat, group = group, fill = ctar), 
               color = "white", alpha = 0.5) +
  geom_bezier2(data = filter(ctar_todist_bez, count > 4, distcode != dist_ctar), 
               aes(x = long, y = lat, group = fct_reorder(group, total_lines, .desc = TRUE), 
                   color = ctar, size = ifelse(index == 1, 0.05, log(count))), n = 1000, 
               alpha = 0.8) +
  geom_point(data = ctar_pts, aes(x = long, y = lat, 
                                  size = log(count)*1.1, fill = ctar), 
             shape = 21, color = "black", stroke = 1.2) +
  geom_point(data = dist_pts, aes(x = long, y = lat, fill = ctar,
                                  size = log(count)),
             shape = 21, color = "white", alpha = 1) +
  geom_point(data = filter(ctar_all, exclude == 1), aes(x = long, y = lat), 
             shape = 1, stroke = 1, color = "black") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  labs(tag = "A") +
  coord_cartesian() +
  theme_minimal_hgrid() +
  theme_map()


##' Mapping raw data: Moramanga at commune level
##' ------------------------------------------------------------------------------------------------
moramanga %>%
  mutate(date_reported = ymd(date_reported)) %>%
  filter(date_reported >= "2016-10-01", date_reported < "2019-05-01", 
         type == "new") %>%
  group_by(commcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_communes@data, long, lat, commcode, ctar = catchment)) %>%
  left_join(select(ctar_metadata, ctar = CTAR, color, fill)) -> comm_pts
comm_pts$from_long <- ctar_metadata$LONGITUDE[ctar_metadata$CTAR == "Moramanga"]
comm_pts$from_lat <- ctar_metadata$LATITUDE[ctar_metadata$CTAR == "Moramanga"]

mada_communes$color <- ctar_metadata$color[match(mada_communes$catchment, ctar_metadata$CTAR)]
gg_mora <- fortify(mada_communes[mada_communes$catchment == "Moramanga", ], region = "commcode")
gg_mora %>% 
  left_join(select(mada_communes@data, district, commcode, color, ctar = catchment), 
            by = c("id" = "commcode")) %>%
  left_join(select(comm_pts, commcode, count), by = c("id" = "commcode")) %>%
  filter(ctar == "Moramanga") -> gg_mora_plot

##' Bezier curves
control_pts <- get.bezier.pts(origin = data.frame(x = comm_pts$long, 
                                                    y = comm_pts$lat),
                          destination = data.frame(x = comm_pts$from_long, y = comm_pts$from_lat), 
                          frac = 0.5, transform = function(x) sqrt(1/x)*0.1) 

comm_check <- cbind(comm_pts, control_pts)
comm_check %>%
  select(long = from_long, lat = from_lat, count, color, fill, commcode) %>%
  mutate(index = 1) -> pts_1
comm_check %>%
  select(long, lat, count, color, fill, commcode) %>%
  mutate(index = 3) -> pts_3
comm_check %>%
  select(long = out_x, lat = out_y, count, color, fill, commcode) %>%
  mutate(index = 2) -> pts_2
bez_pts <- bind_rows(pts_1, pts_3, pts_2) 
bez_pts %>%
  group_by(commcode) %>%
  arrange(index) -> bez_pts
 
## Getting all all reporting communes
bounds <- bbox(mada_communes[mada_communes$commcode %in% comm_pts$commcode[comm_pts$count > 4], ])
bounds <- as(raster::extent(bounds), "SpatialPolygons")
raster::crs(bounds) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
comms_to_plot <- fortify(mada_communes[bounds, ], region = "commcode")
comms_to_plot %>%
  left_join(select(mada_communes@data, -lat, -long), by = c("id" = "commcode")) %>%
  mutate(ctar = ifelse(catchment == "Moramanga", "Moramanga", NA)) -> comms_to_plot
mora_district <- fortify(mada_districts[mada_districts$district == "Moramanga", ])

## Finally getting legend points
ctar_from <- data.frame(y = seq(-18.1, -19.1, length.out = 6), x = rep(49.75, 6), 
                        size = c(100, 200, 400, 800, 1600, 10000),
                        index = 1, type = "ctar")
district_to <- data.frame(y = seq(-18.1, -19.1, length.out = 6), x = rep(50.5, 6), 
                          size = c(100, 200, 400, 800, 1600, 10000), 
                          index = 3, type = "district")
control_pts <- data.frame(y = seq(-18.1, -19.1, length.out = 6) + 0.1, x = rep(50.25, 6), 
                          size = c(100, 200, 400, 800, 1600, 10000), 
                          index = 2, type = "control")
control_pts %>%
  bind_rows(bind_rows(ctar_from, district_to)) %>%
  arrange(index) -> bez_pts_leg

figM1.B_map <- ggplot() +
  geom_polygon(data = comms_to_plot, 
               aes(x = long, y = lat, group = group,
                   fill = ctar), 
               color = "white", alpha = 0.5) +
  geom_polygon(data = mora_district, 
               aes(x = long, y = lat, group = group), 
               color = "#4A3B53", fill = NA) +
  geom_point(data = filter(comm_pts, commcode == "MG33314010"), 
             aes(x = long, y = lat, fill = ctar), 
             shape = 21, size = log(sum(comm_pts$count, na.rm = TRUE))*1.1, color = "black", 
             stroke = 1.2) +
  geom_bezier2(data = filter(bez_pts, commcode !="MG33314010", count > 4), 
               aes(x = long, y = lat, group = commcode, 
                   size = ifelse(index == 1, 0.05, log(count))), n = 1000,
               color = "#4A3B53", alpha = 0.8) +
  geom_point(data = comm_pts, aes(x = long, y = lat, fill = ctar,
                                  size = log(count)),
             shape = 21, color = "white", alpha = 1) + 
  geom_bezier2(data = bez_pts_leg, 
               aes(x = x, y = y, group = size, 
                   size = ifelse(index == 1, 0.05, log(size))), n = 1000, 
               alpha = 0.8) +
  geom_point(data = ctar_from, aes(x, y, size = log(size)*1.1), 
             color = "black", stroke = 2, fill = "grey50", shape = 21) +
  geom_point(data = district_to, aes(x, y, size = log(size + 0.1)), 
             color = "white", fill = "grey50", shape = 21) + 
  geom_text(data = district_to, aes(x = x, y = y, label = size),
            hjust = 0, nudge_x = 0.1) +
  geom_text(data = data.frame(x = c(min(ctar_from$x), min(district_to$x)), 
                              y = rep(max(ctar_from$y + 0.1), 2),
                              label = c("ARMC", "District")), 
            aes(x = x, y = y, label = label), hjust = 0, angle = 45) +
  annotate("text", y = -17.65, x = 49.7, hjust = 0, label = "Reported bites", size = 4.5) +
  scale_fill_manual(values = catch_fills, na.value = "grey50", guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "B") +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme_map() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

##' Inset of Mora catchment in Mada
##' ------------------------------------------------------------------------------------------------
mora_catch <- mada_communes[mada_communes$catchment == "Moramanga", ]
mora_catch <- fortify(gUnaryUnion(mora_catch))
mora_catch$ctar <- "Moramanga"
mada_out <- fortify(gUnaryUnion(mada_districts))

figM1.B_inset <-  ggplot() + 
  geom_polygon(data = mada_out, 
               aes(x = long, y = lat, group = group), color = "grey50", 
               alpha = 0.5) + 
  geom_polygon(data = mora_catch, aes(x = long, y = lat, group = group), 
               fill= "#4A3B53") +
  theme_minimal_hgrid() +
  theme_map()

##' Bite incidence estimates
##' ------------------------------------------------------------------------------------------------
district_bites <- fread("output/bites/district_bites.csv")
mora_bites <- fread("output/bites/mora_bites.csv")

district_bites %>%
  select(group_name = distcode, pop, catchment, ttimes_wtd, avg_bites, min_bites, max_bites, 
         sd_bites, 
         nobs) %>%
  mutate(dataset = "National", 
         nobs = ifelse(is.na(nobs), 1, nobs)) -> bites_plot
mora_bites %>%
  select(group_name = commcode, pop, catchment, ttimes_wtd, avg_bites) %>%
  mutate(dataset = "Moramanga", nobs = 2) %>%
  bind_rows(bites_plot) -> all_bites

size_labs <- c("1" = 1.75, "2" = 2.25, "3" = 3, "4" = 3.75)

figM1.C <- ggplot(all_bites, aes(x = ttimes_wtd/60, color = catchment)) +
  geom_point(aes(y = avg_bites/pop*1e5, shape = factor(dataset), size = factor(nobs)), alpha = 0.75) +
  geom_linerange(aes(ymin = min_bites/pop*1e5, ymax = max_bites/pop*1e5)) +
  scale_size_manual(values = size_labs, labels = names(size_labs),
                    name = "Number of \nobservations:") +
  scale_shape_manual(values = c(15, 16), name = "Dataset:") +
  scale_color_manual(values = catch_cols, guide = "none") + 
  ylab("Annual bites per 100k") +
  xlab("Travel times (hrs)") +
  labs(tag = "C") +
  theme_minimal_hgrid() +
  theme(legend.position = "top", legend.box = "vertical")

## Final figure 1 formatted for plos
layout_inset <- c(patchwork::area(t = 1, b = 5, l = 1, r = 6), 
                  patchwork::area(t = 1, b = 1, l = 1, r = 1))
layout_B <- figM1.B_map + figM1.B_inset + plot_layout(design = layout_inset)
figMright <- layout_B - figM1.C + plot_layout(heights = c(2, 1))
figM1 <- (figM1.A - figMright) + plot_layout(ncol = 2, widths = c(1.5, 1))

# for inline figs
ggsave("figs/main/M1.jpeg", figM1, height = 10, width = 10)
# for plos
ggsave("figs/main/M1.tiff", figM1, dpi = 300, height = 10, width = 10,
       compression = "lzw", type = "cairo")


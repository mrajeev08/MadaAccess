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

pt_sizes <- log(c(100, 200, 400, 800, 1600, 10000) + 0.1)

ctar_metadata %>%
  left_join(select(mada_districts@data, long, lat, distcode)) -> ctar_all
ctar_todist_bez$dist_ctar <- ctar_metadata$distcode[match(ctar_todist_bez$ctar, 
                                                          ctar_metadata$CTAR)]

figM1.A <- ggplot() +
  geom_polygon(data = gg_district, 
               aes(x = long, y = lat, group = group, fill = ctar), 
               color = "white", alpha = 0.5) +
  geom_bezier2(data = filter(ctar_todist_bez, count > 4, distcode != dist_ctar), 
               aes(x = long, y = lat, group = interaction(distcode, ctar), 
                   color = ctar, size = log(count + 0.1)/3), alpha = 0.75) +
  geom_point(data = ctar_pts, aes(x = long, y = lat, 
                                  size = log(count + 0.1), fill = ctar), 
             shape = 21, color = "black", stroke = 1.2) +
  geom_point(data = dist_pts, aes(x = long, y = lat, fill = ctar,
                                  size = log(count + 0.1)*0.75),
             shape = 21, color = "white", alpha = 0.75) +
  geom_point(data = filter(ctar_all, exclude == 1), aes(x = long, y = lat), 
             shape = 1, stroke = 1, color = "black") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  labs(tag = "A") +
  theme_void(base_size = 14)

##' 3. Mapping raw data: Moramanga at commune level
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
 
## Trying out mora communes plot
mora_catch <- mada_communes[mada_communes$catchment == "Moramanga", ]
mora_catch <- fortify(gUnaryUnion(mora_catch))
mora_catch$ctar <- "Moramanga"
mada_out <- fortify(gUnaryUnion(mada_districts))

figM1.B_map <- ggplot() +
  geom_polygon(data = gg_mora_plot, 
               aes(x = long, y = lat, group = group), fill = "#4A3B53", 
               color = "white", alpha = 0.5) +
  geom_bezier2(data = filter(bez_pts, commcode !="MG33314010"), 
               aes(x = long, y = lat, group = commcode, 
                   size = log(count + 0.1)/3), color = "#4A3B53", alpha = 0.75) +
  geom_point(data = filter(comm_pts, commcode == "MG33314010"), 
             aes(x = long, y = lat, fill = ctar), 
             shape = 21, size = log(sum(ctar_pts$count) + 0.1), color = "black", 
             stroke = 1.2) +
  geom_point(data = comm_pts, aes(x = long, y = lat, fill = ctar,
                                  size = log(count + 0.1)),
             shape = 21, color = "white", alpha = 0.75) + 
  scale_fill_manual(values = catch_fills, guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_size_identity("Reported bites", guide = "none") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "B") +
  theme_void()

figM1.B_inset <-  ggplot() + 
  geom_polygon(data = mada_out, 
               aes(x = long, y = lat, group = group), color = "grey50", 
               alpha = 0.5) + 
  geom_polygon(data = mora_catch, aes(x = long, y = lat, group = group), 
               fill= "#4A3B53") +
  theme_void()

line_leg <- data.frame(brks = c(100, 200, 400, 800, 1600, 10000), 
                       size = log(c(100, 200, 400, 800, 1600, 10000) + 0.1)/3)
line_leg_plot <- ggplot(line_leg, aes(x = brks, y = brks, size = size)) + 
  geom_line() + 
  scale_size_identity("", guide = "legend",
                      labels = as.character(line_leg$brks), breaks = line_leg$size) +
  guides(size = guide_legend(override.aes = list(linetype = 1, color = "black", alpha = 0.75))) +
  theme_void()
line_leg <- get_legend(line_leg_plot + theme(legend.box.margin = margin(12, 12, 12, 12)))

pt_leg <- data.frame(brks = c(100, 200, 400, 800, 1600, 10000), 
                       size = log(c(100, 200, 400, 800, 1600, 10000) + 0.1)*0.75)
pt_leg_plot <- ggplot(pt_leg, aes(x = brks, y = brks, size = size)) + 
  geom_point() + 
  scale_size_identity("Reported bites", guide = "legend",
                      labels = rep("", nrow(pt_leg)), breaks = pt_leg$size) +
  guides(size = guide_legend(override.aes = list(linetype = 0, color = "black", alpha = 0.75))) +
  theme_void()
pt_leg <- get_legend(pt_leg_plot + theme(legend.box.margin = margin(12, 12, 12, 12)))
  
figM1.B <- plot_grid(figM1.B_map, line_leg, pt_leg, nrow = 1, rel_widths = c(3, 0.2, 0.2))


## Bite incidence estimates
district_bites <- fread("output/bites/district_bites.csv")
mora_bites <- fread("output/bites/mora_bites.csv")

district_bites %>%
  select(group_name = distcode, pop, catchment, ttimes_wtd, avg_bites, sd_bites, 
         nobs) %>%
  mutate(dataset = "National", 
         nobs = ifelse(is.na(nobs), 1, nobs), 
         lower = avg_bites - 1.96*sd_bites/(sqrt(nobs)), 
         upper = avg_bites + 1.96*sd_bites/(sqrt(nobs))) -> bites_plot
mora_bites %>%
  select(group_name = commcode, pop, catchment, ttimes_wtd, avg_bites) %>%
  mutate(dataset = "Moramanga", nobs = 1) %>%
  bind_rows(bites_plot) -> all_bites

size_labs <- c("1" = 1.75, "2" = 2.25, "3" = 3, "4" = 3.75)

figM1.C <- ggplot(all_bites, aes(x = ttimes_wtd/60, color = catchment)) +
  geom_point(aes(y = avg_bites/pop*1e5, shape = factor(dataset), size = factor(nobs)), alpha = 0.75) +
  geom_linerange(aes(ymin = lower/pop*1e5, ymax = upper/pop*1e5)) +
  scale_size_manual(values = size_labs, labels = names(size_labs),
                    name = "Number of \n observations") +
  scale_shape_manual(values = c(15, 16), name = "Dataset") +
  scale_color_manual(values = catch_cols, guide = "none") + 
  ylab("Annual bites per 100k") +
  xlab("Travel times (hrs)") +
  labs(tag = "C")

## Final figure 1 formatted for plos
map_legends <- wrap_elements(pt_leg, line_leg)
layout_A <- figM1.A + map_legends + plot_layout(ncol = 2, widths = c(1, 0.2))
layout_inset <- c(area(t = 1, b = 5, l = 1, r = 5), 
                  area(t = 1, b = 1, l = 1, r = 1))
layout_B <- figM1.B_map + figM1.B_inset + plot_layout(design = layout_inset)
figM1_top <- layout_A - layout_B + plot_layout(ncol = 1, heights = c(1.5, 1))
figM1 <- figM1_top - figM1.C + plot_layout(nrow = 1, widths = c(1.5, 1))

ggsave("figs/main/M1.tiff", figM1, dpi = 300, height = 12, width = 11)


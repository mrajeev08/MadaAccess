####################################################################################################
##' Agreggating and processing data to input into bite incidence models
##' Details: 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(tidyverse)
library(rgdal)
library(lubridate)

## Read in data
bitedata <- read.csv("data/processed/master_bites.csv")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

##' N. Matching up metadata
##' ------------------------------------------------------------------------------------------------
ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(ctar_coords, mada_communes)$ADM3_PC
ctar_metadata$distcode <- over(ctar_coords, mada_districts)$distcod

## Centers with no data ## Change this to be so that only ones with zero forms!
no_data <- c("Fianarantsoa", "Ambatomainty", "Ambovombe Androy", "Tsiroanomandidy", 
             "Taolagnaro", "Mandritsara", 
             "Antsiranana", "Marolambo", "Nosy be", "Sainte Marie", "Vangaindrano")
ctar_metadata$exclude <- 0
ctar_metadata$exclude[ctar_metadata$CTAR %in% no_data] <- 1
ctar_metadata$color <- c("#FCC56F","#FFDBE5", "#7A4900", "#CBCDD2", "#0000A6", "#EFF2F1",
                         "#99d8c9", "#B79762", "#004D43", "#8FB0FF", "#FFFFFF", "#FD9C54", "#8362B5",
                         "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
                         "#4A3B53", "#FF2F80","#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92",
                         "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
ctar_metadata$fill <- ctar_metadata$color
ctar_metadata$color[ctar_metadata$CTAR %in% no_data] <- "#D3D3D3"


##' 2. Mapping raw data
##' ------------------------------------------------------------------------------------------------
bitedata %>% 
  filter(source != "Moramanga", year(date_reported) > 2013, 
         year(date_reported) < 2018, type == "new", !is.na(distcode), !is.na(id_ctar)) -> natl
natl %>%
  group_by(distcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, long, lat, distcode = distcod, ctar = ctch_wtd_n)) %>%
  left_join(select(ctar_metadata, ctar = CTAR, color, fill)) -> dist_pts
                   
natl %>%
  group_by(id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(ctar_metadata, distcode, id_ctar, color, fill)) %>%
  left_join(select(mada_districts@data, distcode = distcod, long, lat)) -> ctar_pts

natl %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, to_long = long, to_lat = lat, distcode = distcod)) %>%
  left_join(select(ctar_metadata, id_ctar, color, fill, ctar_distcode = distcode)) %>%
  left_join(select(mada_districts@data, from_long = long, from_lat = lat, 
                    ctar_distcode = distcod))-> ctar_todist_lines

ctar_todist_lines %>%
  ungroup() %>%
  mutate(end_lat_plus = to_lat + log(count + 1)*0.05, 
         end_long_plus = to_long + log(count + 1)*0.05,
         end_lat_minus = to_lat - log(count + 1)*0.05, 
         end_long_minus = to_long - log(count + 1)*0.05,
         group_id = paste0(distcode, id_ctar)) %>%
  select(group_id, contains("lat"), contains("long"), -to_lat, -to_long, color) -> ctar_todist_pols

ctar_todist_pols <- bind_rows(select(natl_pols, lat = end_lat_plus, long = end_long_plus,
                                     group = group_id, color),
                              select(natl_pols, lat = end_lat_minus, long = end_long_minus, 
                                     group = group_id, color),
                              select(natl_pols, lat = from_lat, long = from_long, 
                                     group = group_id, color))

mada_districts$color <- ctar_metadata$color[match(mada_districts$ctch_wtd_n, ctar_metadata$CTAR)]
gg_district <- fortify(mada_districts, region = "distcod")
gg_district %>% 
  left_join(select(mada_districts@data, distcod, color, ctar = ctch_wtd_n), 
            by = c("id" = "distcod")) -> gg_district

cols <- c("#FCC56F","#FFDBE5", "#7A4900", "#CBCDD2", "#0000A6", "#EFF2F1",
          "#99d8c9", "#B79762", "#004D43", "#8FB0FF", "#FFFFFF", "#FD9C54", "#8362B5",
          "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
          "#4A3B53", "#FF2F80","#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92",
          "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2", "#D3D3D3")
names(cols) <- as.character(cols)

sizes <- log(c(100, 200, 400, 800, 1600) + 0.1)

p <- ggplot() +
  geom_polygon(data = gg_district, 
               aes(x = long, y = lat, group = group, fill = color), color = "grey50", alpha = 0.5) +
  geom_polygon(data = natl_pols, aes(x = long, y = lat, group = group,
                                     fill = color), color = NA, alpha = 0.5, 
               show.legend = FALSE) +
  geom_point(data = natl_emptypts, aes(x = long, y = lat, fill = fill,
                                     size = log(count + 0.1)), color = alpha("black", 0.75),
             shape = 21, alpha = 0.75) +
  geom_point(data = natl_points, aes(x = long, y = lat, 
                                     size = log(count + 0.1), color = color), 
             shape = 1) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  scale_size_identity("Reported bites", labels = as.character(c(100, 200, 400, 800, 1600)),
                      breaks = sizes, guide = "legend") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "A") +
  theme_void()

p <- ggplot() +
  geom_polygon(data = gg_district, 
               aes(x = long, y = lat, group = group, fill = color), color = "grey50", alpha = 0.5) +
  geom_segment(data = natl_lines, aes(x = from_long, y = from_lat, xend = to_long, 
                                      yend = to_lat, 
                                     color = color, size = log(count + 0.1)*0.5), alpha = 0.5, 
               show.legend = FALSE) +
  geom_point(data = natl_emptypts, aes(x = long, y = lat, fill = fill,
                                       size = log(count + 0.1)), color = alpha("black", 0.75),
             shape = 21, alpha = 0.75) +
  geom_point(data = natl_points, aes(x = long, y = lat, 
                                     size = log(count + 0.1), color = color), 
             shape = 1) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  scale_size_identity("Reported bites", labels = as.character(c(100, 200, 400, 800, 1600)),
                      breaks = sizes, guide = "legend") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "A") +
  theme_void()

ggsave("check.jpg", p, device = "jpeg", height = 10, width = 7)

bitedata %>%
  mutate(date_reported = ymd(date_reported)) %>%
  filter(source == "Moramanga", date_reported >= "2016-10-01", date_reported < "2019-05-01", 
         type == "new") -> mora

## Stats on transfers
## Date limits and reporting
## Contacts
## Bite incidence estimates


##' 1. Filter out transfers (these are people that came from other clinics)
##' 2. Filter Cat 1 based on clinic throughput (compare to doses delivered to clinic in metadata)
##' 3. Calc reporting
##' 4. Summarize by year and admin unit
##' 5. Filter out excluded catchments and correct for reporting

####################################################################################################
##' SM 1. GIS Inputs and Output
##' Details: Showing input raster layers and output travel times at admin level (district and commune)
##' Author: Malavika Rajeev 
####################################################################################################

rm(list = ls())

##' Libraries
library(tidyverse)
library(rgdal)
library(lubridate)
library(raster)
library(rasterVis)
library(patchwork)
library(ggridges)
select <- dplyr::select

##' Read in data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

##' Input rasters
base_times <- raster("output/ttimes/baseline_ttimes.tif")
pop <- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")

##'Shapefiles
gg_communes <- fortify(mada_communes, region = "commcode")
gg_communes %>%
  left_join(select(mada_communes@data, -long, -lat), by = c("id" = "commcode")) -> gg_communes
gg_districts <- fortify(mada_districts, region = "distcode")
gg_districts %>%
  left_join(select(mada_districts@data, -long, -lat), by = c("id" = "distcode")) -> gg_districts

##' Plotting GIS inputs/outputs 
##' ------------------------------------------------------------------------------------------------

##' Colors
ttime_cols <- c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
ttime_breaks <- c(-0.1, 1, 2, 4, 6, 8, 10, 12, Inf)
ttime_labs <- c("< 1", "< 2", "< 4", "< 6", "< 8", "< 10", "< 15", "15 +")
names(ttime_cols) <- ttime_labs

figS1.A <- gplot(base_times) + 
  geom_raster(aes(fill = cut(value/60, breaks = ttime_breaks, labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", 
             shape = 4,
             stroke = 2) +
  theme_void() +
  theme(text = element_text(size = 14)) +
  labs(tag = "A")

figS1.B <- gplot(pop) + 
  geom_raster(aes(fill = log(value))) + 
  scale_fill_distiller(type = "seq", palette = "GnBu", direction = 1, na.value = "white",
                   name = expression("Log(population) \n per 1km"^2)) +
  theme_void() +
  theme(text = element_text(size = 14)) +
  labs(tag = "B")

figS1.C <- ggplot() +
  geom_polygon(data = gg_districts, aes(x = long, y = lat, group = group, 
                                        fill = cut(ttimes_wtd/60, breaks = ttime_breaks, 
                                                   labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", shape = 4,
             stroke = 2) +
  theme_void() +
  theme(text = element_text(size = 14)) +
  labs(tag = "C")

figS1.D <- ggplot() +
  geom_polygon(data = gg_communes, aes(x = long, y = lat, group = group, 
                                       fill = cut(ttimes_wtd/60, breaks = ttime_breaks,
                                                  labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", shape = 4,
             stroke = 2) +
  theme_void() +
  theme(text = element_text(size = 14)) +
  labs(tag = "D")

figS1.1 <- (figS1.A | figS1.B) / (figS1.C | figS1.D)

ggsave("figs/supplementary/S1.1.jpeg", figS1.1, device = "jpeg", height = 12, width = 10)

##' Plotting proportion of within catch vs. outside catch 
##' ------------------------------------------------------------------------------------------------
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

figS1.2A <- ggplot(data = catch_plot, aes(x = pop_catch, fill = scale)) +
  geom_histogram(alpha = 0.75, binwidth = 0.1, color = "white", size = 1) +
  facet_wrap(~scale, scales = "free_y") +
  scale_fill_manual(values = model_cols, guide = "none") +
  labs(x = "Proportion of population \n within assigned catchment", 
       y = "# of Admin units", tag = "A") +
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

figS1.2B <- ggplot(data = prop_dist, aes(x = prop_in_catch)) +
  geom_histogram(breaks = c(seq(0, 1, by = 0.1)), color = "white", size = 3, fill = "grey50") +
  geom_vline(xintercept = prop_mora$prop_in_catch, 
             color = ctar_metadata$color[ctar_metadata$CTAR == "Moramanga"], 
             linetype = 2) +
  labs(x = "Proportion of bites \n from within assigned catchment", y = "Number of clinics", 
       tag = "B") +
  theme_half_open()

## Output figure
figS1.2 <- figS1.2A / figS1.2B
ggsave("figs/supplementary/S1.2.jpeg", figS1.2, height = 7, width = 5)


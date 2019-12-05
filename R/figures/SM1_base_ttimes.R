####################################################################################################
##' SM 1. Baseline travel times  
##' Details: Showing input raster layers and output travel times at admin level  
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
library(scico)
select <- dplyr::select

##' Read in data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

##' Plot baseline travel times
base_times <- raster("output/ttimes/baseline_ttimes.tif")
pop <- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")

##'Shapefiles
gg_communes <- fortify(mada_communes, region = "commcode")
gg_communes %>%
  left_join(select(mada_communes@data, -long, -lat), by = c("id" = "commcode")) -> gg_communes
gg_districts <- fortify(mada_districts, region = "distcode")
gg_districts %>%
  left_join(select(mada_districts@data, -long, -lat), by = c("id" = "distcode")) -> gg_districts

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

ggsave("figs/S1.1.jpeg", figS1.1, device = "jpeg", height = 12, width = 10)


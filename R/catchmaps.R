rm(list = ls())

## Libraries
library(tidyverse)
library(rgdal)
library(lubridate)
library(rgeos)
select <- dplyr::select

## Read in data
bitedata <- read.csv("data/processed/master_bites.csv")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

mada_districts@data %>%
  select(distcode, starts_with("ctch")) %>%
  gather("catch_type", "catch_district", -distcode) -> catch_district
mada_communes@data %>%
  select(ADM3_PCODE, distcode, starts_with("ctch")) %>%
  gather("catch_type", "catch_commune", -ADM3_PCODE, -distcode) %>%
  left_join(catch_district) -> catches

gg_communes <- fortify(mada_communes, region = "ADM3_PCODE")
gg_communes %>% 
  left_join(catches, 
            by = c("id" = "ADM3_PCODE")) -> gg_communes
gg_districts <- fortify(mada_districts, region = "distcode")
gg_districts %>%
  left_join(catch_district, by = c("id" = "distcode")) -> gg_districts

## Named vector of colors
catch_cols <- alpha(as.character(ctar_metadata$color), 0.5)
names(catch_cols) <- ctar_metadata$CTAR
  
check <- ggplot() +
  geom_polygon(data = gg_communes, aes(x = long, y = lat, group = group,
                                       fill = catch_commune), alpha = 0.75) +
  geom_polygon(data = gg_districts, aes(x = long, y = lat, group = group, color = catch_district),
               fill = NA) +
  scale_fill_manual(values = catch_cols) +
  scale_color_manual(values = catch_cols) +
  facet_wrap(~catch_type)

ggsave("check.pdf", check, device = "pdf", width = 10, height = 8)

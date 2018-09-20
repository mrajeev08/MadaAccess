## Clean-up
setwd("~/Dropbox/MadaRabies/MadaPEP")
rm(list = ls())

## Packages
# for gis 
library(knitr)
library(raster)
library(maptools)
library(maps)
library(GISTools)
library(rgdal)
library(sp)
library(rgdal)
library(gdistance)
# everything else
library(lubridate)
library(reshape2)
library(tidyverse)
library(rjags)
library(coda)
library(lattice)
# pkgs <- c("lattice", "coda")
# require(pkgs, character.only = TRUE)

## gis data {NEED TO FIGURE OUT PROJECTIONS!}
p4s <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mada.district <- readOGR("data/MadaGIS/MadaPops.shp", p4s = p4s)
ctar.gps <- read.csv("data/ctar_gps.csv")
ctar.gps <- SpatialPoints(cbind(ctar.gps$LONGITUDE, ctar.gps$LATITUDE),
                          proj4string = crs(mada.district))
ctar.atts <- readOGR("data/MadaGIS/31CTAR.shp")
# ## getting travel time layer
mada.access <- raster("output/study.area.accessibility.tif")
mada.access <- crop(mada.access, mada.district)
plot(mada.access/60, breaks = c(0, 1, 2, 3, 4, 5, 6, 8, 16, max(values(mada.access)/60, na.rm = TRUE)),
     col = c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177", "#49006a"))
#col = colorRampPalette(c("navyblue", "mediumturquoise", "orange", "red"))(11), axes = FALSE, box = FALSE)
points(ctar.gps, pch = 18, col = "grey", cex = 1)

## getting which districts have a ctar
mada.district$ctar.in <- ifelse(is.na(match(mada.district$district, ctar.atts$District)), 0, 1)
mada.district$ctar.in[mada.district$district %in% c('Manakara Atsimo', 'Toliary-I', 'Taolagnaro',
                                                    'Nosy-Be', "Ambovombe-Androy")] <- 1

## should equal 31
sum(mada.district$ctar.in)

## mid.pops
mada.district$midpops <- (mada.district$pop2015adj + mada.district$pop2020adj)/2

## ctar data: getting exposure matrix (district by year)
ctar.data <- read.csv("data/SaisieRage_DATA_2018-06-28_0956.csv")
ctar.data %>% 
  filter(type != "contact") %>% # exclude contacts
  mutate_at(vars(starts_with("date")), funs(mdy(.))) %>% # format dates
  group_by(year = year(date_de_consultation), district) %>% # group by year and district
  summarize(n = n()) -> exposures # get count of exposures

## merge district shapefile with the summarized matrix!
exposure.mat <- dcast(exposures, district ~ year)
colnames(exposure.mat) <- c("district", "year2013", "year2014", "year2015", "year2016", "year2017", "year2018")
mada.district@data <- merge(mada.district@data, exposure.mat, by.x = "mdg_dis_co", by.y = "district", all.x = TRUE)
vars <- c("mdg_dis_co", "district", "midpops", "study_area", "ctar.in",
          "year2014", "year2015", "year2016", "year2017")

## Getting all dist data
mada.district@data %>%
  select(!!vars) %>%
  mutate_at(vars(starts_with("year")), funs(replace(., is.na(.), 0))) %>%
  mutate(avg.bites100k = (select(., starts_with("year")) %>% rowSums())/4/(midpops/1e5),
         dogs_min = round(midpops/25), dogs_max = round(midpops/5)) -> mada.district@data

## non-reporter and data not available yet CTAR
no_data <- c("Manja", 'Ambatondrazaka', "Vangaindrano", "Tsiroanomandidy",
             "Antsiranana I", "Taolagnaro", "Ambovombe-Androy", "Nosy-Be", "Ambatomainty",
             "Marolambo", "Sainte-Marie", "Antananarivo Renivohitra", "Antananarivo Avaradrano", 
             "Ambohidratrimo", "Ankazobe", "Manjakandriana", "Anjozorobe", "Andramasina", 
             "Antananarivo Atsimondrano", "Fianarantsoa I", "Ambalavao", "Ambohimahasoa", 
             "Ikalamavony", "Lalangina", "Vohibato", "Isandra")
mada.district$avg.bites100k[mada.district$district %in% no_data] <- NA

## set to NA all non-reporters/adjust accordingly!!
## function for empirical p
empirical.reporting <- function (bites, p_rabid = 0.2, expected) {
  p_rabid_max = expected/bites
  p_rabid_max[p_rabid_max >= p_rabid] <- p_rabid
  p_report <- bites*p_rabid_max/expected
  return(p_report)
}

## adding in expected exposures annually
# rabies incidence per 100kdogs/# of human exps per rabid dog/human:dog ratio = inc per 100k persons
exps.per100k.high <- 1000*0.39/5 
exps.per100k.low <- 600*0.39/25

mada.district$rho_high <- empirical.reporting(mada.district$avg.bites100k, p_rabid = 0.75, exps.per100k.low)
mada.district$rho_low <- empirical.reporting(mada.district$avg.bites100k, p_rabid = 0.25, exps.per100k.high)
mada.district$rho_med <- (mada.district$rho_high + mada.district$rho_low)/2

plot(mada.district, col = ifelse(mada.district$data_available == 1, "orange", "grey"))

## commune checking
comm_names <- read.csv("commune_check_CH.csv")


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
select <- dplyr::select

## Read in data
national <- read.csv("data/processed/bitedata/national.csv")
moramanga <- read.csv("data/processed/bitedata/moramanga.csv")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
source("R/functions/data_functions.R")

##' CTAR IN DISTRICT
pts <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
mada_districts$ctar_in_district<- over(mada_districts, pts)
mada_districts$ctar_in_district <- ifelse(is.na(mada_districts$ctar_in_district), 0, 1)
mada_communes$ctar_in_district <- mada_districts$ctar_in_district[match(mada_communes$distcode, 
                                                                        mada_districts$distcode)]

##' 1. National bite data: getting incidence estimates
##' ------------------------------------------------------------------------------------------------

##' Getting daily throughput for each clinic
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n()) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
           fill = list(no_patients = 0)) -> throughput

##' rle.days = Helper function for getting which days to include (moved to functions from data_functions.R)
##' and also identify potential CAT 1 by the throughput mean/sd
throughput %>%
  group_by(id_ctar) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(include = rle.days(no_patients, threshold = 15), 
         mean_throughput = mean(no_patients[include == 1]),
         sd_throughput = sd(no_patients[include == 1]),
         estimated_cat1 = ifelse(no_patients >= mean_throughput + 3*sd_throughput, 
                                     1, 0),
         year = year(date_reported)) -> throughput

##' yearly reporting
throughput %>%
  group_by(year, id_ctar) %>%
  summarize(reporting = sum(include/365)) -> reporting

##' Left join with throughput to get exclusion criteria
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, distcode %in% mada_districts$distcode, 
         id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)],
         type == "new") %>% 
  mutate(date_reported = ymd(date_reported)) %>%
  left_join(select(throughput, date_reported, id_ctar, include, estimated_cat1, year)) -> bites

##' Getting district level exclusion criteria
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, distcode %in% mada_districts$distcode, 
         id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)]) %>%
  group_by(id_ctar) %>%
  summarize(total_forms = n()) %>%
  complete(id_ctar = ctar_metadata$id_ctar, fill = list(total_forms = 0)) %>%
  right_join(ctar_metadata) %>%
  mutate(total_forms = ifelse(is.na(total_forms), 0, total_forms),
         exclude = ifelse(total_forms > 10, 0, 1)) -> ctar_metadata

mada_districts$exclude_by_ttimes <- ctar_metadata$exclude[match(mada_districts$ctch_ttwtd, ctar_metadata$CTAR)]
mada_districts$exclude_by_distance <- ctar_metadata$exclude[match(mada_districts$ctch_dsct, ctar_metadata$CTAR)]

##' Getting bite incidence estimates
bites %>%
  ## filter known contacts and estimated ones based on throughput
  filter(estimated_cat1 == 0, include == 1) %>% 
  group_by(year, distcode, id_ctar) %>%
  summarize(bites = n()) %>%
  left_join(reporting) %>%
  ## filter any years with reporting < 25% 
  filter(reporting > 0.25) %>%
  ## correct for reporting by year and ctar reported to 
  mutate(bites_corrected = bites/reporting) %>%
  group_by(distcode, year) %>%
  summarize(total = sum(bites_corrected)) %>%
  group_by(distcode) %>%
  summarize(avg_bites = mean(total, na.rm = TRUE)) %>%
  complete(distcode = mada_districts$distcode, fill = list(avg_bites = 0)) -> bite_ests

 mada_districts@data %>%
  select(names_covar = distcode, district = ADM2_EN, pop, long, lat, 
         catchment = ctch_ttwtd, covar = ttms_wtd, exclude_by_ttimes,
         ctar_in_district) %>%
  mutate(covar_name = "ttimes") %>%
  filter(exclude_by_ttimes == 0) %>%
  left_join(bite_ests, by = c("names_covar" = "distcode")) -> bites_by_ttimes

mada_districts@data %>%
  select(names_covar = distcode, district = ADM2_EN, pop, long, lat, 
         covar = dist_cent, catchment = ctch_dsct, exclude_by_distance,
         ctar_in_district) %>%
  mutate(covar_name = "dist_cent") %>%
  filter(exclude_by_distance == 0) %>%
  left_join(bite_ests, by = c("names_covar" = "distcode")) -> bites_by_distance

##' 2. Moramanga data 
##' ------------------------------------------------------------------------------------------------
moramanga %>%
  mutate(month_date = floor_date(ymd(moramanga$date_reported), unit = "month")) %>%
  filter(known_cat1 == 0, type == "new", month_date >= "2016-10-01", 
         month_date <= "2019-06-01", !is.na(commcode)) %>%
  group_by(commcode, month_date) %>%
  summarize(bites = n()) %>%
  ungroup() %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month"), 
           commcode, fill = list(bites = 0)) %>%
  group_by(commcode) %>%
  summarize(avg_bites = mean(bites)*12) %>% # average monthly bites x 12 to get annual avg_bites
  complete(commcode = mada_communes$ADM3_PCODE, fill = list(avg_bites = 0)) -> mora_bites 

mada_communes@data %>%
  mutate(ctar_in_district = ifelse(distcode == "MG33314", 1, 0),
         covar_name = "ttimes") %>%
  select(names_covar = ADM3_PCODE, commune = ADM3_EN, pop, 
         long, lat, catchment = ctch_ttwtd, covar = ttms_wtd, ctar_in_district, covar_name) %>%
  filter(catchment == "Moramanga") %>%
  left_join(mora_bites, by = c("names_covar" = "commcode")) -> morabites_by_ttimes

mada_communes@data %>%
  mutate(ctar_in_district = ifelse(distcode == "MG33314", 1, 0),
         covar_name = "dist_cent") %>%
  select(names_covar = ADM3_PCODE, commune = ADM3_EN, pop, 
         long, lat, covar = dist_cent, covar_name, catchment = ctch_dsct,
         ctar_in_district) %>%
  filter(catchment == "Moramanga") %>%
  left_join(mora_bites, by = c("names_covar" = "commcode")) -> morabites_by_distance


## Covariate data frames (for summed models need commune level covariates)
mada_communes$exclude_by_ttimes <- mada_districts$exclude_by_ttimes[match(mada_communes$distcode,
                                                               mada_districts$distcode)]
mada_communes$exclude_by_distance <- mada_districts$exclude_by_distance[match(mada_communes$distcode,
                                                                          mada_districts$distcode)]
mada_communes$ctch_ttwtd_dist <- mada_districts$ctch_ttwtd[match(mada_communes$distcode,
                                                                 mada_districts$distcode)]
mada_communes$ctch_dsct_dist <- mada_districts$ctch_dsct[match(mada_communes$distcode,
                                                                 mada_districts$distcode)]
mada_communes@data %>%
  select(names_covar = distcode, district = ADM2_EN, pop, long, lat, 
         catchment = ctch_ttwtd_dist, covar = ttms_wtd, exclude_by_ttimes, ctar_in_district) %>%
  mutate(covar_name = "ttimes") %>%
  filter(exclude_by_ttimes == 0) -> commcovars_by_ttimes

mada_communes@data %>%
  select(names_covar = distcode, district = ADM2_EN, pop, long, lat, 
         covar = dist_cent, catchment = ctch_dsct_dist, exclude_by_distance, ctar_in_district) %>%
  mutate(covar_name = "dist_cent") %>%
  filter(exclude_by_distance == 0) -> commcovars_by_distance


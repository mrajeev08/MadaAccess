####################################################################################################
##' Getting district/commune bite incidence
##' Details: Agreggating data by admin unit and by year to get average annual incidence for National
##' and Moramanga data 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(tidyverse)
library(data.table)
library(rgdal)
library(lubridate)
select <- dplyr::select
source("R/functions/utils.R")
source("R/functions/data_functions.R")

## Read in data
national <- fread("data/processed/bitedata/national.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

##' Get stats on missingness
##' National < 0.5% (0.28%)
nrow(national[is.na(year(date_reported))]) ## 1
nrow(national[is.na(distcode)]) ## 95
nrow(national[is.na(id_ctar)]) ## 176

##' Moramanga < 2%
nrow(moramanga[is.na(year(date_reported))]) ## 0
nrow(moramanga[is.na(distcode)]) ## 23 and these are passage!
nrow(moramanga[is.na(id_ctar)]) ## 0

##' But commune matching went poorly for national data
nrow(national[is.na(commcode)]) ## 54% unmatched (40% of peripheral unmatched and 60% of IPM unmatched)
nrow(moramanga[is.na(commcode)]) ## 23 and these are passage! (< 2%)

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
  mutate(include_day = rle.days(no_patients, threshold = 15), 
         mean_throughput = mean(no_patients[include_day == 1]),
         sd_throughput = sd(no_patients[include_day == 1]),
         estimated_cat1 = ifelse(no_patients >= mean_throughput + 3*sd_throughput, 
                                     1, 0),
         year = year(date_reported)) -> throughput

##' yearly reporting
##' sum the total # of days included over # of days in year (365)
throughput %>%
  group_by(year, id_ctar) %>%
  summarize(reporting = sum(include_day)/365) -> reporting

##' Left join with throughput to get exclusion criteria
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018,
         distcode %in% mada_districts$distcode, 
         id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)],
         type == "new") %>% 
  mutate(date_reported = ymd(date_reported)) %>%
  left_join(select(throughput, date_reported, id_ctar, include_day, estimated_cat1, year)) -> bites

##' Getting district level exclusion criteria
##' if submitted less than 10 forms total
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, 
         distcode %in% mada_districts$distcode, 
         id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)]) %>%
  group_by(id_ctar) %>%
  summarize(total_forms = n()) %>%
  complete(id_ctar = ctar_metadata$id_ctar, fill = list(total_forms = 0)) %>%
  right_join(ctar_metadata) %>%
  mutate(total_forms = ifelse(is.na(total_forms), 0, total_forms),
         exclude_dist = ifelse(total_forms > 10, 0, 1)) -> ctar_metadata

mada_districts$exclude_dist <- ctar_metadata$exclude_dist[match(mada_districts$catchment,
                                                                ctar_metadata$CTAR)]
##' A check
unique(mada_districts$catchment[mada_districts$exclude_dist == 1])

##' Getting bite incidence estimates for all districts
bites %>%
  ## filter known contacts and estimated ones based on throughput
  filter(estimated_cat1 == 0, include_day == 1) %>% 
  group_by(year, distcode) %>%
  summarize(bites = n()) -> bites_district
bites_district$CTAR <- mada_districts$catchment[match(bites_district$distcode, 
                                                       mada_districts$distcode)]
bites_district$id_ctar<- ctar_metadata$id_ctar[match(bites_district$CTAR, ctar_metadata$CTAR)]
bites_district %>%
  left_join(reporting) %>%
  filter(reporting > 0.25) %>% ## dont include any district for which catchment clinic had
  ## less than 25% reporting
  ## correct for reporting by year and ctar reported to 
  mutate(bites = bites/reporting) %>%
  group_by(distcode) %>%
  summarize(avg_bites = mean(bites, na.rm = TRUE),
            sd_bites = sd(bites, na.rm = TRUE), 
            nobs = n()) %>%
  complete(distcode = mada_districts$distcode, fill = list(avg_bites = 0)) -> bite_ests

##' Join bites with district and commune covariates 
mada_districts@data %>%
  filter(exclude_dist == 0) %>%
  mutate(catch = as.numeric(droplevels(catchment)), 
         group = as.numeric(droplevels(distcode))) %>%
  left_join(bite_ests) %>%
  arrange(group) -> district_bites

##' Communes
mada_communes$exclude_dist <- mada_districts$exclude_dist[match(mada_communes$distcode,
                                                                          mada_districts$distcode)]
mada_communes$ctch_dist <- mada_districts$catchment[match(mada_communes$distcode,
                                                                 mada_districts$distcode)]
mada_communes@data %>%
  filter(exclude_dist == 0) %>%
  mutate(catch = as.numeric(droplevels(ctch_dist)),
         group = as.numeric(droplevels(distcode))) %>%
  arrange(group) -> comm_covars
district_bites$end <- cumsum(rle(comm_covars$group)$lengths)
district_bites$start <- c(1, lag(district_bites$end)[-1] + 1)

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
  complete(commcode = mada_communes$commcode, fill = list(avg_bites = 0)) -> mora_bites 

mada_communes@data %>%
  filter(catchment == "Moramanga") %>%
  left_join(mora_bites) -> mora_bites
mora_bites$catch <- district_bites$catch[district_bites$catchment == "Moramanga"][1]

## Write out bite data and covariate data
fwrite(district_bites, "output/bites/district_bites.csv")
fwrite(comm_covars, "output/bites/comm_covars.csv")
fwrite(mora_bites, "output/bites/mora_bites.csv")

##' Session Info
out.session(path = "R/02_bitedata/03_estimate_biteinc.R", filename = "sessionInfo.csv")

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
national <- read.csv("data/processed/bitedata/natl_bites.csv")
moramanga <- read.csv("data/processed/bitedata/moramanga.csv")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
source("R/functions/data_functions.R")

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
  mutate(include = rle.days(no_patients, threshold = 10), 
         mean_throughput = mean(no_patients[include == 1]),
         sd_throughput = sd(no_patients[include == 1]),
         estimated_cat1 = ifelse(no_patients >= mean_throughput + 3*sd_throughput, 
                                     1, 0),
         year = year(date_reported)) -> throughput

## yearly reporting
throughput %>%
  group_by(year, id_ctar) %>%
  summarize(reporting = sum(include/365)) -> reporting

##' Output estimates of bite incidence with covariates 
##' ------------------------------------------------------------------------------------------------
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar), 
         type == "new") %>% 
  mutate(date_reported = ymd(date_reported)) %>%
  left_join(select(throughput, date_reported, id_ctar, include, estimated_cat1, year)) -> bites

## yearly bite incidence
bites %>%
  ## filter known contacts and estimated ones based on throughput
  filter(estimated_cat1 == 0, include == 1) %>% 
  group_by(year, distcode, id_ctar) %>%
  summarize(bites = n()) %>%
  left_join(reporting) %>% 
  ## TO DO: need to filter out any catchments with reporting less than that too!
  ## filter any years with reporting < 25%
  filter(reporting > 0.25) %>%
  ## correct for reporting
  mutate(bites_corrected = bites/reporting) %>%
  group_by(distcode, year) %>%
  summarize(total = sum(bites_corrected)) %>%
  group_by(distcode) %>%
  summarize(est_inc = mean(total, na.rm = TRUE)) -> bite_incidence

## Then join with covars and catchment variables (filter any to exclude)
bite_incidence %>%
  left_join(mada_districts@data, by = c("distcode" = "distcode")) -> ests

## then output

## filter out excluded catchments

##' 6. Make figure of bite incidence for Mada and Mora
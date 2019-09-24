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
library(rgeos)

## Read in data
bitedata <- read.csv("data/processed/master_bites.csv")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
source("R/functions/data_functions.R")

##' 1. Getting reporting and contacts
##' ------------------------------------------------------------------------------------------------
bitedata %>%
  filter(source != "Moramanga", year(date_reported) > 2013, 
         year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n()) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
           fill = list(no_patients = 0)) -> throughput


## rle.days = Helper function for getting which days to include (moved to functions from data_functions.R)
## get if estimated contact by the throughput mean/sd
throughput %>%
  group_by(id_ctar) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(include = rle.days(no_patients, threshold = 10), 
         mean_throughput = mean(no_patients[include == 1]),
         sd_throughput = sd(no_patients[include == 1]),
         estimated_contact = ifelse(no_patients >= mean_throughput + 3*sd_throughput, 
                                     1, 0),
         year = year(date_reported)) -> throughput


## Fig S1A days to include based on a given threshold
## yearly reporting
throughput %>%
  group_by(year, id_ctar) %>%
  summarize(reporting = sum(include/365)) -> reporting

throughput %>%
  left_join(reporting) %>%
  mutate(n = ifelse(include == 1, no_patients, NA)) -> plot_reporting

p <- ggplot(data = plot_reporting, aes(x = date_reported, y = reorder(id_ctar, reporting))) + 
  geom_tile(aes(fill = n)) +
  scale_fill_gradientn(colours = c("white", "purple", "black"), values = c(0, 0.1, 1),
                       na.value = alpha("lightgrey", 1), name = "Number of \n patients") +
  xlim(ymd("2014-01-01"), ymd("2017-12-31")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Year") +
  ylab("ARMC") +
  labs(tag = "A") +
  theme(panel.background = element_blank())

## FigS1B range of reporting ests based on thresholds
col_vals <- ctar_metadata$fill
names(col_vals) <- ctar_metadata$id_ctar

throughput %>%
  group_by(id_ctar) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(include_5 = rle.days(no_patients, threshold = 15), 
         include_15 = rle.days(no_patients, threshold = 5),
         include_30 = rle.days(no_patients, threshold = 30),
         mean_throughput = mean(no_patients[include == 1]),
         sd_throughput = sd(no_patients[include == 1]),
         estimated_contact = ifelse(no_patients >= mean_throughput + 3*sd_throughput, 
                                    1, 0),
         year = year(date_reported)) -> throughput


## Fig S1A days to include based on a given threshold
## yearly reporting
throughput %>%
  group_by(year, id_ctar) %>%
  summarize(reporting = sum(include/365)) -> reporting
n <- clinic_reporting %>%
  mutate(prop15 = prop, prop5 = get.days(doses_wide = doses_wide, threshold = 5)[[1]]$prop,
         prop30 = get.days(doses_wide = doses_wide, threshold = 30)[[1]]$prop) %>%
  ggplot(., aes(x = reorder(ctar, prop), color = ctar)) +
  geom_hline(yintercept = 0.25, color = "darkgrey", alpha = 0.5) +
  geom_boxplot(aes(ymin = prop30, lower = prop15, middle = prop15, upper = prop15, ymax = prop5),
               stat = "identity") +
  scale_color_manual(values = col_vals) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  xlab("ARMC") +
  ylab("Proportion of days included") +
  coord_flip() +
  labs(tag = "B")

##' Output estimates of bite incidence with covariates 
##' ------------------------------------------------------------------------------------------------
bitedata %>%
  filter(source != "Moramanga", year(date_reported) > 2013, 
         year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar), 
         type == "new") %>% 
  mutate(date_reported = ymd(date_reported)) %>%
  left_join(select(throughput, date_reported, id_ctar, include, estimated_contact, year)) -> bites

## yearly bite incidence
bites %>%
  ## filter known contacts and estimated ones based on throughput
  filter(estimated_contact == 0, include == 1) %>% 
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
  left_join(mada_districts@data, by = c("distcode" = "distcod")) -> ests

## then output

## filter out excluded catchments

##' 6. Make figure of bite incidence for Mada and Mora
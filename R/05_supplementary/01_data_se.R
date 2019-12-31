####################################################################################################
##' Looking at reporting cut offs and translating to vial estimates 
##' Details:  
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(tidyverse)
library(data.table)
library(lubridate)
select <- dplyr::select
source("R/functions/utils.R")
source("R/functions/data_functions.R")

## Read in raw and processed data
national<- fread("data/processed/bitedata/national.csv")
peripheral <- read.csv("data/raw/bitedata/peripheral/SaisieRage_DATA_2018-09-21_1755.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
load("data/raw/bitedata/ipm/ipm.rda")

## Metadata
ctar_metadata <- fread("data/raw/ctar_metadata.csv")

##' Getting daily throughput for each clinic
##' ------------------------------------------------------------------------------------------------
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n()) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
           fill = list(no_patients = 0)) -> patient_ts

##' rle.days = Helper function for getting which days to include (moved to data_functions.R)
patient_ts %>%
  group_by(id_ctar) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(include_nozeros = rle.days(no_patients, threshold = 0), 
         include_5 = rle.days(no_patients, threshold = 5),
         include_10 = rle.days(no_patients, threshold = 10),
         include_15 = rle.days(no_patients, threshold = 15),
         include_30 = rle.days(no_patients, threshold = 30),
         include_all = rle.days(no_patients, threshold = Inf),
         year = year(date_reported)) -> throughput
throughput$ctar <- ctar_metadata$CTAR[match(throughput$id_ctar, ctar_metadata$id_ctar)]
throughput$no_patients <- ifelse(throughput$include_15 == 0, NA, throughput$no_patients)

write.csv(throughput, "output/sensitivity/throughput.csv", row.names = FALSE)

##' yearly reporting
throughput %>%
  group_by(year, id_ctar) %>%
  summarize_at(vars(starts_with("include")), function (x) sum(x)/365) -> reporting
reporting$ctar <- ctar_metadata$CTAR[match(reporting$id_ctar, ctar_metadata$id_ctar)]
write.csv(reporting, "output/sensitivity/reporting.csv", row.names = FALSE)

##' Estimates of vials from reporting 
##' ------------------------------------------------------------------------------------------------
prop.f <- function(num, denom) num/denom ## helper function

## Getting bites at the ctar level
patient_ts %>%
  mutate(year = year(date_reported)) %>%
  group_by(year, id_ctar) %>% 
  summarize(no_patients = sum(no_patients)) %>%
  left_join(reporting) %>%
  mutate_at(vars(starts_with("include")), prop.f, num = quote(no_patients)) -> ctar_bites

## Do average of 3 doses per patient at days 0, 3, 7
## Get completion of subsequent doses by clinic
get.vials.3 <- function(x) {
  day0 <- round(runif(x, min = 1, max = 365))
  days <- data.table(days = c(day0, day0 + 3, day0 + 7))
  return(sum(days[, .(.N), by = days][, ceiling(N/2)]))
}

get.vials.4 <- function(x) {
  day0 <- round(runif(x, min = 1, max = 365))
  days <- data.table(days = c(day0, day0 + 3, day0 + 7, day0 + 28))
  return(sum(days[, .(.N), by = days][, ceiling(N/2)]))
}

mean.vials.3 <- function(patients, n){ 
  mean(replicate(n, get.vials.3(patients)))
}

mean.vials.4 <- function(patients, n){ 
  mean(replicate(n, get.vials.4(patients)))
}

ctar_bites %>%
  ungroup() %>%
  mutate_at(vars(starts_with("include")), 
            ~case_when(is.na(.) ~ 0, 
                       !is.na(.) ~ .)) %>%
  mutate_at(vars(starts_with("include")), ~unlist(lapply(., mean.vials.3, n = 100))) %>%
  group_by(id_ctar) %>%
  mutate(vials = 3) -> vial_ests_3

ctar_bites %>%
  ungroup() %>%
  mutate_at(vars(starts_with("include")), 
            ~case_when(is.na(.) ~ 0, 
                       !is.na(.) ~ .)) %>%
  mutate_at(vars(starts_with("include")), ~unlist(lapply(., mean.vials.4, n = 100))) %>%
  group_by(id_ctar) %>%
  mutate(vials = 4) -> vial_ests_4

vial_ests <- bind_rows(vial_ests_3, vial_ests_4)

vial_ests %>%
  filter(year != 2017) %>%
  select(-year) %>%
  group_by(id_ctar, ctar, vials) %>%
  summarize_all(sum) %>%
  pivot_longer(cols = starts_with("include")) -> vials_summed

ctar_metadata %>%
  mutate(vials_observed = doses_2014 + doses_2015 + doses_2016) %>%
  select(id_ctar, vials_observed) %>%
  right_join(vials_summed) %>%
  filter(!is.na(vials_observed), no_patients > 10) %>%
  mutate(cut_off = case_when(name == "include_nozeros" ~ 0, 
                             name == "include_5" ~ 5, 
                             name == "include_10" ~ 10, 
                             name == "include_15" ~ 15,
                             name == "include_30" ~ 30, 
                             name == "include_all" ~ Inf)) -> vial_ests
vial_ests %>%
  pivot_wider(names_from = "vials", names_prefix = "mean", values_from = "value") -> vial_comp

reporting %>%
  group_by(ctar) %>%
  summarize(reporting = mean(include_15, na.rm = TRUE)) %>%
  right_join(filter(vial_comp, cut_off == 15 | cut_off == Inf)) -> vials_to_plot

## output vials_to_plot
write.csv(vials_to_plot, "output/sensitivity/vial_comp.csv", row.names = FALSE)

##' Contacts based on throughput cutoff
##' ------------------------------------------------------------------------------------------------
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n(), 
            no_known = sum(known_cat1, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
           fill = list(no_patients = 0)) %>%
  group_by(id_ctar) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(include_day = rle.days(no_patients, threshold = 15),
         mean_throughput = mean(no_patients[include_day == 1]),
         sd_throughput = sd(no_patients[include_day == 1]),
         estimated_cat1 = ifelse(no_patients >= mean_throughput + 3*sd_throughput, 
                                 1, 0),
         year = year(date_reported), 
         total = sum(no_patients), 
         ctar = ctar_metadata$CTAR[match(id_ctar, ctar_metadata$id_ctar)]) %>%
  filter(total > 10) -> contacts_natl

## Output contacts national
write.csv(contacts_natl, "output/sensitivity/contacts_natl.csv", row.names = FALSE)

##' Testing method with Moramanga data
moramanga %>%
  mutate(date_reported = ymd(date_reported)) %>%
  filter(date_reported >= "2016-10-01", date_reported <= "2019-06-01") %>%
  group_by(date_reported) %>%
  summarise(no_patients = n(), 
            known_contacts = sum(known_cat1)) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), 
           fill = list(no_patients = 0, known_contacts = 0)) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(mean_throughput = mean(no_patients),
         sd_throughput = sd(no_patients)) -> contacts_mora

est_contacts <- function(x, mean_throughput, cut_off, sd_throughput) {
  ifelse(x >= mean_throughput + cut_off*sd_throughput, 1, 0)
}

map_dfc(seq(1, 10, by = 0.25), function(x) ifelse(contacts_mora$no_patients >= 
                                                    contacts_mora$mean_throughput + 
                                                    contacts_mora$sd_throughput*x, 1, 0)) %>%
  setNames(seq(1, 10, by = 0.25)) %>%
  bind_cols(contacts_mora, .) %>%
  pivot_longer(`1`:`10`, names_to = "sd", values_to = "excluded") %>%
  group_by(sd) %>%
  mutate(total_known = sum(known_contacts), 
         total_bites = sum(no_patients - known_contacts)) %>%
  filter(excluded == 1) %>%
  summarize(contacts= sum(known_contacts)/total_known[1], 
            bites = sum(no_patients - known_contacts)/total_bites[1]) %>%
  pivot_longer(contacts:bites, names_to = "prop", 
               values_to = "excluded") -> contacts_mora

##' Output contacts moramanga
write.csv(contacts_mora, "output/sensitivity/contacts_mora.csv", row.names = FALSE)

##' Saving session info
out.session(path = "R/01_gis/01_get_friction.R", filename = "sessionInfo.csv")

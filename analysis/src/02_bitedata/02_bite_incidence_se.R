# ------------------------------------------------------------------------------
#' Looking at reporting cut offs and translating to vial estimates
# ------------------------------------------------------------------------------

start <- Sys.time()

# Set up
library(tidyverse)
library(data.table)
library(lubridate)
select <- dplyr::select
source(here::here("R", "utils.R"))
source(here_safe("R/data_functions.R"))

# Read in data
national <- fread(here_safe("data-raw/out/bitedata/national.csv"))
moramanga <- fread(here_safe("data-raw/out/bitedata/moramanga.csv"))
ctar_metadata <- fread(here_safe("data-raw/out/clinics/ctar_metadata.csv"))

# Getting daily throughput for each clinic -------------------------------------
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018,
         !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n()) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported),
                               by = "day"), id_ctar,
           fill = list(no_patients = 0)) -> patient_ts

# rle_days = Helper function for getting which days to include
patient_ts %>%
  group_by(id_ctar) %>%
  arrange(date_reported, .by_group = TRUE) %>%
  mutate(include_nozeros = rle_days(no_patients, threshold = 0),
         include_5 = rle_days(no_patients, threshold = 5),
         include_10 = rle_days(no_patients, threshold = 10),
         include_15 = rle_days(no_patients, threshold = 15),
         include_30 = rle_days(no_patients, threshold = 30),
         include_all = rle_days(no_patients, threshold = Inf),
         year = year(date_reported)) -> throughput
throughput$ctar <- ctar_metadata$CTAR[match(throughput$id_ctar,
                                            ctar_metadata$id_ctar)]
throughput$no_patients <- ifelse(throughput$include_15 == 0, NA,
                                 throughput$no_patients)

write_create(throughput, "analysis/out/sensitivity/throughput.csv", write_csv)

# yearly reporting
throughput %>%
  group_by(year, id_ctar) %>%
  summarize_at(vars(starts_with("include")),
               function (x) sum(x)/365) -> reporting
reporting$ctar <- ctar_metadata$CTAR[match(reporting$id_ctar,
                                           ctar_metadata$id_ctar)]
write_create(reporting, "analysis/out/sensitivity/reporting.csv", write_csv)

# Estimates of vials from reporting --------------------------------------------
throughput %>%
  group_by(id_ctar) %>%
  summarize_at(vars(starts_with("include")),
               function (x) sum(x)/(365*4)) -> reporting_total

prop.f <- function(num, denom) num/denom # helper function

# Getting bites at the ctar level over 4 year period
patient_ts %>%
  group_by(id_ctar) %>%
  summarize(no_patients = sum(no_patients)) %>%
  left_join(reporting_total) %>%
  mutate_at(vars(starts_with("include")),
            prop.f, num = quote(no_patients)) -> ctar_bites

# Do average of 3 doses per patient at days 0, 3, 7 over 4 year period
# Get completion of subsequent doses by clinic
get_vials.3 <- function(x) {
  day0 <- floor(runif(x, min = 1, max = 365*4))
  days <- tabulate(c(day0, day0 + 3, day0 + 7))
  return(sum(ceiling(days)/2))
}

get_vials.4 <- function(x) {
  day0 <- floor(runif(x, min = 1, max = 365*4))
  days <- tabulate(c(day0, day0 + 3, day0 + 7, day0 + 28))
  return(sum(ceiling(days)/2))
}

mean.vials.3 <- function(patients, n){
  mean(replicate(n, get_vials.3(patients)))
}

mean.vials.4 <- function(patients, n){
  mean(replicate(n, get_vials.4(patients)))
}

ctar_bites %>%
  ungroup() %>%
  mutate_at(vars(starts_with("include")),
            ~case_when(is.na(.) ~ 0,
                       !is.na(.) ~ .)) %>%
  mutate_at(vars(starts_with("include")),
            ~unlist(lapply(., mean.vials.3, n = 100))) %>%
  group_by(id_ctar) %>%
  mutate(vials = 3) -> vial_ests_3

ctar_bites %>%
  ungroup() %>%
  mutate_at(vars(starts_with("include")),
            ~case_when(is.na(.) ~ 0,
                       !is.na(.) ~ .)) %>%
  mutate_at(vars(starts_with("include")),
            ~unlist(lapply(., mean.vials.4, n = 100))) %>%
  group_by(id_ctar) %>%
  mutate(vials = 4) -> vial_ests_4

vial_ests <- bind_rows(vial_ests_3, vial_ests_4)

vial_ests %>%
  group_by(id_ctar, vials) %>%
  summarize_all(sum) %>%
  pivot_longer(cols = starts_with("include")) -> vials_summed

ctar_metadata %>%
  mutate(vials_observed = doses_2014 + doses_2015 + doses_2016 + doses_2017) %>%
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
  pivot_wider(names_from = "vials", names_prefix = "mean",
              values_from = "value") -> vial_comp

reporting %>%
  group_by(ctar, id_ctar) %>%
  summarize(reporting = mean(include_15, na.rm = TRUE)) %>%
  right_join(filter(vial_comp, cut_off == 15 | cut_off == Inf)) -> vials_to_plot

# output vials_to_plot
write_create(vials_to_plot, "analysis/out/sensitivity/vial_comp.csv",
             write_csv)

# Saving session info
out_session(logfile = "logs/log_local.csv", start = start, ncores = 1)

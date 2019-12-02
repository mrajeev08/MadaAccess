####################################################################################################
##' Looking at reporting cut offs and translating to vial estimates 
##' Details:  
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(tidyverse)
library(data.table)
library(rgdal)
library(lubridate)
library(patchwork)
select <- dplyr::select
source("R/functions/utils.R")
source("R/functions/data_functions.R")

## Read in data
national <- fread("data/processed/bitedata/national.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")


##' Reporting sensitivity
##' ------------------------------------------------------------------------------------------------
##' Getting daily throughput for each clinic
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n()) %>%
  ungroup() %>%
  complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
           fill = list(no_patients = 0)) -> patient_ts

##' rle.days = Helper function for getting which days to include (moved to functions from data_functions.R)
##' and also identify potential CAT 1 by the throughput mean/sd
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

##' yearly reporting
##' sum the total # of days included over # of days in year (365)
throughput %>%
  group_by(year, id_ctar) %>%
  summarize_at(vars(starts_with("include")), function (x) sum(x)/365) -> reporting
reporting$ctar <- ctar_metadata$CTAR[match(reporting$id_ctar, ctar_metadata$id_ctar)]

catch_cols <- ctar_metadata$color
names(catch_cols) <- ctar_metadata$CTAR

## Figure S1.1A
throughput$ctar <- ctar_metadata$CTAR[match(throughput$id_ctar, ctar_metadata$id_ctar)]
throughput$no_patients <- ifelse(throughput$include_15 == 0, NA, throughput$no_patients)
figS1.1A <- ggplot(data = throughput, aes(x = date_reported, y = reorder(ctar, include_15))) + 
  geom_tile(aes(fill = no_patients)) +
  scale_fill_gradientn(colours = c("white", "purple", "black"), values = c(0, 0.1, 1),
                       na.value = alpha("lightgrey", 0.25), name = "Number of \n patients") +
  xlim(ymd("2014-01-01"), ymd("2017-12-31")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Year") +
  ylab("ARMC") +
  labs(tag = "A")

## Figure S1.1B
figS1.1B <- ggplot(data = reporting, aes(x = reorder(ctar, include_15), color = ctar, group = interaction(ctar, year))) +
  geom_boxplot(aes(ymin = include_30, lower = include_15, middle = include_15, upper = include_15, 
                   ymax = include_5),
               stat = "identity") +
  geom_hline(yintercept = 0.25, color = "darkgrey", alpha = 0.5) +
  scale_color_manual(values = catch_cols, guide = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank()) +
  xlab("ARMC") +
  ylab("Proportion of days included") +
  coord_flip() +
  labs(tag = "B") 


figS1.1 <- figS1.1A / figS1.1B

## figS1.1
ggsave("figs/S1.1.jpeg", figS1.1, device = "jpeg", width = 7, height = 7)

##' Estimates of vials from reporting 
##' ------------------------------------------------------------------------------------------------
f <- function(num, denom) num/denom
patient_ts %>%
  mutate(year = year(date_reported)) %>%
  group_by(year, id_ctar) %>% 
  summarize(no_patients = sum(no_patients)) %>%
  left_join(reporting) %>%
  mutate_at(vars(starts_with("include")), f, num = quote(no_patients)) -> ctar_bites
  
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
                             name == "include_all" ~ Inf)) %>%
  pivot_wider(names_from = "vials", names_prefix = "mean", values_from = "value") -> vial_comp


ggplot(data = vial_comp, aes(x = ctar, fill = factor(cut_off), group = interaction(ctar, cut_off))) +
  geom_boxplot(aes(ymin = mean3 - vials_observed, lower = mean3 - vials_observed, middle = 0, 
                   upper = mean4 - vials_observed, ymax = mean4 - vials_observed),
               stat = "identity") + 
  scale_fill_brewer(type = "seq", palette = "Blues") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank()) +
  xlab("Observed") +
  ylab("Estimated") +
  coord_flip()

ggplot(data = vial_comp, aes(x = (value - vials_observed)/vials_observed, size = vials_observed, 
                             y = ctar, color = factor(cut_off))) +
  geom_point() +
  geom_vline(xintercept = 0, color = "grey", size = 1.1) +
  scale_color_brewer(type = "seq", palette = "Blues") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank()) +
  xlab("ARMC") +
  ylab("Vial estimates")

vial_comp %>%
  group_by(cut_off, ctar) %>%
  mutate(se_squared_3 = sum((mean4 - vials_observed)^2)) -> check

ggplot(data = check, aes(x = factor(cut_off), y = se_squared_3)) +
  geom_boxplot(outlier.shape = NA) +
  ggbeeswarm::geom_beeswarm(color = "black", alpha = 0.5)

##' Contacts 
##' ------------------------------------------------------------------------------------------------
##' Figure S1.2A example of cut-offs
national %>%
  filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
  mutate(date_reported = ymd(date_reported)) %>%
  group_by(date_reported, id_ctar) %>%
  summarise(no_patients = n()) %>%
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
  filter(total > 10) -> contacts


##' FigS1.2A
figS1.2A <- ggplot(data = contacts, aes(x = ctar, y = no_patients, color = as.factor(estimated_cat1))) +
  scale_color_manual(name = "Exclude", values = c("navy", "red"), labels = c("No", "Yes")) +
  ylab("Number of patients per day") +
  xlab("ARMC") +
  geom_jitter(alpha = 0.5, width = 0.25) +
  coord_flip() +
  labs(tag = "A")

##' Testing with Moramanga
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
               values_to = "excluded") -> contacts_cutoff

##' Plot
figS1.2B <- ggplot(contacts_cutoff, aes(x = as.numeric(sd), y = excluded, color = prop)) +
  geom_line() +
  scale_color_manual(name = "Type of patient", values = c("blue", "red"), 
                     labels = c("Cat II/II", "Cat I")) + 
  xlab("Number of standard \n deviations above the mean") +
  ylab("Proportion excluded") +
  labs(tag = "B")

##' Also make the point that contacts have different ttime distribution than reported bites (so
##' more for getting the impact of travel times right)
figS1.2 <- figS1.2A | figS1.2B

## figS1.2
ggsave("figs/S1.2.jpeg", figS1.2, device = "jpeg", height = 7, width = 9)

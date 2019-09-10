####################################################################################################
##' Generating master data sets
##' Details: deets 
##' Author: author 
##' Date started:  date started 
####################################################################################################
rm(list = ls())

## Libraries
library(stringdist)
library(rgdal)
library(dplyr)
library(lubridate)

## 1. Reading in data
## Read in and format shapefiles
## have to change the codes a bit to account for some formatting
mada_districts <- readOGR(
  "data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes <- readOGR(
  "data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

## Read in raw bite data
peripheral <- read.csv("data/raw/bite_data/peripheral/SaisieRage_DATA_2018-09-21_1755.csv")
load("data/raw/bite_data/ipm/ipm.rda")
moramanga <- read.csv("data/raw/bite_data/moramanga/CTAR_%28V2%29_20190908190136.csv")

##' 2. Match admin names 
##' ------------------------------------------------------------------------------------------------
## Peripheral data to the district
peripheral$distcode <- paste0(substring(peripheral$district, 1, 1), 
                              substring(peripheral$district, 3, 8))
## automatically do communes that were matched 
## using best fixed matches with minimum matching distance <= 2
peripheral_comm_matches <- read.csv("data/raw/match_names/peripheral_comm_matches.csv")
mada_communes$matchcode <- interaction(mada_communes$distcode, tolower(mada_communes$ADM3_EN))
peripheral_comm_matches %>%
  mutate(match = ifelse(min_fixed <= 2, as.character(fixed_best), NA),
         matchcode_comm = interaction(nest, match), 
         matchcode_data = interaction(nest, names_tomatch),
         commcode = mada_communes$ADM3_PCODE[match(matchcode_comm, 
                                                   mada_communes$matchcode)]) %>%
  select(names_tomatch, min_fixed, match, matchcode_comm, 
         matchcode_data, commcode) -> peripheral_comm_matches
peripheral %>%
  mutate(matchcode_data = interaction(distcode, tolower(commune))) %>%
  left_join(peripheral_comm_matches) -> peripheral

## Also do contact matching (identify known contacts)
known_contacts <- read.csv("data/processed/matched_names/peripheral_notes_knowncontacts.csv")
peripheral$known_contact <- known_contacts$known_contact[match(peripheral$remarque, 
                                                                known_contacts$Note)]

## IPM data to the district
names_matched <- read.csv("data/processed/matched_names/ipm_dist_matched.csv")
names_matched$distcode <- mada_districts$distcode[match(names_matched$match, 
                                                        tolower(mada_districts$ADM2_EN))]
IPM$distcode <- names_matched$distcode[match(tolower(IPM$fiv), names_matched$names_tomatch)]

## automatically do communes that were matched to within 4 differences
ipm_comm_matches <- read.csv("data/raw/match_names/ipm_comm_matches.csv")
ipm_comm_matches %>%
  mutate(match = ifelse(min_fixed <= 2, as.character(fixed_best), NA),
         matchcode_comm = interaction(nest, match), 
         matchcode_data = interaction(nest, names_tomatch),
         commcode = mada_communes$ADM3_PCODE[match(matchcode_comm, 
                                                   mada_communes$matchcode)]) %>%
  select(names_tomatch, min_fixed, match, matchcode_comm, matchcode_data, commcode) -> ipm_comm_matches
IPM %>%
  mutate(matchcode_data = interaction(distcode, tolower(fir))) %>%
  left_join(ipm_comm_matches) -> IPM

## Moramanga data
## To the district
moramanga$district <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\, "), "[", 2)
moramanga$district <- gsub(" \\(District\\)", "", moramanga$district)
moramanga_dist_matches <- read.csv("data/processed/matched_names/moramanga_dist_matched.csv")
moramanga_dist_matches$distcode <- mada_districts$distcode[match(moramanga_dist_matches$match, 
                                                                 tolower(mada_districts$ADM2_EN))]
moramanga$distcode <- moramanga_dist_matches$distcode[match(tolower(moramanga$district), 
                                                            moramanga_dist_matches$names_tomatch)]

## To the commune
## pull in manually matched districts
moramanga$commune <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\("), "[", 1)
moramanga$commune <- trimws(moramanga$commune, which = "right")
moramanga_comm_matches <- read.csv("data/raw/match_names/moramanga_comm_matches.csv")

moramanga_comm_matches %>%
  mutate(match = ifelse(min_fixed <= 2, as.character(fixed_best), NA),
         matchcode_comm = interaction(nest, match), 
         matchcode_data = interaction(nest, names_tomatch),
         commcode = mada_communes$ADM3_PCODE[match(matchcode_comm, 
                                                   mada_communes$matchcode)]) %>%
  select(min_fixed, match, matchcode_comm, matchcode_data, commcode) -> moramanga_comm_matches
moramanga %>%
  mutate(matchcode_data = interaction(distcode, tolower(commune))) %>%
  left_join(moramanga_comm_matches) -> moramanga

##' 3. Format other columns
##' ------------------------------------------------------------------------------------------------
##' Date reported, CTAR, type, commcode, distcode, known_contact, notes
##' 
## Peripheral
peripheral %>%
  mutate(type = "new", date_reported = ymd(date_de_consultation), 
         source = "peripheral") %>%
  select(date_reported, type, ctar, id_ctar, distcode, commcode, known_contact, source) -> peripheral_clean

## IPM
IPM %>%
  mutate(type = case_when(categorie == "N" ~ "new", 
                          categorie == "R" ~ "restart", 
                          categorie == "T" ~ "transfer"), 
         date_reported = ymd(dat_consu), 
         known_contact = ifelse(type_cont == "C", "Y", "N"),
         ctar = "IPM", id_ctar = 5, source = "IPM") %>%
  select(date_reported, type, ctar, id_ctar, distcode, commcode, known_contact, source) -> IPM_clean
  
## Moramanga
moramanga %>%
  mutate(type = if_else(Type.of.consultation == "Passage", "transfer",
                        "new"), 
         date_reported = dmy(Date.of.consultation), 
         known_contact = ifelse(Type.of.consultation == "Contact with suspect case", 
                           "Y", "N"), 
         ctar = "Moramanga", id_ctar = 8, source = "Moramanga") %>% 
  select(date_reported, type, ctar, id_ctar, distcode, commcode, known_contact, 
         source) -> moramanga_clean

##' Output master data and summary stats
##' ------------------------------------------------------------------------------------------------
master <- bind_rows(moramanga_clean, IPM_clean, peripheral_clean)
write.csv(master, "data/processed/master_bites.csv", row.names = FALSE)

## With known commune and district
sum(table(as.factor(moramanga$distcode)))/nrow(moramanga)
sum(table(as.factor(moramanga$commcode)))/nrow(moramanga)

sum(table(as.factor(IPM$distcode)))/nrow(IPM)
sum(table(as.factor(IPM$commcode)))/nrow(IPM)

sum(table(as.factor(peripheral$distcode)))/nrow(peripheral)
sum(table(as.factor(peripheral$commcode)))/nrow(peripheral)

sum(table(as.factor(master$commcode)))/nrow(master)

## With known date
sum(table(master$date_reported))/nrow(master)

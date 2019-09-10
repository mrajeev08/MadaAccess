####################################################################################################
##' CLeaning district and commune names 
##' Details: Using some manual and some automated name matching for administrative units for the three
##' data sets 
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(stringdist)
library(rgdal)
library(dplyr)

## source matching function
source("R/functions/match.names.R")

## Read in and format shapefiles
## have to change the codes a bit to account for some formatting
mada_districts <- readOGR("data/raw/shapefiles/mdg_admbnda_adm2_BNGRC_OCHA_20181031/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes <- readOGR("data/raw/shapefiles/mdg_admbnda_adm3_bngrc_ocha_20181031/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

## ## match commune names in peripheral data (leave manually unmatched but any with below 3 match to name)
peripheral <- read.csv("data/raw/bite_data/peripheral/SaisieRage_DATA_2018-09-21_1755.csv")
peripheral$distcode <- paste0(substring(peripheral$district, 1, 1), 
                              substring(peripheral$district, 3, 8))
peripheral_comm_matches <- match.admin (data_names = peripheral$commune, data_nest = peripheral$distcode, 
             match_names = mada_communes$ADM3_EN, match_nest = mada_communes$distcode, 
             match_threshold = Inf, match_method = "osa", nested = TRUE)
write.csv(peripheral_comm_matches, "data/raw/match_names/peripheral_comm_matches.csv", row.names = FALSE)

## match district names in IPM
load("data/raw/bite_data/ipm/ipm.rda")
head(IPM)
ipm_dist_matches <- match.admin(data_names = IPM$fiv, data_nest = NULL, 
                                match_names = mada_districts$ADM2_EN, match_nest = NULL,
                                match_threshold = Inf, match_method = "osa", nested = FALSE)
write.csv(ipm_dist_matches, "data/raw/match_names/ipm_dist_matches.csv", row.names = FALSE)

## after manual matching of names in IPM data
names_matched <- read.csv("data/processed/matched_names/ipm_dist_matches_processed_Sep262019.csv")
names_matched$distcode <- mada_districts$distcode[match(names_matched$match, 
                                                        tolower(mada_districts$ADM2_EN))]
IPM$distcode <- names_matched$distcode[match(tolower(IPM$fiv), names_matched$names_tomatch)]
ipm_comm_matches <- match.admin(data_names = IPM$fir, data_nest = IPM$distcode, 
                                match_names = mada_communes$ADM3_EN, 
                                match_nest = mada_communes$distcode, match_threshold = 15, 
                                match_method ="osa", nested = TRUE)
write.csv(ipm_comm_matches, "data/raw/match_names/ipm_comm_matches.csv", row.names = FALSE)

## match commune names in Moramanga data
moramanga <- read.csv("data/raw/bite_data/moramanga/CTAR_%28V2%29_20190908190136.csv")
moramanga$commune <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\("), "[", 1)
moramanga$commune <- trimws(moramanga$commune, which = "right")
moramanga$district <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\, "), "[", 2)
moramanga$district <- gsub(" \\(District\\)", "", moramanga$district)

moramanga_dist_matches <- match.admin(data_names = moramanga$district, data_nest = NULL, 
                                      match_names = mada_districts$ADM2_EN, match_nest = NULL, 
                                      match_threshold = Inf, match_method = "osa", nested = FALSE)
write.csv(moramanga_dist_matches, "data/raw/match_names/moramanga_dist_matches.csv", row.names = FALSE)

## Manually matched districts
## Then automatically match communes
moramanga_dist_matches <- read.csv("data/processed/matched_names/moramanga_dist_matches_Sep092019.csv")
moramanga_dist_matches$distcode <- mada_districts$distcode[match(moramanga_dist_matches$match, 
                                                        tolower(mada_districts$ADM2_EN))]
moramanga$distcode <- moramanga_dist_matches$distcode[match(tolower(moramanga$district), 
                                                   moramanga_dist_matches$names_tomatch)]
moramanga_comm_matches <- match.admin(data_names = moramanga$commune, data_nest = moramanga$distcode,
                                      match_names = mada_communes$ADM3_EN, 
                                      match_nest = mada_communes$distcode, 
                                      match_threshold = Inf, match_method = "osa", nested = TRUE)
moramanga_comm_matches <- filter(moramanga_comm_matches, !is.na(names_tomatch))
moramanga_comm_matches$match <- moramanga_comm_matches$X1
write.csv(moramanga_comm_matches, "data/processed/matched_names/moramanga_comm_matches_Sep092019", 
          row.names = FALSE)

## Also need to match notes in the peripheral data (as contacts or not)
write.csv(table(peripheral$remarque), "data/raw/match_names/peripheral_notes_contacts.csv", 
          row.names = FALSE)

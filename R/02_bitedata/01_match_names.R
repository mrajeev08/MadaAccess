# ------------------------------------------------------------------------------------------------ #
#' Cleaning district and commune names 
#' Details: Using some manual and some fuzzy  matching for administrative units for the three
#' data sets                                                                                        
# ------------------------------------------------------------------------------------------------ #

# Set-up --------------------------------------------------------------------------------------
library(stringdist)
library(rgdal)
library(dplyr)

# source matching function
source("R/functions/out.session.R")
source("R/functions/match_names.R")

# Read in shapefiles
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

# Match commune names in peripheral ARMC data -------------------------------------------------
# automatch ones with < 3 fixed distance
peripheral <- read.csv("data/raw/bitedata/peripheral/SaisieRage_DATA_2018-09-21_1755.csv")
peripheral$distcode <- paste0(substring(peripheral$district, 1, 1), 
                              substring(peripheral$district, 3, 8))
peripheral_comm_matches <- match.admin(data_names = peripheral$commune, 
                                       data_nest = peripheral$distcode, 
                                       match_names = mada_communes$commune, 
                                       match_nest = mada_communes$distcode,
                                       match_method = "osa", nested = TRUE)
write.csv(peripheral_comm_matches, "data/raw/match_names/peripheral_comm_matches.csv", 
          row.names = FALSE)

# Also get table of notes to match to see if known Category 1 (manually match afterwards)
peripheral %>%
  group_by(notes = remarque) %>%
  summarize(Freq = n()) -> notes_to_match
write.csv(notes_to_match, "data/raw/match_names/peripheral_notes_to_match.csv", row.names = FALSE)

# Match admin names in IPM data ---------------------------------------------------------------
load("data/raw/bitedata/ipm/ipm.rda")
head(IPM)
ipm_dist_matches <- match.admin(data_names = IPM$fiv, data_nest = NULL, 
                                match_names = mada_districts$district, match_nest = NULL,
                                match_method = "osa", nested = FALSE)
write.csv(ipm_dist_matches, "data/raw/match_names/ipm_dist_matches.csv", row.names = FALSE)

# after manual matching of names in IPM data
names_matched <- read.csv("data/processed/matched_names/ipm_dist_matched.csv")
names_matched$distcode <- mada_districts$distcode[match(names_matched$match, 
                                                        tolower(mada_districts$district))]
IPM$distcode <- names_matched$distcode[match(tolower(IPM$fiv), names_matched$names_tomatch)]
ipm_comm_matches <- match.admin(data_names = IPM$fir, data_nest = IPM$distcode, 
                                match_names = mada_communes$commune, 
                                match_nest = mada_communes$distcode, 
                                match_method ="osa", nested = TRUE)
write.csv(ipm_comm_matches, "data/raw/match_names/ipm_comm_matches.csv", row.names = FALSE)

# Match admin names in moramanga data ---------------------------------------------------------
moramanga <- read.csv("data/raw/bitedata/moramanga/CTAR_%28V3%29_20190918150219.csv")
moramanga$commune <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\("), "[", 1)
moramanga$commune <- trimws(moramanga$commune, which = "right")
moramanga$district <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\, "), "[", 2)
moramanga$district <- gsub(" \\(District\\)", "", moramanga$district)

moramanga_dist_matches <- match.admin(data_names = moramanga$district, data_nest = NULL, 
                                      match_names = mada_districts$district, match_nest = NULL, 
                                      match_method = "osa", nested = FALSE)
write.csv(moramanga_dist_matches, "data/raw/match_names/moramanga_dist_matches.csv", row.names = FALSE)

# Pull in manually matched districts
# Then automatically match communes
moramanga_dist_matches <- read.csv("data/processed/matched_names/moramanga_dist_matched.csv")
moramanga_dist_matches$distcode <- mada_districts$distcode[match(moramanga_dist_matches$match, 
                                                        tolower(mada_districts$district))]
moramanga$distcode <- moramanga_dist_matches$distcode[match(tolower(moramanga$district), 
                                                   moramanga_dist_matches$names_tomatch)]
moramanga_comm_matches <- match.admin(data_names = moramanga$commune, 
                                      data_nest = moramanga$distcode,
                                      match_names = mada_communes$commune, 
                                      match_nest = mada_communes$distcode, 
                                      match_method = "osa", nested = TRUE)
moramanga_comm_matches <- filter(moramanga_comm_matches, !is.na(names_tomatch))
write.csv(moramanga_comm_matches, "data/raw/match_names/moramanga_comm_matches.csv", 
          row.names = FALSE)

# Saving session info
out.session(path = "R/02_bitedata/01_match_names.R", filename = "output/log_local.csv")


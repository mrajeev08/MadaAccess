# ------------------------------------------------------------------------------
#' Cleaning district and commune names
#' Details: Using some manual and some fuzzy  matching for administrative units for the three
#' data sets
# ------------------------------------------------------------------------------

start <- Sys.time()

# Set-up -----------------------------------------------------------------------
library(stringdist)
library(data.table)
library(sf)
library(dplyr)
library(here)

# source matching function
source(here("R", "utils.R"))
source(here_safe("R/match_names.R"))

# Read in shapefiles
mada_communes <- st_read(here_safe("data-raw/out/shapefiles/mada_communes.shp"))
mada_districts <- st_read(here_safe("data-raw/out/shapefiles/mada_districts.shp"))

# Match commune names in peripheral ARMC data ----------------------------------
# automatch ones with < 3 fixed distance
peripheral <- read.csv(here_safe("data-raw/raw/ipm_data/SaisieRage_DATA_2018-09-21_1755.csv"))
peripheral$distcode <- paste0(
  substring(peripheral$district, 1, 1),
  substring(peripheral$district, 3, 8)
)
peripheral_comm_matches <- match_admin(
  data_names = peripheral$commune,
  data_nest = peripheral$distcode,
  match_names = mada_communes$commune,
  match_nest = mada_communes$distcode,
  match_method = "osa", nested = TRUE
)
write_create(peripheral_comm_matches,
  here_safe("data-raw/out/match_names/peripheral_comm_matches.csv"),
  write.csv,
  row.names = FALSE
)

# Match admin names in IPM data ------------------------------------------------
load("data-raw/raw/ipm_data/ipm.rda")
ipm_dist_matches <- match_admin(
  data_names = IPM$fiv, data_nest = NULL,
  match_names = mada_districts$district, match_nest = NULL,
  match_method = "osa", nested = FALSE
)
write_create(ipm_dist_matches, here_safe("data-raw/out/match_names/ipm_dist_matches.csv"),
  write.csv,
  row.names = FALSE
)

# after manual matching of names in IPM data
names_matched <- read.csv(here_safe("data-raw/misc/matched/ipm_dist_matched.csv"))
names_matched$distcode <- mada_districts$distcode[match(
  names_matched$match,
  tolower(mada_districts$district)
)]
IPM$distcode <- names_matched$distcode[match(tolower(IPM$fiv), names_matched$names_tomatch)]
ipm_comm_matches <- match_admin(
  data_names = IPM$fir, data_nest = IPM$distcode,
  match_names = mada_communes$commune,
  match_nest = mada_communes$distcode,
  match_method = "osa", nested = TRUE
)
write_create(ipm_comm_matches, here_safe("data-raw/out/match_names/ipm_comm_matches.csv"),
  write.csv,
  row.names = FALSE
)

# Match admin names in moramanga data ------------------------------------------
moramanga <- read.csv(here_safe("data-raw/raw/moramanga/CTAR_%28V3%29_20190918150219.csv"))
moramanga$commune <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\("), "[", 1)
moramanga$commune <- trimws(moramanga$commune, which = "right")
moramanga$district <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\, "), "[", 2)
moramanga$district <- gsub(" \\(District\\)", "", moramanga$district)

moramanga_dist_matches <- match_admin(
  data_names = moramanga$district, data_nest = NULL,
  match_names = mada_districts$district, match_nest = NULL,
  match_method = "osa", nested = FALSE
)
write_create(moramanga_dist_matches, here_safe("data-raw/out/match_names/moramanga_dist_matches.csv"),
  write.csv,
  row.names = FALSE
)

# Using these I manually matched and checked the district names from moramanga
# Then automatically match communes
moramanga_dist_matches <- read.csv("data-raw/misc/matched/moramanga_dist_matched.csv")
moramanga_dist_matches$distcode <- mada_districts$distcode[match(
  moramanga_dist_matches$match,
  tolower(mada_districts$district)
)]
moramanga$distcode <- moramanga_dist_matches$distcode[match(
  tolower(moramanga$district),
  moramanga_dist_matches$names_tomatch
)]
moramanga_comm_matches <- match_admin(
  data_names = moramanga$commune,
  data_nest = moramanga$distcode,
  match_names = mada_communes$commune,
  match_nest = mada_communes$distcode,
  match_method = "osa", nested = TRUE
)
moramanga_comm_matches <- filter(moramanga_comm_matches, !is.na(names_tomatch))
write_create(moramanga_comm_matches, here_safe("data-raw/out/match_names/moramanga_comm_matches.csv"),
  write.csv,
  row.names = FALSE
)

# Saving session info
out_session(logfile = here_safe("data-raw/log.csv"), start = start, ncores = 1)

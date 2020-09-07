# ------------------------------------------------------------------------------------------------ #
#' Generating master data sets
#' Taking raw data from 1) Peripheral clinics, 2) IPM clinic, and 3) Moramanga clinic and
#' combining into cleaned Moramanga and National datasets (output into ~data/processed/)  
# ------------------------------------------------------------------------------------------------ #

# Set-up --------------------------------------------------------------------------------------
library(stringdist)
library(sf)
library(dplyr)
library(lubridate)
library(here)
source(here("R", "utils.R"))
source(safe_path("R/out.session.R"))
select <- dplyr::select 

# Read in shapefiles
mada_communes <- st_read(safe_path("data-raw/out/shapefiles/mada_communes.shp"))
mada_districts <- st_read(safe_path("data-raw/out/shapefiles/mada_districts.shp"))

# Read in raw bite data
peripheral <- read.csv(safe_path("data-raw/raw/ipm_data/SaisieRage_DATA_2018-09-21_1755.csv"))
load(safe_path("data-raw/raw/ipm_data/ipm.rda"))
moramanga <- read.csv(safe_path("data-raw/raw/moramanga/CTAR_%28V3%29_20190918150219.csv"))

# Match admin names --------------------------------------------------------------------------------
# Peripheral data to the district
peripheral$distcode <- paste0(substring(peripheral$district, 1, 1), 
                              substring(peripheral$district, 3, 8))

# automatically fuzzy match communes using best fixed matches with minimum matching distance <= 2
peripheral_comm_matches <- read.csv(safe_path("data-raw/out/match_names/peripheral_comm_matches.csv"))
mada_communes$matchcode <- interaction(mada_communes$distcode, tolower(mada_communes$commune))
peripheral_comm_matches %>%
  mutate(match = ifelse(min_fixed <= 2, as.character(fixed_best), NA),
         matchcode_comm = as.character(interaction(nest, match)), 
         matchcode_data = as.character(interaction(nest, names_tomatch)),
         commcode = mada_communes$commcode[match(matchcode_comm, 
                                                   mada_communes$matchcode)]) %>%
  filter(!is.na(matchcode_data)) %>%
  select(names_tomatch, min_fixed, match, matchcode_comm, 
         matchcode_data, commcode) -> peripheral_comm_matches
peripheral %>%
  mutate(matchcode_data = as.character(interaction(distcode, tolower(commune)))) %>%
  left_join(peripheral_comm_matches) -> peripheral

# IPM data to the district
names_matched <- read.csv(safe_path("data-raw/misc/matched_names/ipm_dist_matched.csv"))
names_matched$distcode <- mada_districts$distcode[match(names_matched$match, 
                                                        tolower(mada_districts$district))]
IPM$distcode <- names_matched$distcode[match(tolower(IPM$fiv), names_matched$names_tomatch)]

# automatically do communes that were matched to within 4 differences
ipm_comm_matches <- read.csv(safe_path("data-raw/out/match_names/ipm_comm_matches.csv"))
ipm_comm_matches %>%
  mutate(match = ifelse(min_fixed <= 2, as.character(fixed_best), NA),
         matchcode_comm = interaction(nest, match), 
         matchcode_data = interaction(nest, names_tomatch),
         commcode = mada_communes$commcode[match(matchcode_comm, 
                                                   mada_communes$matchcode)]) %>%
  filter(!is.na(matchcode_data)) %>%
  select(names_tomatch, min_fixed, match, matchcode_comm, matchcode_data, commcode) -> ipm_comm_matches

IPM %>%
  mutate(matchcode_data = interaction(distcode, tolower(fir))) %>%
  left_join(ipm_comm_matches) -> IPM

# Moramanga data match to the district
moramanga$district <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\, "), "[", 2)
moramanga$district <- gsub(" \\(District\\)", "", moramanga$district)
moramanga_dist_matches <- read.csv("data/processed/matched_names/moramanga_dist_matched.csv")
moramanga_dist_matches$distcode <- mada_districts$distcode[match(moramanga_dist_matches$match, 
                                                                 tolower(mada_districts$district))]
moramanga$distcode <- moramanga_dist_matches$distcode[match(tolower(moramanga$district), 
                                                            moramanga_dist_matches$names_tomatch)]

# Moramanga data matched to the commune
moramanga$commune <- sapply(strsplit(as.character(moramanga$Patient.Home), "\\("), "[", 1)
moramanga$commune <- trimws(moramanga$commune, which = "right")
moramanga_comm_matches <- read.csv("data-raw/out/match_names/moramanga_comm_matches.csv")

moramanga_comm_matches %>%
  mutate(match = ifelse(min_fixed <= 2, as.character(fixed_best), NA),
         matchcode_comm = interaction(nest, match), 
         matchcode_data = interaction(nest, names_tomatch),
         commcode = mada_communes$commcode[match(matchcode_comm, 
                                                   mada_communes$matchcode)]) %>%
  filter(!is.na(matchcode_data)) %>%
  select(min_fixed, match, matchcode_comm, matchcode_data, commcode) -> moramanga_comm_matches

moramanga %>%
  mutate(matchcode_data = interaction(distcode, tolower(commune))) %>%
  left_join(moramanga_comm_matches) -> moramanga

# Format other columns  ---------------------------------------------------------------------------
# Date reported, CTAR, type, commcode, distcode, notes
# + ttimes & transport type for mora data

# Peripheral
peripheral %>%
  mutate(type = "new", date_reported = ymd(date_de_consultation), 
         source = "peripheral") %>%
  select(date_reported, type, ctar, id_ctar, distcode, commcode, 
         source) -> peripheral_clean

# IPM
IPM %>%
  mutate(type = case_when(categorie == "N" ~ "new", 
                          categorie == "R" ~ "restart", 
                          categorie == "T" ~ "transfer"), 
         date_reported = ymd(dat_consu), 
         ctar = "IPM", id_ctar = 5, source = "IPM") %>%
  select(date_reported, type, ctar, id_ctar, distcode, commcode, source) -> IPM_clean
  
# Moramanga bite data
moramanga %>%
  mutate(type = if_else(Type.of.consultation == "Passage", "transfer",
                        "new"), 
         date_reported = dmy(Date.of.consultation), 
         known_cat1 = ifelse(Type.of.consultation == "Contact with suspect case", 
                           1, 0), 
         ctar = "Moramanga", id_ctar = 8, source = "Moramanga") %>% 
  select(date_reported, type, ctar, id_ctar, distcode, commcode, known_cat1, 
         source) -> moramanga_clean

# Output master data and summary stats -------------------------------------------------------------
national <- bind_rows(IPM_clean, peripheral_clean)
write.csv(national, safe_path("data-raw/out/bitedata/national.csv"), row.names = FALSE)
write.csv(moramanga_clean, safe_path("data-raw/out/bitedata/moramanga.csv"), row.names = FALSE)

# Also write out travel time data from Mora --------------------------------------------------------
moramanga %>%
  select(contains("transport"), contains("hours")) %>%
  setNames(lapply(strsplit(names(.), ".", fixed = TRUE), function(x) x[length(x)])) %>%
  rename(hours = CTAR) %>%
  bind_cols(select(moramanga_clean, known_cat1, commcode, distcode)) -> moramanga_ttimes
moramanga_ttimes[moramanga_ttimes == TRUE] <- 1
moramanga_ttimes[moramanga_ttimes == FALSE] <- 0
write.csv(moramanga_ttimes, safe_path("data-raw/out/bitedata/moramanga_ttimes.csv"), 
          row.names = FALSE)

# Saving session info
out.session(path = "data-raw/src/05_bitedata.R", filename = safe_path("analysis/logs/log_local.csv"))


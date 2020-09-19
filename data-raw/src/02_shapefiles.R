# ------------------------------------------------------------------------------------------------ #
#' Cleaning up shapefiles & keeping only columns needed
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Set-up ------------------------------------------------------------
library(sf)
library(data.table)
library(dplyr)
library(here)

# Source
source(here("R", "utils.R"))
source(here_safe("R/ttime_functions.R"))

# Load in GIS files
mada_districts <- st_read(here_safe("data-raw/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp"))
mada_communes <- st_read(here_safe("data-raw/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp"))
ctar_metadata <- read.csv(here_safe("data-raw/raw/ctar_metadata.csv"))

# Fix up shapefiles ------------------------------------------------------
# Get distcodes for both admin levels & dissolve tana polygons
mada_districts %>%
  mutate(distcode = substring(as.character(ADM2_PCODE), 1, 7)) -> mada_districts

mada_districts %>%
  group_by(distcode) %>%
  summarize(geometry = st_union(geometry)) %>%
  left_join(st_drop_geometry(mada_districts[6:nrow(mada_districts), ])) %>%
  mutate(ADM2_EN = recode(ADM2_EN,
    `6e Arrondissement` = "Antananarivo Renivohitra"
  )) %>%
  st_collection_extract(.) -> mada_districts

mada_communes %>%
  mutate(
    distcode = substring(as.character(ADM2_PCODE), 1, 7),
    ADM2_EN = case_when(
      distcode == "MG11101" ~ "Antananarivo Renivohitra",
      TRUE ~ ADM2_EN
    )
  ) -> mada_communes


# Clean up names ----------------------------------------------------------
# NOTE: var names have to be <= 10 characters long for ESRI shapefile output
mada_districts %>%
  dplyr::select(distcode, district = ADM2_EN) %>%
  mutate(as_tibble(st_coordinates(st_centroid(.))), .before = "geometry") %>%
  rename(long_cent = X, lat_cent = Y) -> mada_districts

mada_communes %>%
  dplyr::select(distcode, district = ADM2_EN, commcode = ADM3_PCODE, commune = ADM3_EN) %>%
  mutate(as_tibble(st_coordinates(st_centroid(.))), .before = "geometry") %>%
  rename(long_cent = X, lat_cent = Y) -> mada_communes

write_create(mada_districts, here_safe("data-raw/out/shapefiles/mada_districts.shp"),
  st_write,
  delete_layer = TRUE
)
write_create(mada_communes, here_safe("data-raw/out/shapefiles/mada_communes.shp"),
  st_write,
  delete_layer = TRUE
)

# Saving session info
out_session(logfile = "logs/data_raw.csv", start = start, ncores = 1)

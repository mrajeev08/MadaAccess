# ------------------------------------------------------------------------------------------------ #
#' Cleaning up shapefiles & keeping only columns needed
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Set-up ------------------------------------------------------------
library(sf)
library(raster)
library(gdistance)
library(foreach)
library(doParallel)
library(iterators)
library(data.table)
library(dplyr)
library(lubridate)
library(rmapshaper)
library(fasterize)
library(here)

# Source
source(here("R", "ttime_functions.R"))
source(here("R", "out.session.R"))

# Load in GIS files
mada_districts <- st_read(here("data-raw", "raw", "shapefiles", "districts", 
                               "mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp"))
mada_communes <- st_read(here("data-raw", "raw", "shapefiles", "communes", 
                              "mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp"))
ctar_metadata <- read.csv(here("data-raw", "raw", "ctar_metadata.csv"))

# Fix up shapefiles ------------------------------------------------------
# Get distcodes for both admin levels & dissolve tana polygons
mada_districts %>%
  mutate(distcode = substring(as.character(ADM2_PCODE), 1, 7)) -> mada_districts

mada_districts %>%
  group_by(distcode) %>%
  summarize(geometry = st_union(geometry)) %>%
  left_join(st_drop_geometry(mada_districts[6:nrow(mada_districts), ])) %>%
  mutate(ADM2_EN = recode(ADM2_EN, 
                          `6e Arrondissement` = "Antananarivo Renivohitra")) -> mada_districts
  
mada_communes %>%
  mutate(distcode = substring(as.character(ADM2_PCODE), 1, 7),
         ADM2_EN = case_when(distcode == "MG11101" ~ "Antananarivo Renivohitra",
                             TRUE ~ ADM2_EN)) -> mada_communes


# Clean up names ----------------------------------------------------------
# NOTE: var names have to be <= 10 characters long for ESRI shapefile output
mada_districts %>%
  dplyr::select(distcode, district = ADM2_EN, pop = pop_admin, ttimes_wtd, 
                ttimes_un, catchment, id_ctar, pop_catch = prop_pop_catch) %>%
  mutate(long_cent = st_coordinates(st_centroid(.))[, 1], 
         lat_cent = st_coordinates(st_centroid(.))[, 2]) -> mada_districts

mada_communes %>%
  dplyr::select(distcode, district = ADM2_EN, commcode = ADM3_PCODE, commune = ADM3_EN) %>%
  mutate(long_cent = st_coordinates(st_centroid(.))[, 1], 
         lat_cent = st_coordinates(st_centroid(.))[, 2]) -> mada_communes

st_write(mada_districts, here("data-raw", "out", "shapefiles", "mada_districts.shp"), 
         delete_layer = TRUE)
st_write(mada_communes, here("data-raw", "out", "shapefiles", "mada_communes.shp"), 
         delete_layer = TRUE)

# Saving session info
out.session(path = "data-raw/src/04_run_baseline.R", 
            filename = here("analysis", "logs", "log_local.csv"), 
            start = start)

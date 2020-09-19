# ------------------------------------------------------------------------------
#' Getting district/commune bite incidence
#' Agreggating data by admin unit and by year to get average annual incidence for
#' National and Moramanga data
# ------------------------------------------------------------------------------

start <- Sys.time()

# Libraries
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
select <- dplyr::select

# Source scripts
source(here::here("R", "utils.R"))
source(here_safe("R/data_functions.R"))

# Read in data
national <- fread(here_safe("data-raw/out/bitedata/national.csv"))
moramanga <- fread(here_safe("data-raw/out/bitedata/moramanga.csv"))
ctar_metadata <- fread(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes.shp"))
mada_districts <- st_read(here_safe("analysis/out/shapefiles/mada_districts.shp"))

# Prep shapefiles
ctar_metadata <- ctar_to_exclude(national, ctar_metadata, min_forms = 10)
mada_districts$exclude_dist <- ctar_metadata$exclude_dist[match(mada_districts$catchment,
                                                                ctar_metadata$CTAR)]
mada_communes %>%
  left_join(select(st_drop_geometry(mada_districts), distcode, exclude_dist,
                   ctch_dist = catchment)) -> mada_communes


# National data: district bites & commcovars
natl <- clean_natl(national, mada_districts, mada_communes, ctar_metadata,
                       reporting_thresh = 0.25,
                       tput_thresh = 15)

# Moramanga data
mora_bites <- clean_mora(moramanga, mada_communes, mada_districts, natl$district_bites)

# Write out bite data and covariate data
write_create(natl$district_bites, "analysis/out/bites/district_bites.csv", fwrite)
write_create(natl$comm_covars, "analysis/out/bites/comm_covars.csv", fwrite)
write_create(mora_bites, "analysis/out/bites/mora_bites.csv", fwrite)

# Save session info
out_session(logfile = "logs/log_local.csv", start = start, ncores = 1)

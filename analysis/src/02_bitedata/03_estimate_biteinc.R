# ------------------------------------------------------------------------------------------------ #
#' Getting district/commune bite incidence 
#' Agreggating data by admin unit and by year to get average annual incidence for National
#' and Moramanga data 
# ------------------------------------------------------------------------------------------------ #

# Libraries
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
select <- dplyr::select

# Source scripts
source("R/functions/out.session.R")
source("R/functions/data_functions.R")

# Read in data
national <- fread("data/processed/bitedata/national.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/processed/clinics/ctar_metadata.csv")
mada_communes <- st_read("data/processed/shapefiles/mada_communes.shp")
mada_districts <- st_read("data/processed/shapefiles/mada_districts.shp")

# Get stats on missingness (output if possible!) (# use steward to document in data)
# National < 0.5% (0.28%)
nrow(national[is.na(year(date_reported))]) # 1
nrow(national[is.na(distcode)]) # 95
nrow(national[is.na(id_ctar)]) # 176

# Moramanga < 2%
nrow(moramanga[is.na(year(date_reported))]) # 0
nrow(moramanga[is.na(distcode)]) # 23 and these are passage!
nrow(moramanga[is.na(id_ctar)]) # 0

# But commune matching went poorly for national data
nrow(national[is.na(commcode)]) # 54% unmatched (40% of peripheral unmatched and 60% of IPM unmatched)
nrow(moramanga[is.na(commcode)]) # 23 and these are passage! (< 2%)

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
fwrite(natl$district_bites, "output/bites/district_bites.csv")
fwrite(natl$comm_covars, "output/bites/comm_covars.csv")
fwrite(mora_bites, "output/bites/mora_bites.csv")

# Save session info
out.session(path = "R/02_bitedata/03_estimate_biteinc.R", filename = "output/log_local.csv")

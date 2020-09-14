# ------------------------------------------------------------------------------------------------ #
#' Documenting data
#' Using package steward for metadata & writing roxygen -> data.R
#' usethis::use_this to save as rda to data for lazy loading
# ------------------------------------------------------------------------------------------------ #

# TO DO: Add documentation for ipm ttime data & trans_gc!
# Setup
library(steward)
library(usethis)
library(glue)
library(stringr)

# For some of the metadata
library(raster)
library(sf)
library(data.table)
library(fasterize)

# helper function to spit out data into console
dict <- function(data) {
  usethis::ui_code_block(paste(paste(names(data), "= ,"), sep = "\n"))
}

# bitedata -------------------------------------------------------------------------------------

# moramanga ----
moramanga <- read.csv("data-raw/out/bitedata/moramanga.csv")
mora_missing <- lapply(moramanga, function(x) round(sum(is.na(x) / nrow(moramanga)) * 100, 2))

stw_dataset(moramanga) %>%
  stw_mutate_meta(
    name = "moramanga",
    title = "Moramanga ARMC patient data",
    description = glue("Data on date of first report, home district (and commune when available) of patient, and ctar reported to from Moramanga ARMC {paste(year(range(moramanga$date_reported)), collapse = ' - ')}, serving the Moramanga District and surrounding catchments"),
    sources = list(
      list(
        title = "Data from previously published study, Rajeev et al. 2019 and continued data collection using the same methods",
        path = "https://www.sciencedirect.com/science/article/pii/S0264410X18315202"
      )
    )
  ) %>%
  stw_mutate_dict(
    date_reported = glue("Date patient reported, {mora_missing$date_reported}% NA"),
    ctar = glue(
      "Name of Anti-rabies medical center (ARMC) where patient reported to, ",
      "{mora_missing$ctar}% NA"
    ),
    type = glue(
      "type of patient consultation: {paste(levels(factor(moramanga$type)),
                 collapse = ', ')}; transfer is when a patient reported to another ARMC first; ",
      "{mora_missing$type}% NA"
    ),
    id_ctar = glue(
      "Numeric id of ARMC corresponding to column in `ctar_metadata`, ",
      "{mora_missing$id_ctar}% NA"
    ),
    distcode = glue(
      "District id corresponding to `distcode` column in `mada_districts` shapefile, ",
      "{mora_missing$distcode}% NA"
    ),
    commcode = glue(
      "Commune id corresponding to `commcode` column in `mada_communes` shapefile, ",
      "{mora_missing$commcode}% NA"
    ),
    known_cat1 = glue(
      "Whether patient reporting was seeking PEP for a Category I exposure by WHO classification, 1 indicates they yes",
      "{mora_missing$known_cat1}% NA"
    ),
    source = "The source of the data, the Moramanga Rabies Project"
  ) %>%
  stw_to_roxygen() -> mora_doc

usethis::use_data(moramanga, overwrite = TRUE, compress = "xz")

# national ----
national <- read.csv("data-raw/out/bitedata/national.csv")
natl_missing <- lapply(national, function(x) round(sum(is.na(x) / nrow(national)) * 100, 2))

stw_dataset(national) %>%
  stw_mutate_meta(
    name = "national",
    title = "National ARMC patient data",
    description = glue(
      "Data on date of first report, home district (and commune when available) of ",
      "patient, and ctar reported to from {length(unique(national$id_ctar))} ARMC",
      "between {paste(year(range(national$date_reported, na.rm = TRUE)), collapse = ' - ')}, ",
      "serving multiple districts in Madagascar. Access to the anonymized databases are ",
      "available through data sharing agreements with IPM. These raw databases are **not** available publicly."
    ),
    sources = list(
      list(
        title = "Data cleaned from RedCap databases maintained by IPM and the Ministry of Health which hold bite patient data from peripheral ARMC (outside of Antananarivo) and IPM (within Antananarivo) ARMC"
      )
    )
  ) %>%
  stw_mutate_dict(
    date_reported = glue("Date patient reported, {natl_missing$date_reported}% NA"),
    ctar = glue(
      "Name of Anti-rabies medical center (ARMC) where patient reported to, ",
      "{natl_missing$ctar}% NA"
    ),
    type = glue(
      "type of patient consultation: {paste(levels(factor(moramanga$type)),
                 collapse = ', ')}; transfer is when a patient reported to another ARMC first; ",
      "{natl_missing$type}% NA"
    ),
    id_ctar = glue(
      "Numeric id of ARMC corresponding to column in `ctar_metadata`, ",
      "{natl_missing$id_ctar}% NA"
    ),
    distcode = glue(
      "District id corresponding to `distcode` column in `mada_districts` shapefile, ",
      "{natl_missing$distcode}% NA"
    ),
    commcode = glue(
      "Commune id corresponding to `commcode` column in `mada_communes` shapefile, ",
      "{natl_missing$commcode}% NA.", " Note that these were fuzzy matched and are largely incomplete and may be incorrect."
    ),
    source = "The source of the data, either the peripheral RedCap database, which holds data for all ARMC excluding the IPM ARMC, or the IPM database"
  ) %>%
  stw_to_roxygen() -> national_doc

usethis::use_data(national, overwrite = TRUE, compress = "xz")

# moramanga_ttimes ----
mora_ttimes <- read.csv("data-raw/out/bitedata/moramanga_ttimes.csv")

stw_dataset(mora_ttimes) %>%
  stw_mutate_meta(
    name = "mora_ttimes",
    title = "Patient reported travel times from Moramanga ARMC",
    description = glue("Data on self reported travel times in hours for a subset of patients reporting to the Moramanga ARMC between {paste(year(range(moramanga$date_reported, na.rm = TRUE)), collapse = ' - ')}. and what mode of transport they used (mutliple options per patient possible)."),
    sources = list(
      list(
        title = "Data from previously published study, Rajeev et al. 2019 and continued data collection using the same methods",
        path = "https://www.sciencedirect.com/science/article/pii/S0264410X18315202"
      )
    )
  ) %>%
  stw_mutate_dict(
    car = "patient traveled by car? (1 indicates yes)",
    Motorbike = "patient traveled by motorbike? (1 indicates yes)",
    Ambulance = "patient traveled by ambulance? (1 indicates yes)",
    foot = "patient traveled by foot? (1 indicates yes)",
    Bus = "patient traveled by bus? (1 indicates yes)",
    Bicycle = "patient traveled by bicycle? (1 indicates yes)",
    Pus = "patient traveled by bicycle taxi (pousse-pousse)? (1 indicates yes)",
    Other = "patient traveled by other means of transport? (1 indicates yes)",
    hours = "Travel time from patient home to clinic in hours",
    known_cat1 = "Whether the patient was reporting for a Category I exposure by WHO classification",
    distcode = "District id corresponding to `distcode` column in `mada_districts` shapefile",
    commcode = "Commune id corresponding to `commcode` column in `mada_communes` shapefile"
  ) %>%
  stw_to_roxygen() -> mora_ttimes_doc

usethis::use_data(mora_ttimes, overwrite = TRUE, compress = "xz")

# clinics -------------------------------------------------------------------------------------

# ctar_metadata ----
ctar_metadata <- read.csv("data-raw/out/clinics/ctar_metadata.csv")
ctar_metadata %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "ctar_metadata",
    title = "Metadata on ARMC in Madagascar",
    description = "This data includes information on the location and doses provisioned to ARMC across Madagascar",
    sources = list(
      list(
        title = "IPM Vaccine Clinic"
      )
    )
  ) %>%
  stw_mutate_dict(
    CTAR = "Name of ARMC",
    District = "District that ARMC is located in ",
    id_ctar = "The numeric id for the ARMC used in the bite patient datasets",
    year_opened = "The year that PEP provisioning began at the ARMC",
    doses_2010 = "Number of vials provisioned in 2010",
    doses_2011 = "Number of vials provisioned in 2011",
    doses_2012 = "Number of vials provisioned in 2012",
    doses_2013 = "Number of vials provisioned in 2013",
    doses_2014 = "Number of vials provisioned in 2014",
    doses_2015 = "Number of vials provisioned in 2015",
    doses_2016 = "Number of vials provisioned in 2016",
    doses_2017 = "Number of vials provisioned in 2017",
    color = "The color used in figures for each ARMC",
    notes = "Miscellaneous notes, two of the coastal ARMC locations were offset to ",
    commcode = "Commune id corresponding to `commcode` column in `mada_communes` shapefile",
    distcode = "District id corresponding to `distcode` column in `mada_districts` shapefile",
    district = "District name corresponding to `district` column in `mada_districts` shapefile",
    commune = "Communes name corresponding to `commcode` column in `mada_communes` shapefile",
    clinic_id = "Clinic id used in analyis of expanding PEP access",
  ) %>%
  stw_to_roxygen() -> ctar_metadata_doc

usethis::use_data(ctar_metadata, overwrite = TRUE, compress = "xz")

# csb2 ----
csb2 <- read.csv("data-raw/out/clinics/csb2.csv")

csb2 %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "csb2",
    title = "CSB II locations in Madagascar",
    description = "Location data of Centre de Sante Niveau II (CSB II), a subset of public clinics and hospitals, in Madagascar. CSB II are larger and generally have more staff and services than CSB I (also public).",
    sources = list(
      list(
        title = "Full data on all geolocated clinics available upon request from the IPM GIS unit; raw data are *not* shared here."
      )
    )
  ) %>%
  stw_mutate_dict(
    district = "Name of district where CSB II is located",
    distcode = "District id corresponding to distcode column in `mada_districts` shapefile",
    commune = "Commune id corresponding to commcode column in `mada_communes` shapefile",
    commcode = "Name of commune where CSB II is located",
    long = "Longitude",
    lat = "Latitude",
    clinic_id = "clinic id used in analysis of expanding PEP",
    pop_dens = "Density of people in grid cell where clinic is located (estimated from `pop1x1`)",
    type = "Type of clinic, all 'CSB II'"
  ) %>%
  stw_to_roxygen() -> csb2_doc

usethis::use_data(csb2, overwrite = TRUE, compress = "xz")

# clinic_per_comm ----
clin_per_comm <- read.csv("data-raw/out/clinics/clinic_per_comm.csv")

clin_per_comm %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "clin_per_comm",
    title = "CSB I per II in each commune",
    description = glue("Locations of CSB (either level 1 or 2) in highest density area for each commune (n = {nrow(clin_per_comm)})"),
    sources = list(
      list(
        title = "Full data on all geolocated clinics available upon request from the IPM GIS unit; raw data are *not* shared here"
      )
    )
  ) %>%
  stw_mutate_dict(
    distcode = "District id corresponding to `distcode` column in `mada_districts` shapefile",
    district = "District name corresponding to `district` column in `mada_districts` shapefile",
    commcode = "Commune id corresponding to `commcode` column in `mada_communes` shapefile",
    commune = "Communes name corresponding to `commcode` column in `mada_communes` shapefile",
    long = "Longitude",
    lat = "Latitude",
    clinic_id = "clinic id used in analysis of expanding PEP",
    pop_dens = "Density of people in grid cell where clinic is located (estimated from `pop1x1`)",
    type = glue(
      "Type of clinic, ",
      glue_collapse(paste(
        names(table(clin_per_comm$type)), "=",
        table(clin_per_comm$type)
      ), sep = ", ")
    )
  ) %>%
  stw_to_roxygen() -> clin_per_comm_doc

usethis::use_data(clin_per_comm, overwrite = TRUE, compress = "xz")

# clinic_per_dist ----
clin_per_dist <- read.csv("data-raw/out/clinics/clinic_per_dist.csv")

clin_per_dist %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "clin_per_dist",
    title = "CSB II per district",
    description = glue("Locations of CSB II in location with highest population density for each district (n = {nrow(clin_per_dist)})"),
    sources = list(
      list(
        title = "Full data on all geolocated clinics available upon request from the IPM GIS unit; raw data are *not* shared here"
      )
    )
  ) %>%
  stw_mutate_dict(
    distcode = "District id corresponding to `distcode` column in `mada_districts` shapefile",
    district = "District name corresponding to `district` column in `mada_districts` shapefile",
    commcode = "Commune id corresponding to `commcode` column in `mada_communes` shapefile",
    commune = "Communes name corresponding to `commcode` column in `mada_communes` shapefile",
    long = "Longitude",
    lat = "Latitude",
    clinic_id = "clinic id used in analysis of expanding PEP",
    pop_dens = "Density of people in grid cell where clinic is located (estimated from `pop1x1",
    type = "Type of clinic, all CSB II (CSB2)",
  ) %>%
  stw_to_roxygen() -> clin_per_dist_doc

usethis::use_data(clin_per_dist, overwrite = TRUE, compress = "xz")

# shapefiles ----------------------------------------------------------------------------------

# mada_districts ----
mada_districts <- st_read("data-raw/out/shapefiles/mada_districts.shp")

mada_districts %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "mada_districts",
    title = "Administrative level 2 boundaries (District) for Madagascar",
    description = "sf object cleaned from original OCHA shapefile, retreived from HDX on 2018-10-31. The district polygons for Antananarivo were dissolved to a single polygon to correspond to administrative boundaries at time period of data.",
    sources = list(
      list(
        title = "United Nations Office for the Coordination of Humanitarian Affairs (OCHA) Regional Office for Southern Africa",
        path = "https://data.world/ocha-rosa/26fa506b-0727-4d9d-a590-d2abee21ee22"
      )
    )
  ) %>%
  stw_mutate_dict(
    distcode = "District id",
    district = "District name ",
    long_cent = "Longitude of polygon centroid (approximate)",
    lat_cent = "Latitude of polygon centroid (approximate)",
    geometry = "sf attribute storing spatial coordinate data",
  ) %>%
  stw_to_roxygen() -> mada_districts_doc

# correct the sf format
mada_districts_doc <- gsub(
  "format A data frame", "format A simple feature object (from package `sf`) with data",
  mada_districts_doc
)
usethis::use_data(mada_districts, overwrite = TRUE, compress = "xz")

# mada_communes ----
mada_communes <- st_read("data-raw/out/shapefiles/mada_communes.shp")

mada_communes %>%
  stw_dataset() %>%
  stw_mutate_meta(
    name = "mada_communes",
    title = "Administrative level 3 boundaries (commune) for Madagascar",
    description = "sf object cleaned from original OCHA shapefile from 2018-10-31.",
    sources = list(
      list(
        title = "United Nations Office for the Coordination of Humanitarian Affairs (OCHA) Regional Office for Southern Africa",
        path = "https://data.world/ocha-rosa/26fa506b-0727-4d9d-a590-d2abee21ee22"
      )
    )
  ) %>%
  stw_mutate_dict(
    distcode = "District id",
    district = "District name ",
    commcode = "commune id",
    commune = "commune name ",
    long_cent = "Longitude of polygon centroid (approximate)",
    lat_cent = "Latitude of polygon centroid (approximate)",
    geometry = "sf attribute storing spatial coordinate data",
  ) %>%
  stw_to_roxygen() -> mada_communes_doc

# correct the sf format
mada_communes_doc <- gsub(
  "format A data frame", "format A simple feature object (from package `sf`) with data",
  mada_communes_doc
)

usethis::use_data(mada_communes, overwrite = TRUE, compress = "xz")

# rasters -------------------------------------------------------------------------------------
# using raw roxygen strings as these are not dataframes

# and borrowing this helper function from steward ----
roxygen_substitute <- function(x) {

  # replace single `@` with `@@`
  x <- stringr::str_replace_all(x, "(?<!@|#'\\s{0,10})@(?!@)", "@@")

  # replace `%` with `\%`
  x <- stringr::str_replace_all(x, "(?<!\\\\)%", "\\\\%")

  x
}

# friction surface ----
friction_masked <- raster("data-raw/out/rasters/friction_mada_masked.tif")
friction_doc <-
  glue::glue(
    "#' Friction surface masked to Madagascar",
    "#' ",
    "#' Friction surface from Weiss et al. 2015 (linked in source), accessed through the malariaAtlas R package.",
    "#' ",
    "#' @format A {nrow(friction_masked)} x {ncol(friction_masked)} raster at a resolution of 30 arcsec (approximately 1x1 km @ equator)",
    "#' ",
    "#' @source [A global map of travel time to cities to assess inequalities in accessibility in 2015](https://www.nature.com/articles/nature25181/)",
    "#' @source [Link to download friction surface from Malaria Atlas Project (MAP)](https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2015_friction_surface_v1_Decompressed)",
    "#' ",
    "\"friction_masked\"",
    "",
    .sep = "\n"
  ) %>%
  roxygen_substitute()

usethis::use_data(friction_masked, overwrite = TRUE, compress = "xz")

# World Pop ----

# Data
pop1x1 <- raster("data-raw/out/rasters/wp_2015_1x1.tif")

glue::glue(
  "#' World Pop aggregated to friction surface ",
  "#' ",
  "#' World Pop 2015 dataset for Madagascar aggregated/resampled to the MAP friction surface. See `data-raw/src/01_rasters.R` for code. We used the older version of the dataset using Linaird et al. 2012 as it generated more comparable estimates to recent national and microcensus data.",
  "#' ",
  "#' @format A {nrow(pop1x1)} x {ncol(pop1x1)} raster at a resolution of 30 arcsec (approximately 1x1 km @ equator), the same raster as the friction surface",
  "#' ",
  "#' @source [Population Distribution, Settlement Patterns and Accessibility across Africa in 2010](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0031743)",
  "#' ",
  "\"pop1x1\"",
  "",
  .sep = "\n"
) %>%
  roxygen_substitute() -> pop1x1_doc

usethis::use_data(pop1x1, overwrite = TRUE, compress = "xz")

# Compile and write out all data documentation ------------------------------------------------
objs <- ls()
writeLines(sapply(objs[grep("doc", objs)], get), "R/data.R")

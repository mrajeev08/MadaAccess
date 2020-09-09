# ------------------------------------------------------------------------------------------------ #
#' Fetch raster files clipped to Madagascar for travel time analyses
#' Friction surface and create transition layer
#' Resample world pop to friction surface
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Packages
library(magrittr)
library(malariaAtlas) # for friction surface
library(raster) # for reading in rasters
require(rgdal) # for reading in shapefiles
library(gdistance) # for making transition object
library(here) # for paths
library(data.table) # for out session
source(here("R", "utils.R"))
source(safe_path("R/out.session.R"))
source(safe_path("R/match_pop.R"))

# Shapefile for masking to (from OCHA)
mada_districts <-
  safe_path("data-raw/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp") %>%
  readOGR()

# World Pop 2015 (Linnaird et al.)
wp_2015 <- raster(safe_path("data-raw/raw/WorldPop/MDG_ppp_2015_adj_v2.tif"))

# Masked friction surface
friction_masked <-
  getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_districts
  )

write_create(friction_masked,
  safe_path("data-raw/out/rasters/friction_mada_masked.tif"),
  writeRaster,
  overwrite = TRUE,
  options = c("COMPRESS=LZW")
)

# Masked transition surface (geocorrected  transition matrix (i.e., the graph))
trans <- transition(friction_masked, function(x) 1 / mean(x), 8)
trans_gc <- geoCorrection(trans)
write_create(trans_gc,
  safe_path("data-raw/out/rasters/trans_gc_masked.rds"),
  saveRDS,
  compress = "xz"
)


# convert frition surface & world pop to spatial pixels
friction_pix <- as(friction_masked, "SpatialPixelsDataFrame")
friction_pix$match_id <- friction_pix@grid.index
wp_2015_pix <- as(wp_2015, "SpatialPixelsDataFrame")

# get spatial intersections (minDimension = 0 ranks them)
wp_2015_pix$match_id <- over(wp_2015_pix, friction_pix, minDimension = 0)$match_id
match <- raster(wp_2015_pix["match_id"]) # transform back to raster

# For ones that are missing friction id find the nearest non NA cell id
wp_2015_unmatched <- wp_2015_pix[is.na(wp_2015_pix$match_id), ]
match <- match_nearest(
  unmatched_pix = wp_2015_unmatched, matched_raster = match,
  max_adjacent = 100
)

# Summarize pop to this updated raster
pop_dt <- data.table(pop = wp_2015[], friction_id = match[])
pop_dt <- pop_dt[, .(pop = sum(pop, na.rm = TRUE)), by = "friction_id"][!is.na(friction_id)]
friction_pix$pop <- pop_dt$pop[match(friction_pix$match_id, pop_dt$friction_id)]
pop <- raster(friction_pix["pop"]) # transform back to raster

# should be true (or minimal difference)
sum(pop[], na.rm = TRUE) - sum(wp_2015[], na.rm = TRUE)

write_create(pop, safe_path("data-raw/out/rasters/wp_2015_1x1.tif"),
  writeRaster,
  overwrite = TRUE, options = c("COMPRESS=LZW")
)

# Saving session info
out.session(
  path = "data-raw/src/01_rasters.R",
  filename = safe_path("analysis/logs/log_local.csv"),
  start
)

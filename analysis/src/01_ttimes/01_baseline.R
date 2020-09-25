# ------------------------------------------------------------------------------
#' Getting baseline travel time estimates and catchments @ grid cell level
#' Given the 31 existing clinics in the country
# ------------------------------------------------------------------------------

start <- Sys.time()

# Set-up -------------------------------------------------------------------
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
source(here::here("R", "utils.R"))
source(here_safe("R/ttime_functions.R"))

# Load in GIS files
mada_districts <- st_read(here_safe("data-raw/out/shapefiles/mada_districts.shp"))
mada_communes <- st_read(here_safe("data-raw/out/shapefiles/mada_communes.shp"))
ctar_metadata <- read.csv(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
friction_masked <- raster(here_safe("data-raw/out/rasters/friction_mada_masked.tif"))
pop1x1 <- raster(here_safe("data-raw/out/rasters/wp_2015_1x1.tif"))

# Get the minimum ttimes for each clinic ------------------------------------
prop_pop <- pop1x1 / sum(values(pop1x1), na.rm = TRUE)

# Get candidate points as matrix
# takes ~ 6 seconds per point
ctar_metadata %>%
 dplyr::select(x = long, y = lat, clinic_id) %>%
 as.matrix(.) -> point_mat_base

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

system.time({
  foreach(
    points = iter(point_mat_base, by = "row"),
    .packages = c("raster", "gdistance", "data.table")
  ) %dopar% {
    ttimes <- get_ttimes(
      friction = friction_masked, shapefile = mada_districts,
      coords = points[, c("x", "y")], trans_matrix_exists = TRUE,
      filename_trans = "data-raw/out/rasters/trans_gc_masked.rds"
    )
    write_create(ttimes,
                 here_safe(paste0("analysis/out/ttimes/candidates/clinic_",
                                  points[, "clinic_id"], ".tif")),
                 writeRaster,
                 overwrite = TRUE,
                 options = c("COMPRESS=LZW"))
    ttimes
  } -> stacked_ttimes
})

stopCluster(cl)

# Get minimum travel times
bricked <- brick(stacked_ttimes)
names(bricked) <- 1:31
ttimes_base <- min(bricked, na.rm = TRUE)
catchment <- which.min(bricked)

# write out the baseline raster
write_create(ttimes_base,
  here_safe("analysis/out/ttimes/base/ttimes.tif"),
  writeRaster,
  overwrite = TRUE,
  options = c("INTERLEAVE=BAND", "COMPRESS=LZW")
)

# Get vals by  districts/communes -------------------------------------------
mada_districts %>%
  mutate(id = 1:nrow(.)) %>%
  fasterize(., friction_masked, field = "id") -> district_id

mada_communes %>%
  mutate(id = 1:nrow(.)) %>%
  fasterize(., friction_masked, field = "id") -> commune_id

base_df <- data.table(
  distcode = mada_districts$distcode[district_id[]],
  commcode = mada_communes$commcode[commune_id[]],
  pop = pop1x1[], prop_pop = prop_pop[],
  ttimes = ttimes_base[], catchment = catchment[]
)

# Fix Nosy Komba so it has max ttimes in mainland Nosy Be & catchment of Nosy Be
base_df[commcode == "MG71718002"]$ttimes <- max(base_df[distcode == "MG71718"]$ttimes,
  na.rm = TRUE
)
base_df[commcode == "MG71718002"]$catchment <- base_df[distcode == "MG71718"]$catchment[1]
write_create(
  base_df,
  here_safe("analysis/out/ttimes/base/grid_df.gz"),
  fwrite
)

# District
district_df <- aggregate_admin(base_df = base_df, admin = "distcode", scenario = 0)
district_maxcatch <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch,
  na.rm = TRUE
)],
by = .(distcode, scenario)
]
write_create(
  district_df,
  here_safe("analysis/out/ttimes/base/district_allcatch.gz"),
  fwrite
)
write_create(
  district_maxcatch,
  here_safe("analysis/out/ttimes/base/district_maxcatch.gz"),
  fwrite
)

# Commune
commune_df <- aggregate_admin(base_df = base_df, admin = "commcode", scenario = 0)
commune_maxcatch <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch,
  na.rm = TRUE
)],
by = .(commcode, scenario)
]
write_create(
  commune_df,
  here_safe("analysis/out/ttimes/base/commune_allcatch.gz"),
  fwrite
)
write_create(
  commune_maxcatch,
  here_safe("analysis/out/ttimes/base/commune_maxcatch.gz"),
  fwrite
)

# Make shapefiles --------------------------------------------------------------

# Match up to CTAR
district_maxcatch %>%
  mutate(
    id_ctar = ctar_metadata$id_ctar[catchment], # by row number
    catchment = ctar_metadata$CTAR[catchment]
  ) -> district_maxcatch

commune_maxcatch %>%
  mutate(
    id_ctar = ctar_metadata$id_ctar[catchment], # by row number
    catchment = ctar_metadata$CTAR[catchment]
  ) -> commune_maxcatch

# Clean up names
# var names have to be <= 10 characters long for ESRI shapefile output
mada_districts %>%
  left_join(district_maxcatch) %>%
  rename(pop = pop_admin, pop_catch = prop_pop_catch) -> mada_districts

mada_communes %>%
  left_join(commune_maxcatch) %>%
  rename(pop = pop_admin, pop_catch = prop_pop_catch) -> mada_communes

# Write out the shapefiles for analyses (overwrite)
write_create(mada_districts,
  here_safe("analysis/out/shapefiles/mada_districts.shp"),
  st_write,
  delete_layer = TRUE
)
write_create(mada_communes,
  here_safe("analysis/out/shapefiles/mada_communes.shp"),
  st_write,
  delete_layer = TRUE
)

# Also simplified shapefiles for lightweight (plotting, etc)
mada_districts <- rmapshaper::ms_simplify(mada_districts)
mada_communes <- rmapshaper::ms_simplify(mada_communes)

write_create(mada_districts,
  here_safe("analysis/out/shapefiles/mada_districts_simple.shp"),
  st_write,
  delete_layer = TRUE
)

write_create(mada_communes,
  here_safe("analysis/out/shapefiles/mada_communes_simple.shp"),
  st_write,
  delete_layer = TRUE
)

# Saving session info
out_session(
  logfile = "logs/log_local.csv", start = start,
  ncores = detectCores() - 1
)

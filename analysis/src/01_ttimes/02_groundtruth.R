# ------------------------------------------------------------------------------
#' Comparing estimates of travel times from multiple sources:
#'  1) MAP estimates using friction surface
#'  2) Self-reported travel times of bite patients reporting to Moramanga ARMC
#'  3) IPM driving times (from Helene and Luciano)
# ------------------------------------------------------------------------------

start <- Sys.time()

# Set-up
library(dplyr)
library(sf)
library(raster)
library(lubridate)
library(data.table)
library(doParallel)
library(foreach)
library(here)
select <- dplyr::select

# Source
source(here("R", "utils.R"))
source(here_safe("R/ttime_functions.R"))

# data
ctar_metadata <- read.csv(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes.shp"))
base_times <- raster(here_safe("analysis/out/ttimes/base/ttimes.tif"))
pop1x1 <- raster(here_safe("data-raw/out/rasters/wp_2015_1x1.tif"))
friction_masked <- raster(here_safe("data-raw/out/rasters/friction_mada_masked.tif"))
ttimes_IPM <- read.csv(here_safe("data-raw/misc/ttimes_IPM.csv")) # fix so this is part of data!
mora <- read.csv(here_safe("data-raw/out/bitedata/moramanga_ttimes.csv"))

# Groundtruthing ---------------------------------------------------------------
# Mora self reported ttimes
mora %>%
  left_join(select(st_drop_geometry(mada_communes), commcode, district,
    from_lat = lat_cent, from_long = long_cent,
    commune, pop, ttimes_wtd, ttimes_un, catchment
  ),
  by = c("commcode" = "commcode")
  ) %>%
  mutate(ttimes_est = ttimes_wtd / 60, ttimes_un = ttimes_un / 60) %>%
  filter(catchment == "Moramanga") %>%
  mutate(
    multiple = rowSums(dplyr::select(., car:Pus)),
    mode = case_when(
      multiple > 1 ~ "Multiple",
      car == 1 ~ "Car",
      Motorbike == 1 ~ "Motorbike",
      foot == 1 ~ "Foot",
      Bus == 1 ~ "Bus",
      Bicycle == 1 ~ "Bicycle",
      Pus == 1 ~ "Pus-pus",
      Other == 1 ~ "Other"
    ),
    to_long = ctar_metadata$long[ctar_metadata$CTAR == "Moramanga"],
    to_lat = ctar_metadata$lat[ctar_metadata$CTAR == "Moramanga"],
    ttimes_reported = hours,
    type = "commune_wtd"
  ) %>%
  select(-(car:known_cat1), -ttimes_wtd) -> gtruth_mora

gtruth_mora$distance <- geosphere::distGeo(
  cbind(gtruth_mora$to_long, gtruth_mora$to_lat),
  cbind(gtruth_mora$from_long, gtruth_mora$from_lat)
) / 1000 # in km

# IPM groundtruthing (ttimes between points) -----------------------------------
point_mat_to <- as.matrix(dplyr::select(ttimes_IPM, x = LongAriv, y = LatAriv))
point_mat_from <- as.matrix(dplyr::select(ttimes_IPM, x = LongDep, y = LatDep))

# for each start and end point
# takes ~ 6 seconds per point
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

system.time({
  foreach(
    points_to = iter(point_mat_to, by = "row"),
    points_from = iter(point_mat_from, by = "row"),
    .packages = c("raster", "gdistance", "data.table")
  ) %dopar% {
    ttimes <- get_ttimes(
      friction = friction_masked, shapefile = mada_communes,
      coords = points_to, trans_matrix_exists = TRUE,
      filename_trans = "data-raw/out/rasters/trans_gc_masked.rds"
    )
    extract(ttimes, SpatialPoints(points_from))
  } -> ttimes_est
})

stopCluster(cl)

ttimes_IPM %>%
  mutate(
    arrival = mdy_hm(paste(Date, Harrival)),
    depart = mdy_hm(paste(Date, Hdepart)),
    ttimes_reported = as.numeric(arrival - depart) / 60,
    ttimes_est = unlist(ttimes_est) / 60,
    type = "point", mode = "Car"
  ) %>%
  select(ttimes_est, ttimes_reported,
    from_lat = LatDep, from_long = LongDep, to_lat = LatAriv,
    to_long = LongAriv,
    type, mode
  ) -> gtruth_IPM
gtruth_IPM$distance <- geosphere::distGeo(
  cbind(gtruth_IPM$to_long, gtruth_IPM$to_lat),
  cbind(gtruth_IPM$from_long, gtruth_IPM$from_lat)
) / 1000 # in km
gtruth_IPM$ttimes_est[is.infinite(gtruth_IPM$ttimes_est)] <- NA

gtruth <- bind_rows(gtruth_IPM, gtruth_mora)

write_create(gtruth, "analysis/out/ttimes/gtruth_ttimes.csv",
  write.csv,
  row.names = FALSE
)

# Save session info ------------------------------------------------------------
out_session("logs/log_local.csv", start = start, ncores = detectCores() - 1)


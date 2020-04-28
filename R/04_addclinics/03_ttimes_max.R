# ------------------------------------------------------------------------------------------------ #
#' Getting max ttimes (i.e. if all CSB IIs in Mada have a clinic)
#' Can run this locally (takes ~ 20 minutes)
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Libraries
library(rgdal)
library(raster)
library(foreach)
library(tidyverse)
library(gdistance)
library(iterators)
library(doParallel)
library(data.table)

# Source
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# candidate points
csbs <- read.csv("data/processed/clinics/csb2.csv", stringsAsFactors = FALSE)
point_mat_all <- as.matrix(rbind(dplyr::select(csbs, long, lat),
                           dplyr::select(ctar_metadata, long = LONGITUDE, lat = LATITUDE)))

# Max raster
ttimes_max <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                          coords = point_mat_all, trans_matrix_exists = TRUE,
                          filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_max, "output/ttimes/max_ttimes.tif", overwrite = TRUE)

# Max at grid level to get district/comm dataframes (inlcude baseline CTAR too!)
cand_mat <- fread("output/ttimes/candidate_matrix.gz") # locally
cand_mat <- as.matrix(cand_mat)
base_mat <- as.matrix(fread("output/ttimes/baseline_matrix.gz"))
cand_mat <- cbind(base_mat, cand_mat)
max_catches <- apply(cand_mat, 1, which.min)
max_times <- apply(cand_mat, 1, min, na.rm = TRUE)
max_catches[is.infinite(max_times)] <- NA
max_times[is.infinite(max_times)] <- NA # no path (some island cells, etc.)

# Get df with district and commune ids and aggregate accordingly
max_df <- fread("output/ttimes/baseline_grid.gz")

# Use max ones because only if no clinic possible to be added will it be Inf
max_df[, c("ttimes", "catchment") := .(max_times, max_catches)]
fwrite(max_df, "output/ttimes/max_grid.gz") # compress to save space


# Get weighted ttimes by pop for districts/communes -------------------------------------------

# Remove NA catchments
# deal with NAs
max_to_agg <- max_df[!is.na(ttimes)]
max_to_agg[, pop_wt_dist := sum(pop, na.rm = TRUE), by = distcode]
max_to_agg[, pop_wt_comm := sum(pop, na.rm = TRUE), by = commcode]

# District
district_df <-
  max_to_agg[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE), 
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_dist[1], 
              pop_wt_dist = pop_wt_dist[1], pop = pop_dist[1],
              scenario = "max"), 
          by = .(distcode, catchment)]
district_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop_wt_dist, by = distcode]
fwrite(district_df, "output/ttimes/max_district.csv")

commune_df <-
  max_to_agg[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE),
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_comm[1], 
              pop_wt_comm = pop_wt_comm[1], pop = pop_comm[1],
              scenario = "max"), 
          by = .(commcode, catchment)]
commune_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop_wt_comm, by = commcode]
fwrite(commune_df, "output/ttimes/max_commune.csv")

# Save session info
out.session(path = "R/04_addclinics/03_ttimes_max.R", filename = "output/log_local.csv", 
            start = start)
# ------------------------------------------------------------------------------------------------ #
#' Getting ttimes for one clinic per admin unit scenarios
#' Can run this locally (takes ~ 30 minutes)
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
library(glue)

# Source
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# one clinic per district (this already includes existing ctar)
clinic_per_dist <- read.csv("data/processed/clinics/clinic_per_dist.csv", 
                            stringsAsFactors = FALSE)
point_mat_all <- as.matrix(dplyr::select(clinic_per_dist, long, lat))

# Max raster
ttimes_per_dist <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                         coords = point_mat_all, trans_matrix_exists = TRUE,
                         filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_per_dist, "output/ttimes/clin_per_dist_ttimes.tif", overwrite = TRUE)


# one clinic per commune (this already includes existing ctar)
clinic_per_comm <- read.csv("data/processed/clinics/clinic_per_comm.csv", 
                            stringsAsFactors = FALSE)
point_mat_all <- as.matrix(dplyr::select(clinic_per_comm, long, lat))

# Max raster
ttimes_per_comm <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                              coords = point_mat_all, trans_matrix_exists = TRUE,
                              filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_per_comm, "output/ttimes/clin_per_comm_ttimes.tif", overwrite = TRUE)

# Max at grid level to get district/comm dataframes (inlcude baseline CTAR too!)
cand_mat <- fread("output/ttimes/candidate_matrix.gz") # locally
cand_mat <- as.matrix(cand_mat)
base_mat <- as.matrix(fread("output/ttimes/baseline_matrix.gz"))
cand_mat <- cbind(base_mat, cand_mat)

scale <- c("dist", "comm")

foreach(i = 1:length(scale)) %do% {
  
  if (scale[i] == "dist") {
    clinics <- clinic_per_dist
  } else {
    clinics <- clinic_per_comm
  }
  in_mat <- clinics[clinics$clinic_id < ncol(cand_mat), ]
  clin_mat <- cand_mat[, in_mat$clinic_id] # which clinics in that scenario
  clinic_ids <- in_mat$clinic_id
  missing <- clinics[clinics$clinic_id > ncol(cand_mat), ]
  
  if(nrow(missing) > 0) {
    point_mat <- as.matrix(dplyr::select(missing, long, lat))
    
    foreach(points = iter(point_mat, by = "row")) %do% {
      ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                           coords = points, trans_matrix_exists = TRUE,
                           filename_trans = "data/processed/rasters/trans_gc_masked.rds")
    } -> stacked_ttimes
    
    stacked_ttimes <- do.call("stack", stacked_ttimes)
    stacked_ttimes <- raster::as.matrix(stacked_ttimes)
    stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ] 
    clin_mat <- cbind(clin_mat, stacked_ttimes)
    clinic_ids <- c(clinic_ids, missing$clinic_id)
  }
  
  max_catches <- clinic_ids[apply(clin_mat, 1, which.min)]
  max_times <- apply(clin_mat, 1, min, na.rm = TRUE)
  max_catches[is.infinite(max_times)] <- NA
  max_times[is.infinite(max_times)] <- NA # no path (some island cells, etc.)
  
  # Get df with district and commune ids and aggregate accordingly
  max_df <- fread("output/ttimes/baseline_grid.gz")
  
  # Use max ones because only if no clinic possible to be added will it be Inf
  max_df[, c("ttimes", "catchment") := .(max_times, max_catches)]
  fwrite(max_df, glue("output/ttimes/clin_per_{scale[i]}_grid.gz")) # compress to save space
  
  max_to_agg <- max_df[!is.na(ttimes)]
  max_to_agg[, pop_wt_dist := sum(pop, na.rm = TRUE), by = distcode]
  max_to_agg[, pop_wt_comm := sum(pop, na.rm = TRUE), by = commcode]
  
  # District
  district_df <-
    max_to_agg[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE), 
                   prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_dist[1], 
                   pop_wt_dist = pop_wt_dist[1], pop = pop_dist[1],
                   scenario = glue("armc_per_{scale[i]}")), 
               by = .(distcode, catchment)]
  district_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop_wt_dist, by = distcode]
  fwrite(district_df, glue("output/ttimes/armc_per_{scale[i]}_district.csv"))
  
  commune_df <-
    max_to_agg[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE),
                   prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_comm[1], 
                   pop_wt_comm = pop_wt_comm[1], pop = pop_comm[1],
                   scenario = glue("armc_per_{scale[i]}")), 
               by = .(commcode, catchment)]
  commune_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop_wt_comm, by = commcode]
  fwrite(commune_df, glue("output/ttimes/armc_per_{scale[i]}_commune.csv"))
}

# Save session info
out.session(path = "R/04_addclinics/04_clinic_per_admin.R", filename = "output/log_local.csv", 
            start = start)

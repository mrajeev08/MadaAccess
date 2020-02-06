# ------------------------------------------------------------------------------------------------ #
#' Getting baseline travel time estimates and catchments for grid cells; also summarizing
#' to admin units 
#' Given the 31 existing clinics in the country
# ------------------------------------------------------------------------------------------------ #

# Set up
library(rgdal)
library(raster)
library(gdistance)
library(foreach)
library(doParallel)
library(iterators)
library(data.table)

# Source
source("R/functions/ttime_functions.R")
source("R/functions/out.session.R")

# Load in GIS files
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1 <- raster("data/processed/rasters/fb_2018_1x1.tif")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# Get candidate points as matrix
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
point_mat_base <- as.matrix(dplyr::select(ctar_metadata, x = LONGITUDE, y = LATITUDE))

# For each clinic get the minimum travel times at the raster scale (for the 31 existing)
# takes ~ 6 seconds per point
cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  foreach(points = iter(point_mat_base, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
          } -> stacked_ttimes
})

stopCluster(cl) 

# stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ] # to save memory filter out NAs

# ids for districts and communes (extract row # of shapefiles to the raster, takes a while!)
district_id <- getValues(rasterize(mada_districts, 
                                  friction_masked))[!is.na(getValues(friction_masked))]
commune_id <- getValues(rasterize(mada_communes, 
                                 friction_masked))[!is.na(getValues(friction_masked))]

# Max at grid level to get district/comm dataframes
catchment <- apply(stacked_ttimes, 1, which.min)
ttimes <- apply(stacked_ttimes, 1, min, na.rm = TRUE)
ttimes[is.infinite(ttimes)] <- NA # no path (some island cells, etc.)

# Baseline dataframe at grid level with ttimes + pop (from fb 2018 aggregated to friction surface)
baseline_df <- data.table(district_id = district_id, commune_id = commune_id, 
                          pop = getValues(pop1x1)[!is.na(getValues(friction_masked))], 
                          ttimes, catchment)
baseline_df[, prop_pop := pop/sum(pop, na.rm = TRUE)]
baseline_df[, pop_dist := sum(pop, na.rm = TRUE), by = district_id]
baseline_df[, pop_comm := sum(pop, na.rm = TRUE), by = commune_id]

fwrite(baseline_df, "output/ttimes/baseline_grid.csv")

## Get weighted ttimes (ttimes weighted by the pop in each grid cell)
# District
district_df <-
  baseline_df[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE), 
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_dist[1], pop = pop_dist[1],
              scenario = 0), 
          by = .(district_id, catchment)] # first by catchment to get the max catch
district_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop, by = district_id] # then by district
fwrite(district_df, "output/ttimes/baseline_district.csv")

# Commune
commune_df <-
  baseline_df[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE),
              prop_pop_catch = sum(pop, na.rm = TRUE)/pop_comm[1], pop = pop_comm[1],
              scenario = 0), 
          by = .(commune_id, catchment)]
commune_df[, ttimes_wtd := sum(ttimes_wtd, na.rm = TRUE)/pop, by = commune_id]
fwrite(commune_df, "output/ttimes/baseline_commune.csv")

# Get raster of baseline travel times
# Also quick comparison check: should generate the same min estimates as the ttimes layer above
ttimes_comp <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                          coords = point_mat_base, trans_matrix_exists = TRUE,
                          filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_comp, "output/ttimes/ttimes_baseline.tif")
ttimes_comp <- getValues(ttimes_comp)[!is.na(getValues(friction_masked))]
sum(ttimes - ttimes_comp, na.rm = TRUE) # should ~ 0

# Saving session info
out.session(path = "R/01_gis/04_run_baseline.R", filename = "sessionInfo.csv")

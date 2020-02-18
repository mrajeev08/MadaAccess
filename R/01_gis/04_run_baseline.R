# ------------------------------------------------------------------------------------------------ #
#' Getting baseline travel time estimates and catchments for grid cells; also summarizing
#' to admin units 
#' Given the 31 existing clinics in the country
# ------------------------------------------------------------------------------------------------ #

# Set-up --------------------------------------------------------------------------------------
library(rgdal)
library(raster)
library(gdistance)
library(foreach)
library(doParallel)
library(iterators)
library(data.table)
library(dplyr)
library(lubridate)

# Source
source("R/functions/ttime_functions.R")
source("R/functions/out.session.R")

# Load in GIS files
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")

# Load in rasters
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# Get candidate points as matrix
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
point_mat_base <- as.matrix(dplyr::select(ctar_metadata, x = LONGITUDE, y = LATITUDE))

# Get the minimum ttimes for each clinic ------------------------------------------------------
# takes ~ 6 seconds per point
cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  foreach(points = iter(point_mat_base, by = "row"),
          .packages = c("raster", "gdistance", "data.table"), 
          .export = c("get.ttimes")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
          } -> stacked_ttimes
})

stopCluster(cl) 

# stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)

# to save memory filter out NAs
# these will always be NAs as the input friction masked values are NA
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ] 

# write baseline matrix out 
fwrite(stacked_ttimes, "output/ttimes/baseline_matrix.gz") 

# Get the catchment and ttimes for each grid cell
catchment <- apply(stacked_ttimes, 1, which.min) # row id from ctar metadata
ttimes <- apply(stacked_ttimes, 1, min, na.rm = TRUE)
catchment[is.infinite(ttimes)] <- NA
ttimes[is.infinite(ttimes)] <- NA # no path (some island cells, etc.)

# Match grid cells to commune & district ids --------------------------------------------------
# extract row # of shapefiles to the raster (takes ~ 6 min)
district_id <- getValues(rasterize(mada_districts, 
                                  friction_masked))[!is.na(getValues(friction_masked))]
commune_id <- getValues(rasterize(mada_communes, 
                                 friction_masked))[!is.na(getValues(friction_masked))]

# Baseline dataframe at grid level with ttimes + pop
baseline_df <- data.table(matchcode = mada_districts$ADM2_PCODE[district_id], 
                          commcode = mada_communes$ADM3_PCODE[commune_id], 
                          pop = getValues(pop1x1)[!is.na(getValues(friction_masked))], 
                          ttimes, catchment)

# Match to distcode where all Tana polygons are 1 district
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7) 
baseline_df$distcode <- mada_districts$distcode[match(baseline_df$matchcode, 
                                                      mada_districts$ADM2_PCODE)]

# Fix Nosy Komba so it has max ttimes in mainland Nosy Be & catchment of Nosy Be
# Only other island and so not always adding to this place first!
baseline_df[commcode == "MG71718002"]$ttimes <- max(baseline_df[distcode == "MG71718"]$ttimes, 
                                                    na.rm = TRUE)
baseline_df[commcode == "MG71718002"]$catchment <- baseline_df[distcode == "MG71718"]$catchment[1]

# sum by commune + district
baseline_df[, prop_pop := pop/sum(pop, na.rm = TRUE)]
baseline_df[, pop_dist := sum(pop, na.rm = TRUE), by = distcode]
baseline_df[, pop_comm := sum(pop, na.rm = TRUE), by = commcode]

fwrite(baseline_df, "output/ttimes/baseline_grid.gz") # compress to save space

# Get weighted ttimes by pop for districts/communes -------------------------------------------

# deal with NAs
base_to_agg <- baseline_df[!is.na(ttimes)]
base_to_agg[, pop_wt_dist := sum(pop, na.rm = TRUE), by = distcode]
base_to_agg[, pop_wt_comm := sum(pop, na.rm = TRUE), by = commcode]

# District
district_df <-
  base_to_agg[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE), 
                  ttimes_un = sum(ttimes, na.rm = TRUE),
                  ncells = .N,
                  prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_dist[1], 
                  pop_wt_dist = pop_wt_dist[1], pop = pop_dist[1],
                  scenario = 0), 
              by = .(distcode, catchment)] # first by catchment to get the max catch
district_df[, c("ttimes_wtd", 
                "ttimes_un") := .(sum(ttimes_wtd, na.rm = TRUE)/pop_wt_dist,
                                  sum(ttimes_un, na.rm = TRUE)/sum(ncells, na.rm = TRUE)), 
                                  by = distcode] # then by district
fwrite(district_df, "output/ttimes/baseline_district.csv")

# Commune
commune_df <-
  base_to_agg[, .(ttimes_wtd = sum(ttimes * pop, na.rm = TRUE), 
                  ttimes_un = sum(ttimes, na.rm = TRUE),
                  ncells = .N,
                  prop_pop_catch = sum(pop, na.rm = TRUE)/pop_wt_comm[1], 
                  pop_wt_comm = pop_wt_comm[1], pop = pop_comm[1],
                  scenario = 0), 
              by = .(commcode, catchment)] # first by catchment to get the max catch
commune_df[, c("ttimes_wtd", 
                "ttimes_un") := .(sum(ttimes_wtd, na.rm = TRUE)/pop_wt_comm,
                                  sum(ttimes_un, na.rm = TRUE)/sum(ncells, na.rm = TRUE)), 
            by = commcode] # then by district
fwrite(commune_df, "output/ttimes/baseline_commune.csv")

# Get raster of baseline ttimes ---------------------------------------------------------------
# Also quick comparison check: should generate the same min estimates as the ttimes layer above
ttimes_comp <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                          coords = point_mat_base, trans_matrix_exists = TRUE,
                          filename_trans = "data/processed/rasters/trans_gc_masked.rds")
writeRaster(ttimes_comp, "output/ttimes/base_ttimes.tif", overwrite = TRUE)
ttimes_comp <- getValues(ttimes_comp)[!is.na(getValues(friction_masked))]
sum(ttimes - ttimes_comp, na.rm = TRUE) # should ~ 0

# Saving session info
out.session(path = "R/01_gis/04_run_baseline.R", filename = "output/log_local.csv")

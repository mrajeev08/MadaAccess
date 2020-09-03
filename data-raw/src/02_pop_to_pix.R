# ------------------------------------------------------------------------------------------------ #
#' Aggregating wp pop to friction surface
#' Matching pop with NA friction value to nearest non-NA friction value
#' and then aggregating to the friction surface
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Set-up
library(here)
library(raster)
library(data.table)
source("R/out.session.R")
source("R/match_pop.R")

# Load in frition surface and then convert to spatial pixels
friction_masked <- raster(here("data-raw/outputs/rasters/friction_mada_masked.tif"))
friction_pix <- as(friction_masked, "SpatialPixelsDataFrame") 
friction_pix$match_id <- friction_pix@grid.index

# WP 2015 original 
wp_2015 <- raster(here("data-raw/inputs/rasters/WorldPop/MDG_ppp_2015_adj_v2.tif"))
wp_2015_pix <- as(wp_2015, "SpatialPixelsDataFrame")

# here minDimension which ranks intersections
wp_2015_pix$match_id<- over(wp_2015_pix, friction_pix, minDimension = 0)$match_id
match <- raster(wp_2015_pix["match_id"]) # transform back to raster

# For ones that are missing find the nearest non NA cell id
wp_2015_unmatched <- wp_2015_pix[is.na(wp_2015_pix$match_id), ]
match <- match_nearest(unmatched_pix = wp_2015_unmatched, matched_raster = match, 
                       max_adjacent = 100)

# Summarize pop to this updated raster
pop_dt <- data.table(pop = wp_2015[], friction_id = match[])
pop_dt <- pop_dt[, .(pop = sum(pop, na.rm = TRUE)), by = "friction_id"][!is.na(friction_id)]
friction_pix$pop <- pop_dt$pop[match(friction_pix$match_id, pop_dt$friction_id)]
pop <- raster(friction_pix["pop"]) # transform back to raster

# should be true (or minimal difference)
sum(pop[], na.rm = TRUE) - sum(wp_2015[], na.rm = TRUE)

writeRaster(pop, "data-raw/outputs/rasters/wp_2015_1x1.tif", overwrite = TRUE, 
            options=c("COMPRESS=LZW"))

# Save session info
out.session(path = "data-raw/src/02_pop_to_pix.R", filename = "output/log_local.csv")


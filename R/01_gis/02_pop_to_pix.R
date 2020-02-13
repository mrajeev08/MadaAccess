# ------------------------------------------------------------------------------------------------ #
#' Pop rasters to pixels
#' Matching NA pops to non NA cells; this takes a long time and lots of memory!
#' Ran it on Della @ Princeton and allocated 25 gigs of RAM; takes ~ 2 hour to run 
# ------------------------------------------------------------------------------------------------ #

# Set-up
library(raster)
library(data.table)
source("R/functions/out.session.R")
source("R/functions/match_pop.R")

# Load in frition surface and then convert to spatial pixels
friction_masked <- as(raster("data/processed/rasters/friction_mada_masked.tif"),
                      "SpatialPixelsDataFrame") 
friction_masked$cell_id <- 1:length(friction_masked)

# WP 2015
wp_2015 <- raster("data/raw/WorldPop/MDG_ppp_2015_adj_v2.tif")
wp_2015_matched <- pop_to_pixels(friction_pixels = friction_masked, 
                                 pop_raster = wp_2015, nmoves = 250)
library(raster)
writeRaster(wp_2015_matched, "data/processed/rasters/wp_2015_temp.tif", overwrite = TRUE, 
            options=c("COMPRESS=LZW"))
sum(getValues(wp_2015), na.rm = TRUE)
sum(getValues(wp_2015_matched), na.rm = TRUE)

# Save session info
out.session(path = "R/01_gis/02_pop_to_pix.R", filename = "output/log_cluster.csv")

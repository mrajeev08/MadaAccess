# ------------------------------------------------------------------------------------------------ #
#' Pop rasters to pixels
#' Matching NA pops to non NA cells; this takes a long time and lots of memory!
#' Ran it on Della @ Princeton and allocated 25 gigs of RAM; takes ~ 1 hours to run 
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

# Facebook 2018 pop estimates
fb_2018 <- raster("data/raw/population_mdg_2018-10-01-2/fb2018_aggregated.tif")
fb_2018_pix <- pop_to_pixels(friction_pixels = friction_masked, pop_raster = fb_2018, nmoves = 200)
sum(getValues(fb_2018), na.rm = TRUE)
sum(fb_2018_pix$pop, na.rm = TRUE)
saveRDS(fb_2018_pix, "data/temp_pop/fb_2018_temp.rds")

# Out session
out.session(path = "R/01_gis/02_pop_to_pix.R", filename = "sessionInfo.csv")

# ------------------------------------------------------------------------------------------------ #
#' Aggregating pop estimates to friction surgace
#' Getting pop to same extent and resolution as the friction surface
# ------------------------------------------------------------------------------------------------ #

# Set up
library(raster) 
library(data.table)
source("R/functions/out.session.R")

# Load in frition surface and then convert to spatial pixels
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
friction_pixels <- as(friction_masked, "SpatialPixelsDataFrame") 
friction_pixels$cell_id <- 1:length(friction_pixels)

# Aggregate pop to friction surface
pop_fb <- readRDS("data/temp_pop/fb_2018_temp.rds")
check <- data.table(pop = pop_fb$pop, cell_id = pop_fb$cell_id)
check <- check[ , .(pop = sum(pop, na.rm = TRUE)), by = "cell_id"]
check <- check[!is.na(cell_id)]
friction_pixels$pop <- check$pop[match(friction_pixels$cell_id, check$cell_id)]
fb_2018_1x1 <- raster(friction_pixels["pop"]) # transform back to raster
writeRaster(fb_2018_1x1, "data/processed/rasters/fb_2018_1x1.tif", overwrite = TRUE)

fb_2018 <- raster("data/raw/population_mdg_2018-10-01-2/fb2018_aggregated.tif")
sum(getValues(fb_2018), na.rm = TRUE) - sum(getValues(fb_2018_1x1), na.rm = TRUE)

## Saving session info
out.session(path = "R/01_gis/03_aggregate_pop.R", filename = "sessionInfo.csv")


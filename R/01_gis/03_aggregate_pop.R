# ------------------------------------------------------------------------------------------------ #
#' Aggregating pop estimates to friction surgace
#' Getting pop to same extent and resolution as the friction surface
#' Splitting by districts to speed up
# ------------------------------------------------------------------------------------------------ #

# Set up
library(raster) 
library(data.table)
source("R/functions/out.session.R")

# Load in frition surface and then convert to spatial pixels
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
friction_pixels <- as(friction_masked, "SpatialPixelsDataFrame") 
friction_pixels$cell_id <- 1:length(friction_pixels)

# Load in World Pop data
wp_2015 <- raster("data/processed/rasters/wp_2015_temp.tif")
pop_2015 <- as(wp_2015, "SpatialPixelsDataFrame")
names(pop_2015) <- "pop"

# Get corresponding cell id of fric surface
# take either area or line intersection (with minDimension = 0)
pop_2015$cell_id <- over(pop_2015, friction_pixels, minDimension = 0)$cell_id  

# Make into data table and aggregate
check <- data.table(pop = pop_2015$pop, cell_id = pop_2015$cell_id)
check <- check[ , .(pop = sum(pop, na.rm = TRUE)), by = "cell_id"]
check <- check[!is.na(cell_id)]
friction_pixels$pop <- check$pop[match(friction_pixels$cell_id, check$cell_id)]

# transform back to raster
wp_2015_1x1 <- raster(friction_pixels["pop"])
writeRaster(wp_2015_1x1, "data/processed/rasters/wp_2015_1x1.tif", overwrite = TRUE)

# these should be equalish!
sum(getValues(wp_2015), na.rm = TRUE)
sum(getValues(wp_2015_1x1), na.rm = TRUE)

# Saving session info
out.session(path = "R/01_gis/03_aggregate_pop.R", filename = "sessionInfo.csv")


# ------------------------------------------------------------------------------------------------ #
#' Getting baseline travel time estimates and catchments for grid cells; also summarizing
#' to admin units 
#' Given the 31 existing clinics in the country
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

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
library(rgeos) # for dissolving the admin units
library(rmapshaper)

# Source
source("R/functions/ttime_functions.R")
source("R/functions/out.session.R")

# Load in GIS files
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- readOGR("data/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")

# Fix up shapefiles ------------------------------------------------------
# Get distcodes for both admin levels
mada_districts$distcode <- substring(as.character(mada_districts$ADM2_PCODE), 1, 7)
mada_communes$distcode <- substring(as.character(mada_communes$ADM2_PCODE), 1, 7)

# Dissolving districts for Tana to one district: Antananarivo Renivohitra 
districts_dissolved <- gUnaryUnion(mada_districts, id = mada_districts$distcode)
districts_to_merge <- mada_districts@data[6:nrow(mada_districts@data), ]
districts_to_merge$ADM2_EN <- recode(districts_to_merge$ADM2_EN, 
                                     `6e Arrondissement` = "Antananarivo Renivohitra")
row.names(districts_dissolved) <- as.character(1:nrow(districts_to_merge))
row.names(districts_to_merge) <- as.character(1:nrow(districts_to_merge))
mada_districts <- SpatialPolygonsDataFrame(districts_dissolved, districts_to_merge)

# Fix mada commune names as well for Tana
mada_communes$ADM2_EN <- as.character(mada_communes$ADM2_EN)
mada_communes$ADM2_EN[mada_communes$distcode == "MG11101"] <- "Antananarivo Renivohitra"

# Get the minimum ttimes for each clinic ------------------------------------------------------
# Load in rasters
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
prop_pop <- pop1x1/sum(values(pop1x1), na.rm = TRUE)

friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# Get candidate points as matrix
point_mat_base <- as.matrix(dplyr::select(ctar_metadata, x = LONGITUDE, y = LATITUDE))

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

# write the brick
bricked <- brick(stacked_ttimes)
names(bricked) <- 1:31
writeRaster(bricked, filename = "output/ttimes/candidates/candmat_cand1_cand31.tif",
            overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))    

# Get minimum travel times and write out to tiff
ttimes_base <- min(bricked, na.rm = TRUE)
catchment <- which.min(bricked)
writeRaster(ttimes_base, "output/ttimes/base_ttimes.tif", 
             overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))

# Get vals by  districts/communes -------------------------------------------
# extract row # of shapefiles to the raster (takes ~ 6 min, so only do this if not already done!)
if(!file.exists("output/ttimes/district_id.tif")) {
  district_id <- rasterize(mada_districts, friction_masked)
  writeRaster(district_id, "output/ttimes/district_id.tif",
              overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))
} else {
  district_id <- raster("output/ttimes/district_id.tif")
}

if(!file.exists("output/ttimes/commune_id.tif")) {
  commune_id <- rasterize(mada_communes, friction_masked)
  writeRaster(commune_id, "output/ttimes/commune_id.tif",
              overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))
} else {
  commune_id <- raster("output/ttimes/commune_id.tif")
}

base_df <- data.table(distcode = mada_districts$distcode[values(district_id)], 
                      commcode = mada_communes$ADM3_PCODE[values(commune_id)], 
                      pop = values(pop1x1), prop_pop = values(prop_pop),
                      ttimes = values(ttimes_base), catchment = values(catchment))

# Fix Nosy Komba so it has max ttimes in mainland Nosy Be & catchment of Nosy Be
# Only other island and so not always adding to this place first!
base_df[commcode == "MG71718002"]$ttimes <- max(base_df[distcode == "MG71718"]$ttimes, 
                                                    na.rm = TRUE)
base_df[commcode == "MG71718002"]$catchment <- base_df[distcode == "MG71718"]$catchment[1]
fwrite(base_df, "output/ttimes/base_df.gz")

# District
district_df <- aggregate.admin(base_df = base_df, admin = "distcode", scenario = 0)
district_maxcatch <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                                 by = .(distcode, scenario)]
fwrite(district_df, "output/ttimes/base_district_allcatch.gz")
fwrite(district_maxcatch, "output/ttimes/base_district_maxcatch.gz")

# Commune
commune_df <- aggregate.admin(base_df = base_df, admin = "commcode", scenario = 0)
commune_maxcatch <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                               by = .(commcode, scenario)]
fwrite(commune_df, "output/ttimes/base_commune_allcatch.gz")
fwrite(commune_maxcatch, "output/ttimes/base_commune_maxcatch.gz")

# Make shapefiles -----------------------------------------------------------------------------
district_maxcatch$id_ctar <- ctar_metadata$id_ctar[district_maxcatch$catchment] # by row number
district_maxcatch$catchment <- ctar_metadata$CTAR[district_maxcatch$catchment] # by row number
mada_districts@data <- district_maxcatch[mada_districts@data, on = "distcode"]

# Do the same for commune level
commune_maxcatch$id_ctar <- ctar_metadata$id_ctar[commune_maxcatch$catchment] # by row number
commune_maxcatch$catchment <- ctar_metadata$CTAR[commune_maxcatch$catchment] # by row number
mada_communes@data <- commune_maxcatch[mada_communes@data, on = c("commcode" = "ADM3_PCODE")] # join

# Get centroid longitude and latitude (for plotting)
mada_districts$long_cent <- coordinates(mada_districts)[, 1]
mada_districts$lat_cent <- coordinates(mada_districts)[, 2]
mada_communes$long_cent <- coordinates(mada_communes)[, 1]
mada_communes$lat_cent <-  coordinates(mada_communes)[, 2]

# Clean up names
# NOTE: var names have to be <= 10 characters long for ESRI shapefile output
mada_districts@data %>%
  dplyr::select(distcode, district = ADM2_EN, pop = pop_admin, long_cent, lat_cent, ttimes_wtd, 
                ttimes_un, catchment, id_ctar, pop_catch = prop_pop_catch) -> mada_districts@data

mada_communes@data %>%
  dplyr::select(distcode, district = ADM2_EN, commcode, commune = ADM3_EN, pop = pop_admin, 
                long_cent, lat_cent, ttimes_wtd, ttimes_un, catchment, id_ctar,
                pop_catch = prop_pop_catch) -> mada_communes@data

# Write out the shapefiles (overwrite) 
writeOGR(mada_communes, dsn = "data/processed/shapefiles", layer = "mada_communes", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_districts, dsn = "data/processed/shapefiles", layer = "mada_districts", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Also simplified shapefiles for easier plotting 
mada_districts <- rmapshaper::ms_simplify(mada_districts)
mada_communes <- rmapshaper::ms_simplify(mada_communes)
writeOGR(mada_districts, "data/processed/shapefiles", layer = "mada_districts_simple", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(mada_communes, "data/processed/shapefiles", layer = "mada_communes_simple", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Saving session info
out.session(path = "R/01_gis/04_run_baseline.R", filename = "output/log_local.csv", 
            start = start)

# ------------------------------------------------------------------------------------------------ #
#' Getting baseline travel time estimates and catchments @ grid cell level
#' Given the 31 existing clinics in the country
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Set-up --------------------------------------------------------------------------------------
library(sf)
library(raster)
library(gdistance)
library(foreach)
library(doParallel)
library(iterators)
library(data.table)
library(dplyr)
library(lubridate)
library(rmapshaper)
library(fasterize)
library(here)

# Source
source("R/functions/ttime_functions.R")
source("R/functions/out.session.R")

# Load in GIS files
mada_districts <- st_read("data-raw/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
mada_communes <- st_read("data-raw/raw/shapefiles/communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
ctar_metadata <- read.csv("data-raw/raw/ctar_metadata.csv")

# Fix up shapefiles ------------------------------------------------------
# Get distcodes for both admin levels
mada_districts %>%
  mutate(distcode = substring(as.character(ADM2_PCODE), 1, 7)) -> mada_districts

mada_districts %>%
  group_by(distcode) %>%
  summarize(geometry = st_union(geometry)) %>%
  left_join(st_drop_geometry(mada_districts[6:nrow(mada_districts), ])) %>%
  mutate(ADM2_EN = recode(ADM2_EN, 
                          `6e Arrondissement` = "Antananarivo Renivohitra")) -> mada_districts

mada_communes %>%
  mutate(distcode = substring(as.character(ADM2_PCODE), 1, 7),
         ADM2_EN = case_when(distcode == "MG11101" ~ "Antananarivo Renivohitra",
                             TRUE ~ ADM2_EN)) -> mada_communes

# Get the minimum ttimes for each clinic ------------------------------------------------------
# Load in rasters
pop1x1 <- raster("data-raw/out/rasters/wp_2015_1x1.tif")
prop_pop <- pop1x1/sum(values(pop1x1), na.rm = TRUE)

friction_masked <- raster("data-raw/out/rasters/friction_mada_masked.tif")

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
                                 filename_trans = "data-raw/out/rasters/trans_gc_masked.rds")
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
writeRaster(ttimes_base, "output/ttimes/base/ttimes.tif", 
            overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))

# Get vals by  districts/communes -------------------------------------------
mada_districts$id <- 1:nrow(mada_districts)
district_id <- fasterize(st_collection_extract(mada_districts), friction_masked, field = "id")
writeRaster(district_id, "output/ttimes/district_id.tif",
            overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))

mada_communes$id <- 1:nrow(mada_communes)
commune_id <- fasterize(mada_communes, friction_masked, field = "id")
writeRaster(commune_id, "output/ttimes/commune_id.tif", overwrite = TRUE, 
            options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))

base_df <- data.table(distcode = mada_districts$distcode[values(district_id)], 
                      commcode = mada_communes$ADM3_PCODE[values(commune_id)], 
                      pop = values(pop1x1), prop_pop = values(prop_pop),
                      ttimes = values(ttimes_base), catchment = values(catchment))

# Fix Nosy Komba so it has max ttimes in mainland Nosy Be & catchment of Nosy Be
base_df[commcode == "MG71718002"]$ttimes <- max(base_df[distcode == "MG71718"]$ttimes, 
                                                na.rm = TRUE)
base_df[commcode == "MG71718002"]$catchment <- base_df[distcode == "MG71718"]$catchment[1]
fwrite(base_df, "output/ttimes/base/grid_df.gz")

# District
district_df <- aggregate.admin(base_df = base_df, admin = "distcode", scenario = 0)
district_maxcatch <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                                 by = .(distcode, scenario)]
fwrite(district_df, "output/ttimes/base/district_allcatch.gz")
fwrite(district_maxcatch, "output/ttimes/base/district_maxcatch.gz")

# Commune
commune_df <- aggregate.admin(base_df = base_df, admin = "commcode", scenario = 0)
commune_maxcatch <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                               by = .(commcode, scenario)]
fwrite(commune_df, "output/ttimes/base/commune_allcatch.gz")
fwrite(commune_maxcatch, "output/ttimes/base/commune_maxcatch.gz")

# Make shapefiles -----------------------------------------------------------------------------
district_maxcatch %>%
  mutate(id_ctar = ctar_metadata$id_ctar[catchment], # by row number
         catchment = ctar_metadata$CTAR[catchment]) -> district_maxcatch 

# Do the same for commune level
commune_maxcatch %>%
  mutate(id_ctar = ctar_metadata$id_ctar[catchment], # by row number
         catchment = ctar_metadata$CTAR[catchment]) -> commune_maxcatch 

# Clean up names
# NOTE: var names have to be <= 10 characters long for ESRI shapefile output
mada_districts %>%
  left_join(district_maxcatch) %>%
  dplyr::select(distcode, district = ADM2_EN, pop = pop_admin, ttimes_wtd, 
                ttimes_un, catchment, id_ctar, pop_catch = prop_pop_catch) %>%
  mutate(long_cent = st_coordinates(st_centroid(.))[, 1], 
         lat_cent = st_coordinates(st_centroid(.))[, 2]) -> mada_districts

mada_communes %>%
  left_join(commune_maxcatch, by = c("ADM3_PCODE" = "commcode")) %>%
  dplyr::select(distcode, district = ADM2_EN, commcode = ADM3_PCODE, commune = ADM3_EN, pop = pop_admin, 
                ttimes_wtd, ttimes_un, catchment, id_ctar,
                pop_catch = prop_pop_catch) %>%
  mutate(long_cent = st_coordinates(st_centroid(.))[, 1], 
         lat_cent = st_coordinates(st_centroid(.))[, 2]) -> mada_communes

# Write out the shapefiles (overwrite) 
st_write(mada_districts, "data/processed/shapefiles/mada_districts.shp", 
         delete_layer = TRUE)
st_write(mada_communes, "data/processed/shapefiles/mada_communes.shp", 
         delete_layer = TRUE)

# Also simplified shapefiles for easier plotting 
mada_districts <- rmapshaper::ms_simplify(mada_districts)
mada_communes <- rmapshaper::ms_simplify(mada_communes)
st_write(mada_districts, "data/processed/shapefiles/mada_districts_simple.shp", 
         delete_layer = TRUE)
st_write(mada_communes, "data/processed/shapefiles/mada_communes_simple.shp", 
         delete_layer = TRUE)

# Saving session info
out.session(path = "R/01_gis/04_run_baseline.R", filename = "output/log_local.csv", 
            start = start)

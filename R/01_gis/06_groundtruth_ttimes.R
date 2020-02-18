# ------------------------------------------------------------------------------------------------ #
#' Comparing estimates of travel times from multiple sources:
#'  1) MAP estimates using friction surface
#'  2) Self-reported travel times of bite patients reporting to Moramanga ARMC
#'  3) IPM driving times (from Helene and Luciano)                                                
# ------------------------------------------------------------------------------------------------ #

# Set-up
library(tidyverse)
library(rgdal)
library(raster)
library(lubridate)
library(data.table)
library(doParallel)
library(foreach)
select <- dplyr::select
source('R/functions/out.session.R')
source('R/functions/ttime_functions.R')

# data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
base_times <- raster("output/ttimes/baseline_ttimes.tif")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# Groundtruthing ------------------------------------------------------------------------------

# Mora self reported ttimes
mora <- read.csv("data/processed/bitedata/moramanga_ttimes.csv")
mada_communes$catchment <- ctar_metadata$CTAR[mada_communes$catchment]
mora %>%
  left_join(select(mada_communes@data, commcode, district, from_lat = lat_cent, from_long = long_cent, 
                   commune, pop, ttimes_wtd, ttimes_un, catchment), 
            by = c("commcode" = "commcode")) %>%
  mutate(ttimes_est = ttimes_wtd/60, ttimes_un = ttimes_un/60) %>% 
  filter(catchment == "Moramanga") %>%
  mutate(multiple = rowSums(dplyr::select(., car:Pus)),
         mode = case_when(multiple > 1 ~ "Multiple",
                          car == 1 ~ "Car", 
                          Motorbike == 1 ~ "Motorbike", 
                          foot == 1 ~ "Foot",
                          Bus == 1 ~ "Bus", 
                          Bicycle == 1 ~ "Bicycle", 
                          Pus == 1 ~ "Pus-pus",
                          Other == 1 ~ "Other"),
         to_long = ctar_metadata$LONGITUDE[ctar_metadata$CTAR == "Moramanga"], 
         to_lat  =  ctar_metadata$LATITUDE[ctar_metadata$CTAR == "Moramanga"], 
         ttimes_reported = hours, 
         type = "commune_wtd") %>%
  select(-(car:known_cat1), -ttimes_wtd) -> gtruth_mora 

gtruth_mora$distance <- geosphere::distGeo(cbind(gtruth_mora$to_long, gtruth_mora$to_lat), 
                              cbind(gtruth_mora$from_long, gtruth_mora$from_lat))/1000 # in km

# IPM groundtruthing (ttimes between points) ---------------------------------------------
ttimes_IPM <- read.csv("data/raw/ttimes_IPM.csv")
point_mat_to <- as.matrix(dplyr::select(ttimes_IPM, x = LongAriv, y = LatAriv))
point_mat_from <- as.matrix(dplyr::select(ttimes_IPM, x = LongDep, y = LatDep))

# for each start and end point
# takes ~ 6 seconds per point
cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  foreach(points_to = iter(point_mat_to, by = "row"),
          points_from = iter(point_mat_from, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points_to, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
            extract(ttimes, SpatialPoints(points_from)) 
          } -> ttimes_est
})

stopCluster(cl) 

ttimes_IPM %>%
  mutate(arrival = mdy_hm(paste(Date, Harrival)),
         depart = mdy_hm(paste(Date, Hdepart)),
         ttimes_reported = as.numeric(arrival - depart)/60, 
         ttimes_est = unlist(ttimes_est)/60, 
         type = "point", mode = "Car") %>%
  select(ttimes_est, ttimes_reported, 
         from_lat = LatDep, from_long = LongDep, to_lat = LatAriv, to_long = LongAriv, 
         type, mode) -> gtruth_IPM
gtruth_IPM$distance <- geosphere::distGeo(cbind(gtruth_IPM$to_long, gtruth_IPM$to_lat), 
                                           cbind(gtruth_IPM$from_long, gtruth_IPM$from_lat))/1000 # in km
gtruth_IPM$ttimes_est[is.infinite(gtruth_IPM$ttimes_est)] <- NA

gtruth <- bind_rows(gtruth_IPM, gtruth_mora)

write.csv(gtruth, "output/ttimes/gtruth_ttimes.csv", row.names = FALSE)

# Save session info ---------------------------------------------------------------------------
out.session(path = "R/01_gis/06_groundtruth.R", filename = "output/log_local.csv")

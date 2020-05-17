# ------------------------------------------------------------------------------------------------ #
#' Process clinic data                                                                                       
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()
source("R/functions/out.session.R")

# packages
library(tidyverse)
library(raster)
library(geosphere)
library(rgdal)
select <- dplyr::select

# data
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)

# get clinic commcodes & distcodes
csbs_coords <- SpatialPoints(cbind(csbs$xcoor, csbs$ycoor), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
csbs$commcode <- over(csbs_coords, mada_communes)$commcode
csbs$commune <- over(csbs_coords, mada_communes)$commune
csbs$distcode <- over(csbs_coords, mada_districts)$distcode
csbs$district <- over(csbs_coords, mada_communes)$district

# Process exisiting ctar data -------------------------------------------------------------
ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata%>%
  mutate(commcode = over(ctar_coords, mada_communes)$commcode,
         commune = over(ctar_coords, mada_communes)$commune,
         distcode = over(ctar_coords, mada_districts)$distcode,
         district = over(ctar_coords, mada_districts)$district,
         clinic_id = 1:nrow(ctar_metadata)) %>%
  rename(lat = LATITUDE, long = LONGITUDE) -> ctar_metadata

# Process all csb IIs ------------------------------------------------------------------------
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  select(district, distcode, commune, commcode, lat = ycoor, long = xcoor) -> csb2
dist_mat <- distm(cbind(ctar_metadata$long, ctar_metadata$lat), 
                  cbind(csb2$long, csb2$lat))
csb2 <- csb2[-apply(dist_mat, 1, which.min), ]
csb2$clinic_id <- 1:nrow(csb2) + 31 # id for clinics

write.csv(csb2, "data/processed/clinics/csb2.csv", row.names = FALSE)

# Process additional scenarios -------------------------------------------------------------
pop_1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
csb2_coords <- SpatialPoints(cbind(csb2$long, csb2$lat), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
csb2$pop_dens <- extract(pop_1x1, csb2_coords)

# 1 per district
csb2 %>%
  filter(!(distcode %in% ctar_metadata$distcode)) %>% # filter out any districts with a ctar
  group_by(distcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  filter(clinic_id == min(clinic_id)) -> clinic_per_dist # only pick 1 clinic per district

# 1 per commune for any that don't yet have one
csb2 %>%
  filter(!(commcode %in% c(ctar_metadata$commcode, clinic_per_dist$commcode))) %>% # filter out any communes with a ctar
  group_by(commcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  filter(clinic_id == min(clinic_id)) -> clinic_per_comm

# for those commune that don't have a clinic, look at all the other public csb 1
missing_comms <- mada_communes$commcode[!(mada_communes$commcode %in% clinic_per_comm$commcode)]

csbs %>% 
  filter(type != "CSB1", genre_fs != "Priv") %>%
  select(distcode, district, commcode, commune, lat = ycoor, long = xcoor) %>%
  mutate(clinic_id = 1:nrow(.) + max(csb2$clinic_id)) -> csb1

csb1_coords <- SpatialPoints(cbind(csb1$long, csb1$lat), 
                            proj4string = 
                              CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
csb1$pop_dens <- extract(pop_1x1, csb1_coords)

csb1 %>%
  filter(commcode %in% missing_comms) %>%
  group_by(commcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(clinic_id = max(csb2$clinic_id) + 1:nrow(.)) %>%
  bind_rows(clinic_per_comm) ->  clinic_per_comm

write.csv(clinic_per_comm, "data/processed/clinics/clinic_per_comm.csv", row.names = FALSE)
write.csv(clinic_per_dist, "data/processed/clinics/clinic_per_dist.csv", row.names = FALSE)
write.csv(ctar_metadata, "data/processed/clinics/ctar_metadata.csv", row.names = FALSE)

# Session Info
out.session(path = "R/01_gis/06_process_clinic_data.R", filename = "output/log_local.csv", 
            start = start)


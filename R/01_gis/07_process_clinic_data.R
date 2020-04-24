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
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)

# filter out ctar from csb IIs
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  select(lat = ycoor, long = xcoor) -> csb_ii

dist_mat <- distm(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                  cbind(csb_ii$long, csb_ii$lat))
csb_ii <- csb_ii[-apply(dist_mat, 1, which.min), ]
csb_ii$clinic_id <- 1:nrow(csb_ii) + 31 # id for clinics
write.csv(csb_ii, "data/processed/clinics/csb2.csv", row.names = FALSE)

# get clinic commcodes & distcodes
csb_coords <- SpatialPoints(cbind(csb_ii$long, csb_ii$lat), 
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
csb_ii$commcode <- over(csb_coords, mada_communes)$commcode
csb_ii$distcode <- over(csb_coords, mada_districts)$distcode

ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(ctar_coords, mada_communes)$commcode
ctar_metadata$distcode <- over(ctar_coords, mada_districts)$distcode
ctar_metadata$clinic_id <- 1:nrow(ctar_metadata)

# get 1 per district with the highest pop density
pop_1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
csb_ii$pop_dens <- extract(pop_1x1, csb_coords)

csb_ii %>%
  filter(!(distcode %in% ctar_metadata$distcode)) %>% # filter out any districts with a ctar
  group_by(distcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  filter(clinic_id == min(clinic_id)) -> clinic_per_dist # only pick 1 clinic per district

# get 1 per commune with the higher pop density
csb_ii %>%
  filter(!(commcode %in% ctar_metadata$commcode)) %>% # filter out any communes with a ctar
  group_by(commcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  filter(clinic_id == min(clinic_id)) -> clinic_per_comm

# bind to existing ctar points  
ctar_metadata %>%
  select(lat = LATITUDE, long = LONGITUDE, distcode, commcode) %>%
  mutate(pop_dens = extract(pop_1x1, ctar_coords), 
         clinic_id = 1:nrow(.)) -> ctar_to_bind

clinic_per_dist <- bind_rows(clinic_per_dist, ctar_to_bind)
clinic_per_comm <- bind_rows(clinic_per_comm, ctar_to_bind)  

# for those that commune that don't have a clinic, look at all the other public clinics
missing_comms <- mada_communes$commcode[!(mada_communes$commcode %in% clinic_per_comm$commcode)]

csbs %>% 
  filter(type != "CSB1", genre_fs != "Priv") %>%
  select(lat = ycoor, long = xcoor) %>%
  mutate(clinic_id = 1:nrow(.) + max(csb_ii$clinic_id)) -> csb_i

csb_coords <- SpatialPoints(cbind(csb_i$long, csb_i$lat), 
                            proj4string = 
                              CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

csb_i$commcode <- over(csb_coords, mada_communes)$commcode
csb_i$distcode <- over(csb_coords, mada_districts)$distcode
csb_i$pop_dens <- extract(pop_1x1, csb_coords)

csb_i %>%
  filter(commcode %in% missing_comms) %>%
  group_by(commcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  filter(clinic_id == min(clinic_id)) %>%
  bind_rows(clinic_per_comm) ->  clinic_per_comm
  
write.csv(clinic_per_comm, "data/processed/clinics/clinic_per_comm.csv", row.names = FALSE)
write.csv(clinic_per_dist, "data/processed/clinics/clinic_per_dist.csv", row.names = FALSE)

write.csv(ctar_metadata, "data/processed/clinics/ctar_metadata.csv", row.names = FALSE)

# Session Info
out.session(path = "R/01_gis/07_process_clinic_data.R", filename = "output/log_local.csv", 
            start = start)


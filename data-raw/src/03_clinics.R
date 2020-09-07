# ------------------------------------------------------------------------------------------------ #
#' Process clinic data                                                                                       
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# packages
library(tidyverse)
library(raster)
library(sf)
select <- dplyr::select
source(here("R", "utils.R"))
source(safe_path("R/out.session.R"))

# data
mada_communes <- st_read(safe_path("data-raw/out/shapefiles/mada_communes.shp"))
ctar_metadata <- read.csv(safe_path("data-raw/raw/ctar_metadata.csv"))
csbs <- read.csv(safe_path("data-raw/raw/ipm_data/csbs.csv"), stringsAsFactors = FALSE)

# get clinic commcodes & distcodes
csbs <- st_as_sf(csbs, coords = c("xcoor", "ycoor"), 
                     crs = st_crs(mada_communes))
csbs <- st_join(csbs, 
                select(mada_communes, commcode, distcode, district, commune), 
                join = st_intersects)

# Process exisiting ctar data -------------------------------------------------------------
ctar_metadata <- st_as_sf(ctar_metadata, coords = c("LONGITUDE", "LATITUDE"), 
                          crs = st_crs(mada_communes))
ctar_metadata <- st_join(ctar_metadata, 
                         select(mada_communes, commcode, distcode, district, commune), 
                         join = st_intersects)

ctar_metadata%>%
  mutate(clinic_id = 1:nrow(ctar_metadata)) -> ctar_metadata

# Process all csb IIs ------------------------------------------------------------------------
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  select(district, distcode, commune, commcode) -> csb2
dist_mat <- st_distance(ctar_metadata, csb2)
csb2 %>% 
  slice(-c(apply(dist_mat, 1, which.min))) %>%
  mutate(clinic_id = 1:nrow(.) + 31, 
         long = st_coordinates(.)[, "X"], 
         lat = st_coordinates(.)[, "Y"]) -> csb2

write.csv(st_drop_geometry(csb2), safe_path("data-raw/out/clinics/csb2.csv"), row.names = FALSE)

# Process additional scenarios -------------------------------------------------------------
pop_1x1 <- raster(safe_path("data-raw/out/rasters/wp_2015_1x1.tif"))
csb2$pop_dens <- extract(pop_1x1, csb2[, c("long", "lat")])

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
missing_comms <- mada_communes$commcode[!(mada_communes$commcode %in% clinic_per_comm$commcode |
                                          mada_communes$commcode %in% ctar_metadata$commcode |
                                          mada_communes$commcode %in% clinic_per_dist$commcode)]
csbs %>% 
  filter(type != "CSB1", genre_fs != "Priv") %>%
  select(distcode, district, commcode, commune) %>%
  mutate(clinic_id = 1:nrow(.) + max(csb2$clinic_id)) -> csb1

csb1$pop_dens <- extract(pop_1x1, st_coordinates(csb1))

csb1 %>%
  filter(commcode %in% missing_comms) %>%
  group_by(commcode) %>%
  filter(pop_dens == max(pop_dens, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(clinic_id = max(csb2$clinic_id) + 1:nrow(.), 
         long = st_coordinates(.)[, "X"], 
         lat = st_coordinates(.)[, "Y"]) %>%
  bind_rows(clinic_per_comm) ->  clinic_per_comm

write_create(st_drop_geometry(clinic_per_comm),
             safe_path("data/out/clinics/clinic_per_comm.csv"), 
             write.csv,
             row.names = FALSE)
write_create(st_drop_geometry(clinic_per_dist), 
             safe_path("data/out/clinics/clinic_per_dist.csv"), 
             write.csv, 
             row.names = FALSE)
write_create(st_drop_geometry(ctar_metadata), 
             safe_path("data/out/clinics/ctar_metadata.csv"), 
             write.csv,
             row.names = FALSE)

# Session Info
out.session(path = "data-raw/src/03_clinics.R", 
            filename = safe_path("analysis/logs/log_local.csv"), 
            start = start)


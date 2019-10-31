####################################################################################################
##' Step 2: Getting scenario for additional clinics
##' Details: Take the access matrices (admin units to each clinic) and rank clinics by how much
##' they improve access for a population living greater than a threshold distance or travel time away
##' Author: Malavika Rajeev 
####################################################################################################

##' Set-up 
##' ------------------------------------------------------------------------------------------------
rm(list = ls())

##' Libraries
library(doRNG)
library(foreach)
library(dplyr)
select <- dplyr::select
source("R/functions/access_functions.R")

## Read in data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
district_ttimes_candidates <- read.csv("data/processed/catchmats/candidates_district_ttimes_weighted.csv", 
                                       row.names = 1)
commune_ttimes_candidates <- read.csv("data/processed/catchmats/candidates_commune_ttimes_weighted.csv", 
                                      row.names = 1)

## candidate points
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, lat = ycoor, long = xcoor) -> csbs
csbs$unique_id <- 1:nrow(csbs)

##' Commune scenarios
commune_ttimes_weighted <- add.armc(base_metric = mada_communes$ttms_wtd, 
                                    clinic_names = csbs$unique_id, 
                                    clinic_catchmat = commune_ttimes_candidates, 
                                    prop_pop = mada_communes$pop/sum(mada_communes$pop), 
                                    max_clinics = ncol(commune_ttimes_candidates),
                                    threshold = 3*60)
write.csv(commune_ttimes_weighted, "output/scenarios/scenario_commune_ttmswtd.csv")

##' District scenarios
district_ttimes_weighted <- add.armc(base_metric = mada_districts$ttms_wtd, 
                                     clinic_names = colnames(district_ttimes_candidates), 
                                     clinic_catchmat = district_ttimes_candidates, 
                                     prop_pop = mada_districts$pop/sum(mada_districts$pop), 
                                     max_clinics = ncol(district_ttimes_candidates), threshold = 3*60)
write.csv(district_ttimes_weighted, "output/scenarios/scenario_district_ttmswtd.csv")

## Testing figures
gg_communes <- fortify(mada_communes, region = "ADM3_PCODE")
gg_communes %>%
  left_join(mada_communes@data, by = c("id" = "ADM3_PCODE")) -> gg_communes
gg_districts <- fortify(mada_districts, region = "distcode") %>%
  left_join(mada_districts@data, by = c("id" = "distcode")) -> gg_districts

ggplot() +
  geom_polygon(data = gg_districts, aes(x = long.x, y = lat.x, group = group, fill = ttms_wtd/60), 
               color = NA) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), shape = 4, stroke = 2, 
             color = "lightgrey") +
  scale_fill_gradient(low = "white", high = "#c32148") +
  theme_void()

## Getting ctar district and commune
pts <- SpatialPoints(cbind(csbs$long, csbs$lat), 
                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
csbs$commune <- over(pts, mada_communes)$ADM3_PCODE
csbs$district <- over(pts, mada_communes)$distcode

commune_ranks <- data.frame(clinics = colnames(commune_ttimes_weighted), 
                            commune_rank = 1:ncol(commune_ttimes_weighted))
district_ranks <- data.frame(clinics = colnames(district_ttimes_weighted), 
                             district_rank = 1:ncol(district_ttimes_weighted))

csbs %>%
  mutate(clinics = gsub(" ", ".", CTAR)) %>%
  left_join(commune_ranks) %>%
  left_join(district_ranks) -> csbs_ranked

csbs_ranked %>%
  group_by(district) %>%
  summarize(rank = min(district_rank, na.rm = TRUE)) %>%
  right_join(gg_districts, by = c("district" = "id")) -> gg_districts

ggplot() +
  geom_polygon(data = gg_districts, aes(x = long.x, y = lat.x, group = group, fill = rank), 
               color = NA) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), shape = 4, stroke = 2, 
             color = "black") +
  scale_fill_gradient(low = "white", high = "#FEE1D9") +
  theme_void()

csbs_ranked %>%
  group_by(commune) %>%
  summarize(rank = min(commune_rank, na.rm = TRUE)) %>%
  right_join(gg_communes, by = c("commune" = "id")) -> gg_communes
 

opts <- c("#AFD1E3", "#A6CFC8", "#9CCDAD", "#EDCB46", "#EDCB46", "#FFA500", 
          "#FF8D00", "#FF6400", "#FF4500", "#FC2B1C")
  
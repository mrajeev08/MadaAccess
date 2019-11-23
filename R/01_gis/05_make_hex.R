####################################################################################################
##' Making hexagonal 
##' Details: ran on cluster to save time on computer
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
Sys.time()
rm(list = ls())

##' Packages
library(rgdal) # for shapefiles (also comes with sp)
library(raster) # for rasters and resampling
library(geogrid)
library(data.table)
library(geosphere)
library(rgeos)
library(spdep)
library(tidyverse)
library(sf)
source("R/functions/utils.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

check <- mada_communes[mada_communes$distcode == levels(mada_communes$distcode)[1], ]
check_1 <- calculate_grid(shape = check, grid_type = "hexagonal", seed = 4)
check_1 <- check_1[[2]]
check_1_area <- check_1@polygons[[1]]@area

check <- mada_communes[mada_communes$distcode == levels(mada_communes$distcode)[2], ]
check_2 <- calculate_grid(shape = check, grid_type = "hexagonal", seed = 4)
check_2 <- check_2[[2]]
check_2_area <- check_2@polygons[[1]]@area


##' Communes manually assigned
communes_hex <- readOGR("data/processed/shapefiles/mada_communes_hex.shp")
communes_hex@data %>% group_by(distcode) %>% summarize(ncomms_man = n()) -> check
mada_communes@data %>% group_by(distcode) %>% summarize(ncomms = n()) -> ncomms
ncomms %>%
  left_join(check) -> check
check %>% mutate(diff = ncomms - ncomms_man) -> check

## For each district (assign communes within by directionality from centroid)

## Commune
## Check the potentials, here seed 3 looks the best
# for (i in 1:6) {
#   new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }

comm_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = 5)
cell_pts <- comm_cells[[1]]

## Assigning cells to communes
## Inputs are district distance matrix
district_pts <- gCentroid(mada_districts, byid = TRUE)
district_pts$distcode <- mada_districts$distcode

## Count of communes in each district
mada_communes@data %>%
  group_by(distcode) %>%
  summarize(ncomms = n()) -> dist_comms
dist_comms <- as.data.table(dist_comms)

## Adjacency data frame for comm cells
adj_mat <- poly2nb(comm_cells[[2]], row.names = 1:1579)
adj_mat <- nb2mat(adj_mat, style = "B", zero.policy = TRUE)
colnames(adj_mat) <- rownames(adj_mat)

## Doesnt't really work, need to assign edges 1st
## Set order to assigning for districts

## If you want to start over start here
adj_pairs <- melt(data.table(cell_id = rownames(adj_mat), adj_mat), id.vars = "cell_id")
colnames(adj_pairs) <- c("from", "to", "adj")
adj_pairs <- adj_pairs[adj != 0]

## District distance matrix
dist_mat <- distm(district_pts)
rownames(dist_mat) <- colnames(dist_mat) <- mada_districts$distcode

## Degree matrix for districts
bearing_mat <- apply(district_pts@coords, 1, function(x) bearing(x, district_pts@coords))
rownames(bearing_mat) <- colnames(bearing_mat) <- mada_districts$distcode

## Cell to district distance matrix
cell_to_dist <- distm(cell_pts, district_pts)
colnames(cell_to_dist) <- mada_districts$distcode
cell_to_dist <- data.table(to = as.factor(1:1579), cell_to_dist)
adj_pairs <- adj_pairs[cell_to_dist, on = "to"]

## Seed cell and district id
seed_dist <- "MG51507"
seed_cell <- 39

out_dt <- data.table(distcode = NA, cell_id = NA)
## Loop to assign cells
while(nrow(adj_pairs) > 0) {
  adj_now <- adj_pairs[from == seed_cell]
  ncells <- dist_comms[distcode == seed_dist]$ncomms
  ids <- ids_added <- unique(adj_now$to) 
  
  while(ncells - length(ids) > 0) {
    adj_now <- adj_pairs[from %in% ids_added]
    ids_added <- unique(adj_now$to)
    ids <- c(ids, ids_added)
  }
  
  ids <- c(seed_cell, ids)
  ids <- ids[1:ncells] ## so as not to get more than you need
  adj_pairs <- adj_pairs[!(to %in% ids)] ## get rid of already assigned ones as options
  
  ## pick new seed distcode (closest district to current dist seed)
  seed_dist_old <- seed_dist
  seed_dist <- names(which.min(dist_mat[seed_dist, colnames(dist_mat) != seed_dist]))
  ## Remove old district from dist mat
  dist_mat <- dist_mat[rownames(dist_mat) != seed_dist_old, colnames(dist_mat) != seed_dist_old]
  
  ## pick new seed cell id
  ## get bearings to candidate cell ids
  cands <- adj_pairs[from %in% ids]
  if(nrow(cands) == 0) {
    cands <- adj_pairs
    cands$distance <- adj_pairs[, colnames(adj_pairs) == seed_dist, with = FALSE] 
    ## pick ones closest to new district? or to old district?
    setorder(cands, distance)
    cands <- cands[1:10, ]
  }
  
  ref_bearing <- bearing_mat[seed_dist_old, seed_dist]
  bearing_mat <- bearing_mat[rownames(bearing_mat) != seed_dist_old, 
                             colnames(bearing_mat) != seed_dist_old]
  
  bearing_cands <- bearing(district_pts[district_pts$distcode == seed_dist_old, ], 
                           cell_pts[cands$to])
  
  seed_cell <- cands$to[which.min(abs(bearing_cands - ref_bearing))] 
  
  print(seed_dist)
  
  ## Make data frame and rbind
  now_dt <- data.table(distcode = seed_dist_old, cell_id = ids)
  out_dt <- rbind(out_dt, now_dt)
}

comm_hex <- comm_cells[[2]]
comm_hex$cell_id <- as.character(1:1579)
gg_comm <- fortify(comm_hex, region = "cell_id")
out_dt$cell_id <- as.character(out_dt$cell_id)
gg_comm <- left_join(gg_comm, out_dt, by = c("id" = "cell_id"))

ggplot(data = gg_comm, aes(x = long, y = lat, group = group, fill = distcode)) +
  geom_polygon() +
  scale_fill_discrete(guide = "none")

comm_hex@data <- left_join(comm_hex@data, out_dt)

writeOGR(comm_hex, dsn = "data/processed/shapefiles", layer = "test_it", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

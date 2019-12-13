###################################################################################################
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
source("R/functions/utils.R")
select <- dplyr::select

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")

## Getting ctar district and commune
ctar_metadata$ctar_dist <- mada_districts$distcode[match(ctar_metadata$District, 
                                                         mada_districts$district)]
pts <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                     proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$ctar_comm <- over(pts, mada_communes)$commcode
mada_districts$ctar_in_dist <- ifelse(mada_districts$distcode %in% ctar_metadata$ctar_dist, 1, 0)
mada_communes$ctar_in_comm <- ifelse(mada_communes$commcode %in% ctar_metadata$ctar_comm, 1, 0)

## Hexagonal maps
## Check the potentials, here seed 1 looks the best
# for (i in 1:6) {
#   new_cells <- calculate_grid(shape = mada_districts, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }
new_cells <- calculate_grid(shape = mada_districts, grid_type = "hexagonal", seed = 1)
district_hex <- assign_polygons(mada_districts, new_cells)
writeOGR(district_hex, dsn = "data/processed/shapefiles", layer = "mada_districts_hex", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

## Commune
## Check the potentials, here seed 3 looks the best
# for (i in 1:6) {
#   new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }
new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = 5)

## cartogram of districts
mada_communes@data %>%
  group_by(distcode) %>%
  summarize(ncommunes = n()) %>%
  right_join(mada_districts@data) %>%
  as.data.frame(.) -> mada_districts@data
dist_cart <- cartogram::cartogram_cont(mada_districts, weight = "ncommunes", itermax = 10)

comm_hex <- calculate_grid(shape = dist_cart, grid_type = "hexagonal", 
                           npts = length(mada_communes), seed = 5, verbose = TRUE)
comm_to_assign <- comm_hex[[2]]
hexes <- over(comm_to_assign, dist_cart)
hexes$hex_id <- 1:nrow(hexes)
hexes %>%
  group_by(distcode) %>%
  mutate(hex_comms = n()) -> hexes

library(spdep)
hex_adj <- poly2nb(comm_to_assign)
hex_adj <- data.frame(hex_id = rep(1:length(hex_adj), lapply(hex_adj, length)), 
                     adj_id = simplify(hex_adj))

## Take ones with more than necessary and then assign them to adjacent pols (no matter what)
## Do this iteratively until you reach a plateau where you're switching back and forthish
hexes <- over(comm_to_assign, dist_cart)
hexes$hex_id <- 1:nrow(hexes)
hexes %>%
  group_by(distcode) %>%
  mutate(hex_comms = n()) -> hexes
last <- 1000 ## placeholder for first loop step
counter <- 0
repeat{
  hex_adj %>%
    left_join(select(hexes, hex_id, hex_distcode = distcode, hex_comms, 
                     true_comms_hex = ncommunes)) %>%
    left_join(select(hexes, hex_id, adj_distcode = distcode, adj_comms = hex_comms, 
                     true_comms_adj = ncommunes), 
              by = c("adj_id" = "hex_id")) %>%
    mutate(hex_diff =  hex_comms - true_comms_hex,
           adj_diff = adj_comms - true_comms_adj,
           unique_id = interaction(hex_id, adj_id)) -> hex_df
  
  distcodes <- unique(hex_df$adj_distcode)
  all_changed <- data.frame(hex_id = NA, hex_diff = NA, hex_distcode = NA, group_id = NA)
  
  for(i in 1:length(distcodes)) {
    to_assign <- filter(hex_df, hex_distcode != adj_distcode & hex_diff > 0 & adj_diff < 0, 
                        hex_distcode == distcodes[i])
    if(nrow(to_assign) > 0) {
      to_add <- to_assign$hex_diff[1]
      to_assign %>% 
        select(hex_id, adj_id, hex_diff, adj_diff, adj_distcode) %>%
        distinct(.) %>%
        group_by(adj_distcode) %>%
        mutate(group_id = 1:n()) %>%
        filter(group_id <= abs(adj_diff)) -> cands
      to_add <- ifelse(nrow(cands) < to_add, nrow(cands), to_add)
      cands <- cands[1:to_add, ]
      cands %>%
        mutate(hex_distcode = as.character(adj_distcode)) -> cands
      hex_df <- filter(hex_df, !(as.character(hex_id) %in% as.character(cands$hex_id))) # filter out those already assigned
      all_changed <- bind_rows(cands, all_changed)
    } else
      next
  }
  
  newcode <- all_changed$hex_distcode[match(hexes$hex_id, all_changed$hex_id)]
  hexes$distcode <- coalesce(newcode, as.character(hexes$distcode)) 
  hexes %>%
    group_by(distcode) %>%
    mutate(hex_comms = n()) -> hexes
  hexes$diff <- hexes$hex_comms - hexes$ncommunes
  hist(hexes$diff)
  
  if((nrow(all_changed) - 1) > last) {
    break
  } else {
    last <- (nrow(all_changed) - 1)
    counter <- counter + 1
  }
  print(last)
  print(counter)
}

## No more options, so for any that have more than n polygons assign those to adjacent districts and
## do this iteratively until difference between actual and total sums to zero!
counter <- 0
 

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

## Graph matching (try it and see, with first 5 matched?)
library(igraph)
library(spdep)
hex_shp <- new_cells[[2]]
hex_adj <- poly2nb(hex_shp)
hex_adj <- nb2mat(hex_adj, style = "B")
pol_adj <- poly2nb(mada_communes, row.names = mada_communes$commcode)
pol_adj <- nb2mat(pol_adj, style = "B", zero.policy = TRUE)

## Seeding Tana as known polygons
# seed <- matrix(0, nrow = 6, ncol = ncol(hex_adj))
# seed[1, 966] <- 1
# seed[2, 967] <- 1
# seed[3, 968] <- 1
# seed[4, 995] <- 1
# seed[5, 996] <- 1
# seed[6, 997] <- 1

hex_adj <- rbind(hex_adj[c(966:968, 995:997), ], hex_adj[-c(966:968, 995:997), ])
hex_adj <- cbind(hex_adj[, c(966:968, 995:997)], hex_adj[, -c(966:968, 995:997)])
rownames(hex_adj) <- colnames(hex_adj) <- c(c(966:968, 995:997), 
                                            (1:nrow(hex_adj))[-c(966:968, 995:997)])

## Graph matching
check <- match_vertices(hex_adj, pol_adj, m = 6, start = diag(rep(1, nrow(hex_adj) - 6)), 
                        iteration = 200)
matches <- check[[1]]
matches <- cbind(rownames(hex_adj)[matches[, 1]], rownames(pol_adj)[matches[, 2]])
matched <- cbind(c(966:968, 995:997), rownames(pol_adj)[1:6])
matches <- rbind(matched, matches)
matches <- data.frame(matches)
hex_shp$hex_id <- 1:length(hex_shp)

mada_communes@data %>%
  left_join(matches, by = c("commcode" = "X2")) %>%
  mutate(hex_id = as.numeric(as.character(X1))) %>%
  right_join(hex_shp@data) -> hex_shp@data

writeOGR(hex_shp, dsn = "data/processed/shapefiles", layer = "graphed", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

## Use centroids and bearings to assign communes by district for the pick_one_dist file (also double
## check right number of communes per district!)


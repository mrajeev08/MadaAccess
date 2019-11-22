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
ref_against <- function(x, base_ref) {
  check <- ifelse(x > base_ref, NA, x)
  min(check, na.rm = TRUE)
} 

library(geosphere)
library(rgeos)
new_cells <- calculate_grid(shape = mada_districts, grid_type = "hexagonal", seed = 1)
# shape_pts <- coordinates(mada_districts)
shape_pts <- gCentroid(mada_districts, byid = TRUE)
cell_pts <- new_cells[[1]]

dist_mat <- distm(shape_pts, cell_pts)
colnames(dist_mat) <- 1:114
rownames(dist_mat) <- mada_districts$distcode
hex_id <- apply(dist_mat, 1, which.min)
base_ref <- apply(dist_mat, 1, min)
merge_df <- data.table(hex_id = 1:114)
match_df <- data.table(distcode = rownames(dist_mat), hex_id, base_ref)
match_df <- match_df[, .SD[base_ref == min(base_ref)], by = hex_id]
match_df <- match_df[merge_df, on = "hex_id"] ## NAs for ones with no matches or multiple matches
match_df$base_ref[is.na(match_df$base_ref)] <- Inf
setorder(match_df, hex_id)
dist_mat_now <- dist_mat[!(rownames(dist_mat) %in% match_df$distcode), ] # Only rows without a unique match
print(nrow(dist_mat_now))

while(!is.null(dim(dist_mat_now))) {
  updated <- apply(dist_mat_now, 1, ref_against, base_ref = match_df$base_ref)
  check <- which(dist_mat_now == updated, arr.ind = TRUE)
  check_base <- which(dist_mat_now == updated)
  updated <- data.table(distcode = rownames(check), hex_id = check[, 2], 
                        base_ref = dist_mat_now[check_base])
  match_df <- rbind(match_df, updated)
  match_df <- match_df[, .SD[base_ref == min(base_ref)], by = hex_id]
  dist_mat_now <- dist_mat[!(rownames(dist_mat) %in% match_df$distcode), ] # Only rows without a unique match
  print(nrow(dist_mat_now))
}

match_df$distcode[is.na(match_df$distcode)] <- rownames(dist_mat)[!(rownames(dist_mat) 
                                                                         %in% match_df$distcode)]
shape_cells <- new_cells[[2]]
shape_cells$hex_id <- 1:length(shape_cells)
shape_cells$distcode <- match_df$distcode[match(shape_cells$hex_id, merge_df$hex_id)]

## Compare to
shape_cells@data <- dplyr::left_join(shape_cells@data, mada_districts@data)
district_hex <- assign_polygons(mada_districts, new_cells)
writeOGR(shape_cells, dsn = "data/processed/shapefiles", layer = "mada_districts_hex_check", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(district_hex, dsn = "data/processed/shapefiles", layer = "mada_districts_hex", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

## Commune
## Check the potentials, here seed 3 looks the best
# for (i in 1:6) {
#   new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }

new_cells <- calculate_grid(shape = mada_communes, grid_type = "hexagonal", seed = 5)
shape_pts <- coordinates(mada_communes)
cell_pts <- new_cells[[1]]

dist_mat <- distm(shape_pts, cell_pts)
colnames(dist_mat) <- 1:length(mada_communes)
rownames(dist_mat) <- mada_communes$commcode
hex_id <- apply(dist_mat, 1, which.min)
unique <- names(table(hex_id)[table(hex_id)  == 1])
hex_left <- names(table(hex_id)[table(hex_id) > 1])
check <- rep(NA, length(hex_left))
for (i in 1:length(hex_left)) {
  check[i] <- names(which.min(dist_mat[hex_id == hex_left[i], hex_left[i]]))
}
hex_id[hex_id %in% hex_left] <- NA
hex_id[check] <- hex_left
dist_mat_now <- dist_mat[is.na(hex_id), !(colnames(dist_mat) %in% hex_id)] # Only rows without a unique match

while(!is.null(dim(dist_mat_now))) {
  ## Should always be symmetrical (ncols = nrows)
  hex_id_next <- colnames(dist_mat_now)[apply(dist_mat_now, 1, which.min)]
  names(hex_id_next) <- rownames(dist_mat_now)
  
  unique <- names(table(hex_id_next)[table(hex_id_next)  == 1])
  hex_left <- names(table(hex_id_next)[table(hex_id_next) > 1])
  
  check <- rep(NA, length(hex_left))
  for (i in 1:length(hex_left)) {
    check[i] <- names(which.min(dist_mat_now[hex_id_next == hex_left[i], hex_left[i]]))
  }
  hex_id[names(hex_id) %in% check] <- hex_left
  dist_mat_now <- dist_mat[is.na(hex_id), !(colnames(dist_mat) %in% hex_id)] # Only rows without a unique match
  print(nrow(dist_mat_now))
}
hex_id[is.na(hex_id)] <- colnames(dist_mat)[!(colnames(dist_mat) %in% hex_id)]
merge_df <- data.frame(commcode = names(hex_id), row_id = hex_id)
shape_cells <- new_cells[[2]]
shape_cells$row_id <- 1:length(shape_cells)
shape_cells$commcode <- merge_df$commcode[match(shape_cells$row_id, merge_df$row_id)]
shape_cells@data <- dplyr::left_join(mada_communes@data, shape_cells@data)

writeOGR(shape_cells, dsn = "data/processed/shapefiles", layer = "mada_communes_hex", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

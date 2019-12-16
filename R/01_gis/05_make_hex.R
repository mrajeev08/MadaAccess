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
mada_districts$weight <- mada_districts$ncommunes*1000
dist_cart <- cartogram::cartogram_cont(mada_districts, weight = "weight", itermax = 10)
mada_communes$weight <- 1000
comm_cart <- cartogram::cartogram_cont(mada_communes, weight = "weight", itermax = 4)

comm_hex <- calculate_grid(shape = dist_cart, grid_type = "hexagonal", 
                           npts = length(mada_communes), seed = 3, verbose = TRUE)
library(foreach)
foreach(i = 1:length(levels(dist_cart$distcode)), .combine = bind) %do% {
  dist_shp <- dist_cart[dist_cart$distcode == levels(dist_cart$distcode)[i], ]
  print(paste("district #", i, "with", dist_shp$ncommunes, "communes"))
  if(dist_shp$ncommunes > 1) {
    dist_hex <- calculate_grid(shape = dist_shp, grid_type = "regular",
                               npts = dist_shp$ncommunes, verbose = FALSE)
    dist_pts <- dist_hex[[1]]
  } else {
    dist_pts <- rgeos::gCentroid(dist_shp)
  }
  dist_pts$distcode <- rep(levels(dist_cart$distcode)[i], length(dist_pts))
  dist_pts
} -> pts_to_match

library(clue)
hex_pts <- comm_hex[[1]]
hex_shp <- comm_hex[[2]]
distmatrix <- sp::spDists(hex_pts, pts_to_match, longlat = FALSE)
# apply hungarian algorithm
sol <- clue::solve_LSAP(distmatrix)
hex_shp$distcode <- pts_to_match$distcode[sol[seq_along(sol)]]
writeOGR(hex_shp,  dsn = "data/processed/shapefiles", layer = "hex_check", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
hex_pts <- data.frame(long = hex_pts@coords[, 1], lat = hex_pts@coords[, 2])
hex_pts$id <- 1:nrow(hex_pts)
hex_pts$name <- hex_shp$distcode
hex_pts %>%
  arrange(long) %>%
  mutate(q = as.numeric(factor(long))-1) %>%
  arrange(lat) %>%
  mutate(r = as.numeric(factor(lat))-1) -> hex_csv 
write.csv(hex_csv, "hex.csv", row.names = FALSE)  

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
 
dist_shp <- mada_communes[mada_communes$distcode == levels(mada_districts$distcode)[1], ]

dist_hex <- calculate_grid(shape = dist_shp, grid_type = "regular", verbose = TRUE)
plot(dist_hex, add = TRUE)


check <- raster(dist_hex[[2]])
res(check) <- 500
r <- rasterize(dist_hex[[2]], check)

## Last option is to assign coastal polygons and then to move inwards based on adjacency (in a clockwise way for example)
## Probably need to seed start one and then move around

## Then manually fix based on adjacency

## Trying other continous cartograms (rectmap & Rcartogram)
mada_communes <- mada_communes[!(mada_communes$district %in% c("Sainte Marie", "Nosy-Be")), ]
coords <- rgeos::gCentroid(mada_communes, byid = TRUE)
commareas <- area(mada_communes)/1e6
mada <- data.frame(x = coords@coords[, "x"], 
                  y = coords@coords[, "y"], 
                  # make the rectangles overlapping by correcting 
                  # lines of longitude distance.
                  dx = sqrt(commareas) / 2 
                  / (0.8 * 60 * cos(coords@coords[, "y"] * pi / 180)), 
                  dy = sqrt(commareas) / 2 / (0.8 * 60), 
                  z = 1,
                  name = mada_communes$commcode)
madarec <- recmap(mada)
plot(madarec)

# define a fitness function
recmap.fitness <- function(idxOrder, Map, ...){
  Cartogram <- recmap(Map[idxOrder, ])
  # a map region could not be placed; 
  # accept only feasible solutions!
  if (sum(Cartogram$topology.error == 100) > 0){return (0)}
  1 / sum(Cartogram$z / (sqrt(sum(Cartogram$z^2))) 
          * Cartogram$relpos.error)
}


recmapGA <- ga(type = "permutation", 
               fitness = recmap.fitness, 
               Map = madarec,
               monitor = gaMonitor,
               min = 1, max = nrow(madarec) , 
               popSize = 4 * nrow(madarec), 
               maxiter = 10,
               run = 100, 
               parallel=TRUE,
               pmutation = 0.25)

## Not run: 

## use Genetic Algorithms (GA >=3.0.0) as metaheuristic

M <- US.Map

recmapGA <- ga(type = "permutation", 
               fitness = recmap.fitness, 
               Map = M,
               monitor = gaMonitor,
               min = 1, max = nrow(M) , 
               popSize = 4 * nrow(M), 
               maxiter = 10,
               run = 100, 
               parallel=TRUE,
               pmutation = 0.25)

library(voronoi)
check <- voronoiShapefile(lon = coords@coords[, "x"], lat = coords@coords[, "y"], shp = mada_communes)
check$weight <- 100/log(area(check)/1e6)
comm_cart <- cartogram::cartogram_cont(check, weight = "weight", itermax = 7)
mada_communes$id <- 1:length(mada_communes)
library(dplyr)
comm_cart@data %>%
  left_join(mada_communes@data, by = c("id" = "id")) -> comm_cart@data
writeOGR(comm_cart, dsn = "data/processed/shapefiles", layer = "comm_cart", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)


voro_pts <- rgeos::gCentroid(comm_cart, byid = TRUE)
comm_hex <- calculate_grid(shape = comm_cart, grid_type = "hexagonal", seed = 3, verbose = TRUE)
distmatrix <- sp::spDists(comm_hex[[1]], voro_pts, longlat = FALSE)
# apply hungarian algorithm
sol <- clue::solve_LSAP(distmatrix)
hex_shp <- comm_hex[[2]]
hex_shp$distcode <- comm_cart$distcode[sol[seq_along(sol)]]
writeOGR(hex_shp, dsn = "data/processed/shapefiles", layer = "comm_hex", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################################################################################################
##' Getting travel times for each grid cell for each clinic
##' Details: getting minimum travel time estimate for each clinic
##'   Code should be run in parallel and final dataset requires a lot of memory! ~ 10 GB
##'   Split up clinics or work with fewer clinics if there are memory limitations
##'   With three cores, it takes approximately 120 minutes on MacOS with 16 GB 1867 MHz DDR3 and
##'   2.9 GHz Intel Core i5
##' Author: Malavika Rajeev
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
Sys.time()
rm(list = ls())

##' Libraries
library(rgdal)
library(raster)
library(foreach)
library(tidyverse)
library(gdistance)
library(iterators)
library(doParallel)
library(data.table)

##' Source
source("R/functions/utils.R")
source("R/functions/access_functions.R")

##' Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
pop1x1<- raster("data/processed/rasters/worldpop2015adj_mada_1x1km.tif")
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

## candidate points
csbs <- read.csv("data/raw/csbs.csv", stringsAsFactors = FALSE)
csbs %>% 
  filter(type == "CSB2", genre_fs != "Priv", type_fs != "Health Post") %>%
  dplyr::select(CTAR = nom_fs, X_COORD = ycoor, Y_COORD = xcoor) -> csbs

point_mat_candidates <- as.matrix(select(csbs, Y_COORD, X_COORD))
candidate_ids <- 1:nrow(csbs) + 31 ## above the baseline 31 clinics (number by rows!)
baseline_df <- fread("output/baseline.csv")

## Do the candidates
cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  foreach(points = iter(point_mat_candidates, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
          } -> stacked_ttimes
})

stopCluster(cl) ## for doParallel
## 6 seconds per point

## stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ]

fwrite(stacked_ttimes, "output/candidate_matrix.csv")
write.csv(candidate_ids, "output/candidate_ids.csv")

# ------------------------------------------------------------------------------------------------ #
#' Getting travel times for each grid cell for each clinic
#' Code should be run in parallel and final dataset requires a lot of memory! ~ 10 GB
#' Split up clinics or work with fewer clinics if there are memory limitations
#'   with three cores, it takes approximately 120 minutes on MacOS with 16 GB 1867 MHz DDR3 and
#'   2.9 GHz Intel Core i5
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-sn -t 12 -n 18 -mem 4500 -sp "./R/04_addclinics/01_ttimes_candidates.R" -jn candidates -wt 5m -n@
  
# set up cluster on single node with do Parallel
library(doParallel)
cl <- makeCluster(18)
registerDoParallel(cl)
start <- Sys.time()

# Libraries
library(rgdal)
library(raster)
library(foreach)
library(tidyverse)
library(gdistance)
library(iterators)
library(data.table)

# Source scripts
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# candidate points
# filter out existing csbs and also write out to file!
csbs <- read.csv("data/processed/clinics/csb2.csv")
point_mat_candidates <- as.matrix(select(csbs, long, lat))
baseline_df <- fread("output/ttimes/baseline_grid.gz")

system.time ({
  foreach(points = iter(point_mat_candidates, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
          } -> stacked_ttimes
})

# stack it
stacked_ttimes <- do.call("stack", stacked_ttimes)
stacked_ttimes <- raster::as.matrix(stacked_ttimes)
stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ]

# on cluster write to scratch gpfs for larger file size and zip it! 
fwrite(stacked_ttimes, "/scratch/gpfs/mrajeev/output/ttimes/candidate_matrix.gz") 

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/output/ttimes/candidate_*"

file_path <- "R/04_addclinics/01_ttimes_candidates.R"

out.session(path = file_path, filename = "log_cluster.csv", start = start)
stopCluster(cl)
print("Done remotely:)")

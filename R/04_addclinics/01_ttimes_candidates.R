# ------------------------------------------------------------------------------------------------ #
#' Getting travel times for each grid cell for each clinic
#' Code should be run in parallel and final dataset requires a lot of memory! ~ 10 GB
#' Split up clinics or work with fewer clinics if there are memory limitations
#'   with three cores, it takes approximately 120 minutes on MacOS with 16 GB 1867 MHz DDR3 and
#'   2.9 GHz Intel Core i5
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 31 -sp "./R/04_addclinics/01_ttimes_candidates.R" -jn candidates -wt 2m -n@
  
# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# Libraries
library(rgdal)
library(raster)
library(foreach)
library(tidyverse)
library(gdistance)
library(iterators)
library(data.table)
library(glue)
library(fst)

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
rownames(point_mat_candidates) <- csbs$clinic_id
baseline_df <- fread("output/ttimes/baseline_grid.gz")
out_dir <- "/scratch/gpfs/mrajeev/output/ttimes/candidates/"

if(!dir.exists(out_dir)) dir.create(out_dir)

# Depends on # of files you want--lets aim for 16
# Alternatively do the chunks in parallel and you get 30 files (which one is faster/has manageable file sizes?)
# Use fst!

system.time ({
  foreach(point_sub = iter(point_mat_candidates, by = "row", chunksize = 25),    
          .packages = c("raster", "gdistance", "fst", 
                        "data.table", "glue")) %dopar% {
    foreach(points = iter(point_sub, by = "row"), .combine = stack) %do% {
              
      ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                           coords = points, trans_matrix_exists = TRUE,
                           filename_trans = "data/processed/rasters/trans_gc_masked.rds")
      
    } -> stacked_ttimes
    
    stacked_ttimes <- raster::as.matrix(stacked_ttimes)
    stacked_ttimes <- stacked_ttimes[!is.na(getValues(friction_masked)), ]
    colnames(stacked_ttimes) <- rownames(point_sub)
    file_name <- glue("candmat_cand{rownames(point_sub)[1]}_cand{rownames(point_sub)[nrow(point_sub)]}.gz")
    write_fst(data.table(stacked_ttimes), paste0(out_dir, file_name)) 
    
  }
})

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/output/ttimes/candidates"

file_path <- "R/04_addclinics/01_ttimes_candidates.R"

out.session(path = file_path, filename = "log_cluster.csv", start = start)
closeCluster(cl)
mpi.quit()
print("Done remotely:)")


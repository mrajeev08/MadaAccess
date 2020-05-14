# ------------------------------------------------------------------------------------------------ #
#' Getting travel times for each grid cell for each clinic
#' Code should be run in parallel and data is split to make easier to work with/store
#' Adjust the chunksize option based on memory limitations
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
library(foreach)
library(data.table)
library(glue)

# Source scripts
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# Load in GIS files 
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
ctar_metadata <- read.csv("data/processed/clinics/ctar_metadata.csv")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")

# candidate points
csbs <- fread("data/processed/clinics/csb2.csv")
clin_per_comm <- fread("data/processed/clinics/clinic_per_comm.csv")
clin_per_comm <- clin_per_comm[!(clinic_id %in% csbs$clinic_id) & clinic_id > 31][, pop_dens := NULL]
csbs <- rbind(csbs, clin_per_comm)
setorder(csbs, clinic_id) # make sure they're in the right order
point_mat_candidates <- as.matrix(select(csbs, long, lat))
rownames(point_mat_candidates) <- csbs$clinic_id

out_dir <- "/scratch/gpfs/mrajeev/output/ttimes/candidates/"

if(!dir.exists(out_dir)) dir.create(out_dir)

system.time ({
  foreach(point_sub = iter(point_mat_candidates, by = "row", chunksize = 25),    
          .packages = c("raster", "gdistance", "glue", "foreach", "iterators")) %dopar% {
    
    foreach(points = iter(point_sub, by = "row")) %do% {
              
      ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                           coords = points, trans_matrix_exists = TRUE,
                           filename_trans = "data/processed/rasters/trans_gc_masked.rds")
      
    } -> stacked_ttimes

    stacked_ttimes <- brick(stacked_ttimes)
    names(stacked_ttimes) <- rownames(point_sub)
    file_name <- glue("candmat_cand{rownames(point_sub)[1]}_cand{rownames(point_sub)[nrow(point_sub)]}.tif")
    writeRaster(stacked_ttimes, filename = paste0(out_dir, file_name),
                overwrite = TRUE, options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))    
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


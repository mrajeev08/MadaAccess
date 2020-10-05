# ------------------------------------------------------------------------------
#' Getting travel times for each grid cell for each clinic
#' Code should be run in parallel and data is split to make easier to download
# ------------------------------------------------------------------------------

# sub_cmd:=-t 12 -n 31 -jn candidates -wt 2m  -md "gdal"

# Set it up
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = TRUE)

if(!set_up$slurm) fp <- here::here else fp <- cl_safe

cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

# Libraries
library(sf)
library(raster)
library(foreach)
library(gdistance)
library(iterators)
library(data.table)
library(glue)
source(here_safe("R/ttime_functions.R"))

# Load in GIS files
mada_districts <- st_read(fp("analysis/out/shapefiles/mada_districts.shp"))
ctar_metadata <- read.csv(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
friction_masked <- raster(here_safe("data-raw/out/rasters/friction_mada_masked.tif"))

# candidate points
csbs <- fread(here_safe("data-raw/out/clinics/csb2.csv"))
clin_per_comm <- fread(here_safe("data-raw/out/clinics/clinic_per_comm.csv"))
clin_per_comm <- clin_per_comm[!(clinic_id %in% csbs$clinic_id) & clinic_id > 31][, pop_dens := NULL]
csbs <- rbind(csbs, clin_per_comm, fill = TRUE)
setorder(csbs, clinic_id) # make sure they're in the right order

csbs %>%
  dplyr::select(x = long, y = lat, clinic_id) %>%
  as.matrix(.) -> candidates

foreach(
  points = iter(candidates, by = "row"),
  .packages = c("raster", "gdistance", "data.table")
) %dopar% {
  ttimes <- get_ttimes(
    friction = friction_masked, shapefile = mada_districts,
    coords = points[, c("x", "y")], trans_matrix_exists = TRUE,
    filename_trans = "data-raw/out/rasters/trans_gc_masked.rds"
  )
  write_create(ttimes,
    fp(paste0(
      "analysis/out/ttimes/candidates/clinic_",
      points[, "clinic_id"],
      ".tif"
    )),
    writeRaster,
    overwrite = TRUE,
    options = c("COMPRESS=LZW")
  )
} -> stacked_ttimes

# Keep the outputs on the cluster (otherwise takes a while to pulldown)

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

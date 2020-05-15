# ------------------------------------------------------------------------------------------------ #
#' Getting other scenarios (i.e. given all csbs, 1 per district, 1 per commune)
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-sn -t 12 -n 5 -sp "./R/04_addclinics/03_ttimes_alt.R" -jn altclinics -wt 2m -n@

# set up cluster on single node with do Parallel
library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)
start <- Sys.time()

# Libraries
library(rgdal)
library(raster)
library(tidyverse)
library(gdistance)
library(data.table)
library(foreach)
library(iterators)

# Source
source("R/functions/out.session.R")
source("R/functions/ttime_functions.R")

# all candidate points
ctar_metadata <- fread("data/processed/clinics/ctar_metadata.csv")
csbs <- fread("data/processed/clinics/csb2.csv")
clin_per_comm <- fread("data/processed/clinics/clinic_per_comm.csv")
clin_per_dist <- fread("data/processed/clinics/clinic_per_dist.csv")
base_df <- fread("output/ttimes/base_df.gz")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")@data

# Match up with which file & band
brick_dt <- get.bricks(brick_dir = "/scratch/gpfs/mrajeev/output/ttimes/candidates")

# Other scenarios ----------------------------------------------------------------------
# Max (with all csb2s) 
csbs <- brick_dt[csbs, on = "clinic_id"]
max_stats <- update.base(base_df = base_df, cand_df = csbs, nsplit = 5)
max_dist <- aggregate.admin(base_df = max_stats, admin = "distcode", scenario = "max")
max_dist_maxcatch <- get.maxcatch(max_dist, admin = "distcode")
max_comm <- aggregate.admin(base_df = max_stats, admin = "commcode", scenario = "max")
max_comm_maxcatch <- get.maxcatch(max_comm, admin = "commcode")
print("done max")

# One clinic per district 
clin_per_dist <- brick_dt[clin_per_dist, on = "clinic_id"]
armc_per_dist <- update.base(base_df = base_df, cand_df = clin_per_dist, nsplit = 5)
per_dist_dist <- aggregate.admin(base_df = armc_per_dist, admin = "distcode", 
                                 scenario = "armc_per_dist")
per_dist_dist_maxcatch <- get.maxcatch(per_dist_dist, admin = "distcode")
per_dist_comm <- aggregate.admin(base_df = armc_per_dist, admin = "commcode", 
                                 scenario = "armc_per_dist")
per_dist_comm_maxcatch <- get.maxcatch(per_dist_comm, admin = "commcode")

# One per commune 
clin_per_comm <- brick_dt[clin_per_comm, on = "clinic_id"]
armc_per_comm <- update.base(base_df = base_df, cand_df = clin_per_comm, nsplit = 5)
per_comm_dist <- aggregate.admin(base_df = armc_per_comm, admin = "distcode", 
                                 scenario = "armc_per_comm")
per_comm_dist_maxcatch <- get.maxcatch(per_comm_dist, admin = "distcode")
per_comm_comm <- aggregate.admin(base_df = armc_per_comm, admin = "commcode", 
                                 scenario = "armc_per_comm")
per_comm_comm_maxcatch <- get.maxcatch(per_comm_comm, admin = "commcode")

# Merge all scenarios ----------------------------------------------------------------------
# For all catchments
district_df <- fread("output/ttimes/addclinics_district_allcatch.gz")
district_df_all <- rbind(district_df, max_dist, per_dist_dist, per_comm_dist, fill = TRUE)

commune_df <- fread("output/ttimes/addclinics_commune_allcatch.gz")
commune_df_all <- rbind(commune_df, max_comm, per_dist_comm, per_comm_comm, fill = TRUE)

# For max catchments
district_maxcatch <- fread("output/ttimes/addclinics_district_maxcatch.gz")
district_max_all <- rbind(district_maxcatch, per_dist_dist_maxcatch, per_comm_dist_maxcatch,
                          max_dist_maxcatch, fill = TRUE)

commune_maxcatch <- fread("output/ttimes/addclinics_commune_maxcatch.gz")
commune_max_all <- rbind(commune_maxcatch, per_dist_comm_maxcatch, per_comm_comm_maxcatch, 
                         max_comm_maxcatch, fill = TRUE)

# Get district ttimes for communes --------------------------------------------------------
district_merge <- district_max_all[, c("distcode", "ttimes_wtd", "scenario"), 
                                   with = FALSE][, setnames(.SD, "ttimes_wtd", "ttimes_wtd_dist")]

# Match district ttimes to commune ids to get uniform expectations of bite inc across district
commune_df_all$distcode <- mada_communes$distcode[match(commune_df_all$commcode, 
                                                          mada_communes$commcode)]

commune_df_all <- commune_df_all[district_merge, on = c("scenario", "distcode")]

commune_max_all$distcode <- mada_communes$distcode[match(commune_max_all$commcode, 
                                                        mada_communes$commcode)]
commune_max_all <- commune_max_all[district_merge, on = c("scenario", "distcode")]

# Write files out (compressed to reduce file sizes!) ------------------------------------------
fwrite(commune_df_all, "output/ttimes/commune_allcatch.gz")
fwrite(district_df_all, "output/ttimes/district_allcatch.gz")
fwrite(commune_max_all, "output/ttimes/commune_maxcatch.gz")
fwrite(district_max_all, "output/ttimes/district_maxcatch.gz")

# Write out the commune files with a look up var for predictions
commune_df_all$lookup <- paste("scenario", commune_df_all$scenario, sep = "_")
fwrite(commune_df_all, "output/ttimes/commune_allcatch.csv")
commune_max_all$lookup <- paste("scenario", commune_max_all$scenario, sep = "_")
fwrite(commune_max_all, "output/ttimes/commune_maxcatch.csv")

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/ttimes/*"

# Save session info
out.session(path = "R/04_addclinics/03_ttimes_max.R", filename = "log_cluster.csv", 
            start = start)

# ------------------------------------------------------------------------------------------------ #
#' Getting other scenarios (i.e. given all csbs, 1 per district, 1 per commune)
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-sn -t 12 -n 5 -sp "./R/04_addclinics/04_postprocess.R" -jn process -wt 2m -n@

# set up cluster on single node with do Parallel
library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)
start <- Sys.time()

# Libraries
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
base_df <- fread("output/ttimes/base/grid_df.gz")

# Match up with which file & band
brick_dt <- get.bricks(brick_dir = "/scratch/gpfs/mrajeev/output/ttimes/candidates")

# Max (with all csb2s + 1 per comm) ------------------------------------------------
csbs_max <- rbind(csbs, clin_per_comm[!(clinic_id %in% csbs$clinic_id)], fill = TRUE)
csbs_max <- brick_dt[csbs_max, on = "clinic_id"]
max_stats <- update.base(base_df = base_df, cand_df = csbs_max, nsplit = 5)

max_dist <- aggregate.admin(base_df = max_stats, admin = "distcode", 
                            scenario = nrow(csbs_max))
max_dist_maxcatch <- max_dist[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                              by = .(distcode, scenario)]
fwrite(max_dist, "output/ttimes/addclinics/district_allcatch_max.gz")
fwrite(max_dist_maxcatch, "output/ttimes/addclinics/district_maxcatch_max.gz")

max_comm <- aggregate.admin(base_df = max_stats, admin = "commcode", 
                            scenario = nrow(csbs_max))
max_comm_maxcatch <- max_comm[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                              by = .(commcode, scenario)]
fwrite(max_comm, "output/ttimes/addclinics/commune_allcatch_max.gz")
fwrite(max_comm_maxcatch, "output/ttimes/addclinics/commune_maxcatch_max.gz")

print("done max")

# Merge all scenarios -----------------------------------------------------------------
# This will write out to addclinics folder
process_ttimes(dir_name = "output/ttimes/addclinics", include_base = TRUE,
               base_dir = "output/ttimes/base")

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/ttimes/addclinics/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/ttimes/addclinics/*"

# Save session info
out.session(path = "R/04_addclinics/04_postprocess.R", filename = "log_cluster.csv", 
            start = start)

# ------------------------------------------------------------------------------
#' Getting other scenarios (i.e. given all csbs, 1 per district, 1 per commune)
# ------------------------------------------------------------------------------

# sub_cmd=-sn -t 12 -n 5 -jn process -wt 2m
# skipit

# Set it up
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = FALSE)

if (!set_up$slurm) fp <- here::here else fp <- cl_safe

cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

# Libraries
library(raster)
library(gdistance)
library(data.table)
library(foreach)
library(iterators)

# Source
source(here_safe("R/ttime_functions.R"))

# all candidate points
ctar_metadata <- fread(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
csbs <- fread(here_safe("data-raw/out/clinics/csb2.csv"))
clin_per_comm <- fread(here_safe("data-raw/out/clinics/clinic_per_comm.csv"))
base_df <- fread(cl_safe("analysis/out/ttimes/base/grid_df.gz"))


# Max (with all csb2s + 1 per comm) ------------------------------------------------
csbs_max <- rbind(csbs, clin_per_comm[!(clinic_id %in% csbs$clinic_id)], fill = TRUE)
csbs_max[, candfile := fp(paste0(
  "analysis/out/ttimes/candidates/clinic_",
  clinic_id, ".tif"
))]
max_stats <- update_base(base_df = base_df, cand_df = csbs_max, nsplit = 5)

max_dist <- aggregate_admin(
  base_df = max_stats, admin = "distcode",
  scenario = nrow(csbs_max)
)
max_dist_maxcatch <- max_dist[, .SD[prop_pop_catch == max(prop_pop_catch,
                                                          na.rm = TRUE)],
                              by = .(distcode, scenario)]
write_create(
  max_dist,
  fp("analysis/out/ttimes/addclinics/district_allcatch_max.gz"),
  fwrite
)
write_create(
  max_dist_maxcatch,
  fp("analysis/out/ttimes/addclinics/district_maxcatch_max.gz"),
  fwrite
)

max_comm <- aggregate_admin(
  base_df = max_stats, admin = "commcode",
  scenario = nrow(csbs_max)
)
max_comm_maxcatch <- max_comm[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)],
  by = .(commcode, scenario)
]
write_create(
  max_comm,
  fp("analysis/out/ttimes/addclinics/commune_allcatch_max.gz"),
  fwrite
)
write_create(
  max_comm_maxcatch,
  fp("analysis/out/ttimes/addclinics/commune_maxcatch_max.gz"),
  fwrite
)

print("done max")

# Merge all scenarios ----------------------------------------------------------
process_ttimes(
  dir_name = fp("analysis/out/ttimes/addclinics"), include_base = TRUE,
  base_dir = fp("analysis/out/ttimes/base")
)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/ttimes/addclinics/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/ttimes/addclinics/*"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")

# ------------------------------------------------------------------------------------------------ #
#' Post-processing incremental travel times
#' Pulling in district and commune estimates of travel times as clinics are added and also 
#' the maximum with all clinics and baseline with 31 armc
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# Packages
library(data.table)
library(rgdal)
source("R/functions/out.session.R")

# Shapefiles
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
max_clinics <- nrow(read.csv("data/processed/clinics/csb2.csv"))
comm_clinics <- nrow(read.csv("data/processed/clinics/clinic_per_comm.csv"))
dist_clinics <- nrow(read.csv("data/processed/clinics/clinic_per_dist.csv"))

# Combine baseline, added, and other scenarios ------------------------------------------------------------
# District
district_baseline <- fread("output/ttimes/baseline_district.csv")
district_df <- fread("output/ttimes/addclinics_district.gz")
district_max <- fread("output/ttimes/max_district.csv")
district_per_district <- fread("output/ttimes/clin_per_dist_district.csv")
district_per_commune <- fread("output/ttimes/clin_per_comm_district.csv")

district_baseline$clinic_added <- district_baseline$scenario <- 0
district_baseline <- district_baseline[, -c("ttimes_un", "ncells"), with = FALSE]
district_max$clinic_added <- district_max$scenario <- max_clinics
district_per_district$clinic_added <- district_per_district$scenario <- dist_clinics + 0.5
district_per_commune$clinic_added <- district_per_commune$scenario <- comm_clinics + 0.5
district_allcatch <- rbind(district_baseline, district_df, district_max, district_per_commune, 
                           district_per_district)


# Commune
commune_baseline <- fread("output/ttimes/baseline_commune.csv")
commune_df <- fread("output/ttimes/addclinics_commune.gz")
commune_max <- fread("output/ttimes/max_commune.csv")
commune_per_commune <- fread("output/ttimes/clin_per_dist_commune.csv")
commune_per_district <- fread("output/ttimes/clin_per_comm_commune.csv")

commune_baseline$clinic_added <- commune_baseline$scenario <- 0
commune_baseline <- commune_baseline[, -c("ttimes_un", "ncells"), with = FALSE]
commune_max$clinic_added <- commune_max$scenario <- max_clinics
commune_per_district$clinic_added <- commune_per_district$scenario <- dist_clinics + 0.5
commune_per_commune$clinic_added <- commune_per_commune$scenario <- comm_clinics + 0.5
commune_allcatch <- rbind(commune_baseline, commune_df, commune_max, commune_per_commune, 
                           commune_per_district)

# Filter to a single catchment for predicting bites @ admin level -----------------------------
# District single catchment
district_maxcatch <- district_allcatch[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(distcode, scenario)]


# Match district ttimes to commune ids to get uniform expectations of bite inc across district
commune_allcatch$distcode <- mada_communes$distcode[match(commune_allcatch$commcode, 
                                                          mada_communes$commcode)]
district_merge <- district_maxcatch[, 
                                         c("distcode", "ttimes_wtd", "scenario"), 
                                         with = FALSE][, setnames(.SD, "ttimes_wtd", 
                                                                  "ttimes_wtd_dist")]
commune_allcatch <- commune_allcatch[district_merge, on = c("scenario", "distcode")]

# Commune all catch
commune_maxcatch <- commune_allcatch[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                         by = .(commcode, scenario)]

# Write files out (compressed to reduce file sizes!) ------------------------------------------
fwrite(commune_allcatch, "output/ttimes/commune_allcatch.gz")
fwrite(district_allcatch, "output/ttimes/district_allcatch.gz")
fwrite(commune_maxcatch, "output/ttimes/commune_maxcatch.gz")
fwrite(district_maxcatch, "output/ttimes/district_maxcatch.gz")

# Write out the commune files with a look up var for predictions
commune_allcatch$lookup <- paste("scenario", commune_allcatch$scenario, sep = "_")
fwrite(commune_allcatch, "output/ttimes/commune_allcatch.csv")
commune_maxcatch$lookup <- paste("scenario", commune_maxcatch$scenario, sep = "_")
fwrite(commune_maxcatch, "output/ttimes/commune_maxcatch.csv")

out.session(path = "R/04_addclinics/04_postprocess_ttimes.R", filename = "output/log_local.csv",
            start = start)

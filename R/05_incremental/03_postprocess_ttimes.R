####################################################################################################
##' Post-processing incremental travel times
##' Details: Pulling in district and commune estimates of travel times as clinics are added and also 
##' the maximum with all clinics and baseline with 31 armc
##' Author: Malavika Rajeev 
####################################################################################################
rm(list=ls())

##' Packages and scripts
library(data.table)

## Shapefiles
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")

## Filter to a single catchment
## District all and max
district_df <- fread("output/ttimes/incremental_district.csv")
district_df <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(district_id, scenario)]
district_max <- fread("output/ttimes/max_district.csv")
district_max <- district_max[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(district_id, scenario)]
district_max[, c("scenario", "base_catches", "max_catches", 
                "clinic_added") := .(1648, max_catches, NULL, 1648)]
district_baseline <- fread("output/ttimes/baseline_district.csv")
district_baseline <- district_baseline[scenario == 31][, .SD[prop_pop_catch == max(prop_pop_catch, 
                                                                                   na.rm = TRUE)], 
                                                       by = .(district_id, scenario)]
district_baseline$scenario <- 0

district_max <- district_max
## Commune all and max
commune_df <- fread("output/ttimes/incremental_commune.csv")
commune_df <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                         by = .(commune_id, scenario)]
commune_max <- fread("output/ttimes/max_commune.csv")
commune_max <- commune_max[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(commune_id, scenario)]
commune_max[, c("scenario", "base_catches", "max_catches", 
                "clinic_added") := .(1648, max_catches, NULL, 1648)]
commune_baseline <- fread("output/ttimes/baseline_commune.csv")
commune_baseline <- commune_baseline[scenario == 31][, .SD[prop_pop_catch == max(prop_pop_catch, 
                                                                                   na.rm = TRUE)], 
                                                       by = .(commune_id, scenario)]
commune_baseline$scenario <- 0

## Rbind them 
commune_master <- rbind(commune_baseline, commune_df, commune_max)
district_master <- rbind(district_baseline, district_df, district_max)

## Fix so that catches are numeric within the scenarios
district_master[, catch_numeric := as.numeric(as.factor(base_catches)), by = scenario]
commune_master[, catch_numeric := as.numeric(as.factor(base_catches)), by = scenario]

## Match district ttimes to commune ids to get 
mada_communes$match_id <- 1:nrow(mada_communes@data)
commune_master$distcode <- mada_communes$distcode[match(commune_master$commune_id, mada_communes$match_id)]
mada_districts$match_id <- 1:nrow(mada_districts@data)
district_master$distcode <- mada_districts$distcode[match(district_master$district_id, mada_districts$match_id)]
district_master_merge <- district_master[, 
                                 c("distcode", "weighted_times", "scenario"), 
                                 with = FALSE][, setnames(.SD, "weighted_times", "weighted_times_dist")]
commune_master <- commune_master[district_master_merge, on = c("scenario", "distcode")]

## Write out
fwrite(commune_master, "output/ttimes/master_commune.csv")
fwrite(district_master, "output/ttimes/master_district.csv")

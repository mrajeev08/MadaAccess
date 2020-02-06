####################################################################################################
##' Post-processing incremental travel times
##' Details: Pulling in district and commune estimates of travel times as clinics are added and also 
##' the maximum with all clinics and baseline with 31 armc
##' Author: Malavika Rajeev 
####################################################################################################
rm(list=ls())

##' Packages and scripts
library(data.table)
library(rgdal)

## Shapefiles
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")

##' All catchments 
##' ------------------------------------------------------------------------------------------------
## District
district_baseline <- fread("output/ttimes/baseline_district.csv")
district_baseline$clinic_added <- district_baseline$scenario <- 0
district_df <- fread("output/ttimes/incremental_district.csv")
district_max <- fread("output/ttimes/max_district.csv")
district_max$clinic_added <- district_max$scenario <- 1648
district_allcatch <- rbind(district_baseline, district_df, district_max)

## Commune
commune_baseline <- fread("output/ttimes/baseline_commune.csv")
commune_baseline$clinic_added <- commune_baseline$scenario <- 0
commune_df <- fread("output/ttimes/incremental_commune.csv")
commune_max <- fread("output/ttimes/max_commune.csv")
commune_max$clinic_added <- commune_max$scenario <- 1648
commune_allcatch <- rbind(commune_baseline, commune_df, commune_max)

##' Filter to a single catchment 
##' ------------------------------------------------------------------------------------------------
## District max
district_maxcatch <- district_allcatch[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                           by = .(district_id, scenario)]

## Match district ttimes to commune ids to get 
commune_allcatch$distcode <- mada_communes$distcode[commune_allcatch$commune_id]
district_maxcatch$distcode <- mada_districts$distcode[district_maxcatch$district_id]
district_merge <- district_maxcatch[, 
                                         c("distcode", "weighted_times", "scenario"), 
                                         with = FALSE][, setnames(.SD, "weighted_times", 
                                                                  "weighted_times_dist")]
commune_allcatch <- commune_allcatch[district_merge, on = c("scenario", "distcode")]

## Commune max
commune_maxcatch <- commune_allcatch[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)], 
                         by = .(commune_id, scenario)]

## Write out
fwrite(commune_allcatch, "output/ttimes/commune_allcatch.csv")
fwrite(district_allcatch, "output/ttimes/district_allcatch.csv")
fwrite(commune_maxcatch, "output/ttimes/commune_maxcatch.csv")
fwrite(district_maxcatch, "output/ttimes/district_maxcatch.csv")

####################################################################################################
##' Step 2: Getting scenario for additional clinics
##' Details: Take the access matrices (admin units to each clinic) and rank clinics by how much
##' they improve access for a population living greater than a threshold distance or travel time away
##' Author: Malavika Rajeev 
####################################################################################################

##' Set-up 
##' ------------------------------------------------------------------------------------------------
rm(list = ls())

##' Libraries
library(doRNG)
library(foreach)
library(dplyr)
select <- dplyr::select
source("R/functions/access_functions.R")

## Read in data
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
district_ttimes_candidates <- read.csv("data/processed/catchmats/candidates_district_ttimes_weighted.csv")
commune_ttimes_candidates <- read.csv("data/processed/catchmats/candidates_commune_ttimes_weighted.csv")
district_distwtd_candidates <- read.csv("data/processed/catchmats/candidates_district_distance_weighted.csv")
commune_distwtd_candidates <- read.csv("data/processed/catchmats/candidates_commune_distance_weighted.csv")
district_distcent_candidates <- read.csv("data/processed/catchmats/candidates_district_distance_centroid.csv")
commune_distcent_candidates <- read.csv("data/processed/catchmats/candidates_commune_distance_centroid.csv")

##' Commune scenarios
commune_ttimes_weighted <- add.armc(base_metric = mada_communes$ttms_wtd, 
                                    clinic_names = rownames(commune_ttimes_candidates), 
                                    clinic_catchmat = commune_ttimes_candidates, 
                                    prop_pop = mada_communes$pop/sum(mada_communes$pop), 
                                    max_clinics = ncol(commune_ttimes_candidates), threshold = 3*60)
write.csv(commune_ttimes_weighted, "output/scenarios/scenario_commune_ttmswtd.csv")

commune_distance_weighted <- add.armc(base_metric = mada_communes$dist_wtd, 
                                      clinic_names = rownames(commune_distwtd_candidates), 
                                      clinic_catchmat = commune_distwtd_candidates, 
                                      prop_pop = mada_communes$pop/sum(mada_communes$pop), 
                                      max_clinics = ncol(commune_distwtd_candidates), threshold = 50)
write.csv(commune_distance_weighted, "output/scenarios/scenario_commune_distwtd.csv")

commune_distance_centroid <- add.armc(base_metric = mada_communes$dist_cent, 
                                      clinic_names = rownames(commune_distcent_candidates), 
                                      clinic_catchmat = commune_distcent_candidates, 
                                      prop_pop = mada_communes$pop/sum(mada_communes$pop), 
                                      max_clinics = ncol(commune_distcent_candidates), threshold = 50)
write.csv(commune_distance_centroid, "output/scenarios/scenario_commune_distcent.csv")

##' District scenarios
district_ttimes_weighted <- add.armc(base_metric = mada_districts$ttms_wtd, 
                                     clinic_names = rownames(district_ttimes_candidates), 
                                     clinic_catchmat = district_ttimes_candidates, 
                                     prop_pop = mada_districts$pop/sum(mada_districts$pop), 
                                     max_clinics = ncol(district_ttimes_candidates), threshold = 3*60)
write.csv(district_ttimes_weighted, "output/scenarios/scenario_district_ttmswtd.csv")

district_distance_weighted <- add.armc(base_metric = mada_districts$dist_wtd, 
                                       clinic_names = rownames(district_distwtd_candidates),
                                       clinic_catchmat = district_distwtd_candidates, 
                                       prop_pop = mada_districts$pop/sum(mada_districts$pop), 
                                       max_clinics = ncol(district_distwtd_candidates), threshold = 50)
write.csv(district_distance_weighted, "output/scenarios/scenario_district_distwtd.csv")

district_distance_centroid <- add.armc(base_metric = mada_districts$dist_cent, 
                                     clinic_names = rownames(district_distcent_candidates), 
                                     clinic_catchmat = district_distcent_candidates, 
                                     prop_pop = mada_districts$pop/sum(mada_districts$pop), 
                                     max_clinics = ncol(district_distcent_candidates), threshold = 50)
write.csv(district_distance_centroid, "output/scenarios/scenario_district_distcent.csv")


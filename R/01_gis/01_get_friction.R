####################################################################################################
##' Generating GIS files
##' Details: Getting masked friction surface as input for travel time estimates 
##' Also creating transition layer for gdistance functions
##' Author: Malavika Rajeev 
####################################################################################################

##' Packages
rm(list=ls())
library(malariaAtlas) # for friction surface
library(raster) # for reading in rasters
library(rgdal) # for reading in shapefiles
library(gdistance) # for making transition object
source("R/functions/utils.R")

##' Shapefile for masking to (from OCHA)
mada_districts <- readOGR("data/raw/shapefiles/districts/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")

##' Masked friction surface
friction_masked <- getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_districts)
plot(friction_masked) ## test
writeRaster(friction_masked, "data/processed/rasters/friction_mada_masked.tif", overwrite = TRUE)

##' Masked transition surface
##' Make and geocorrect the transition matrix (i.e., the graph)
trans <- transition(friction_masked, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
trans_gc <- geoCorrection(trans)
saveRDS(trans_gc, "data/processed/rasters/trans_gc_masked.rds")

## load package
# install.packages("devtools")
# devtools::install_github("wpgp/wpgpDownloadR")
library(wpgpDownloadR)
worlPop2015 <- wpgpGetCountryDataset(ISO3 = "MDG", covariate = "ppp_2015",
                                     destDir ="data/raw/WorldPop")
worlPop2020 <- wpgpGetCountryDataset(ISO3 = "MDG", covariate = "ppp_2020",
                                     destDir ="data/raw/WorldPop")
worlPop2018 <- wpgpGetCountryDataset(ISO3 = "MDG", covariate = "ppp_2018",
                                     destDir ="data/raw/WorldPop") # for comparing to census

## aggregate facebook pop (this takes abt 10min!)
pop_fb <- raster("data/raw/population_mdg_2018-10-01-2/population_mdg_2018-10-01.tif")
system.time(pop_fb <- raster::aggregate(pop_fb, fact = 5, fun = sum, na.rm = TRUE))
writeRaster(pop_fb, "data/raw/population_mdg_2018-10-01-2/fb2018_aggregated.tif", overwrite = TRUE)

##' Saving session info
out.session(path = "R/01_gis/01_get_friction.R", filename = "sessionInfo.csv")

library(dplyr)
health_facs_all <- read_excel("data/raw/health_facs_all.xlsx")
health_facs_all %>%
  filter(Country == "Madagascar") -> health_facs_mada
write.csv(health_facs_mada, "health_facs_mada.csv", row.names = FALSE)

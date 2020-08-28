# ------------------------------------------------------------------------------------------------ #
#' Pop comparison of resampled World Pop 2015 estimates (older version from Linard et al. 2012) 
# ------------------------------------------------------------------------------------------------ #

# Setup
library(raster)
library(sf)
library(data.table)
library(fasterize)

# Data
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
census_2018 <- read.csv("data/raw/census2018.csv")
mada_districts <- st_read("data/processed/shapefiles/mada_districts.shp")
mada_communes <- st_read("data/processed/shapefiles/mada_communes.shp")

# Extract pops to region and check ------------------------------------------------------------
mada_districts$reg_code <- substr(mada_districts$distcode, 1, 4)
length(unique(mada_districts$reg_code)) == length(unique(census_2018$Region)) 
nrow(census_2018) == length(unique(census_2018$Region)) 

mada_districts$id <- 1:nrow(mada_districts)
reg_code <- fasterize(mada_districts, pop1x1, "id")[] # by row id
reg_code <- mada_districts$reg_code[reg_code]

# data table and agg by region code
pop_by_region <- data.table(reg_code = reg_code, pop = pop1x1[])
pop_by_region <- pop_by_region[, .(pop_comp = sum(pop, na.rm = TRUE)), by = reg_code] 
pop_by_region <- as.data.table(census_2018)[pop_by_region, on = "reg_code"]
write.csv(pop_by_region, "output/stats/pop_by_region.csv", row.names = FALSE)

# Comparing total 
sum(getValues(pop1x1), na.rm = TRUE) == sum(pop_by_region$pop_comp) # okay (no missing pops)

# Moramanga comparison (Ratovoson et al. 2019) ------------------------------------------------
mora_pop_comp <- data.table(commune = c("Ambohibary", "Moramanga", "Ampasimpotsy Gara"), 
                            commcode = c("MG33314030", "MG33314010", "MG33314050"), 
                            pop_MHURAM = c(40767, 37634/2, 37634/2), 
                            pop_est = mada_communes$pop[mada_communes$commcode %in% c("MG33314030", "MG33314010", "MG33314050")],
                            notes = c("", rep("split between Ambohibary & Ampasimpotsy", 2)))
write.csv(mora_pop_comp, "output/stats/mora_pop_comp.csv", row.names = FALSE)


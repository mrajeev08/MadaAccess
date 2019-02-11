## Script to generate rasterized shapefile codes (mada districts + communes)
rm(list = ls())
mada_district <- readOGR("data/MadaGIS/district_init.shp")
mada_communes <- readOGR("data/MadaGIS/commune_mada.shp")
ttimes_base <- raster("output/ttimes_unmasked.tif")
pop10 <- raster("output/pop10.tif")

mada_district$row_id <- 1:nrow(mada_district@data)
mada_communes$row_id <- 1:nrow(mada_communes@data)

system.time({
  district_rast <- rasterize(mada_district, ttimes_base, field = mada_district$row_id)})
writeRaster(district_rast, filename = "output/district_raster.tif", overwrite = TRUE)
district_check <- raster("output/district_raster.tif")
plot(district_check)

system.time({
  commune_rast <- rasterize(mada_communes, ttimes_base, field = mada_communes$row_id)})
writeRaster(commune_rast, filename = "output/commune_raster.tif", overwrite = TRUE)
commune_check <- raster("output/commune_raster.tif")
plot(commune_check)

mada_communes@data %>%
  select(comm_code = mdg_com_co, comm_name = commune, row_id_comm = row_id) %>%
  mutate(dist_code = substr(comm_code, 1, 8)) %>%
  left_join(select(mada_district@data, 
                   dist_code = mdg_dis_co, dist_name = district, 
                   row_id_dist = row_id)) -> shapefile_key
write.csv(shapefile_key, "output/shapefile_key.csv")
key <- read.csv("output/shapefile_key.csv", row.names = 1)


check <- as.data.table(list(ttimes = values(ttimes_base), pop = values(pop10), 
                            commune = values(commune_check), 
                            district = values(district_check)))
check <- drop_na(check)
check[, .(check = ttimes*pop/sum(pop)), by = commune]

check$df <- 1:nrow(check)

pop_df <- as.data.table(list(ttimes = rep(check$ttimes, check$pop), 
                             commune = rep(check$commune, check$pop), 
                             district = rep(check$district, check$pop)))

pop_df$check_size <- 1:nrow(pop_df)
pop_df$check_size2 <- 1:nrow(pop_df)

head(pop_df)
pop_df[, .(ttimes_weighted_dist = mean(ttimes)), by = district]
pop_df[, .(ttimes_weighted_comm = mean(ttimes)), by = commune]


length(ttimes_check) ## should equal the pop!

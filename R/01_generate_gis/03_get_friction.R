## Getting masked and unmasked friction surfaces
rm(list=ls())
library(malariaAtlas)
library(raster)

# Masked from MAP
mada_communes <- getShp(country = "Madagascar", admin_level = "admin3") # get commune level shapefile
friction_masked <- malariaAtlas::getRaster(
    surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
    shp = mada_communes)
plot(friction_masked)
writeRaster(friction_masked, "output/friction_mada_masked.tif", overwrite = TRUE)

## Unmasked
## get unmasked file from MAP (takes a long time! Big file...)
friction_world <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015")
friction_unmasked <- crop(friction_world, mada_communes)
plot(friction_unmasked)
writeRaster(friction_unmasked, "output/friction_mada_unmasked.tif", overwrite = TRUE)

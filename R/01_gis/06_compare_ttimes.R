# ------------------------------------------------------------------------------------------------ #
#' Comparing estimates of travel times from multiple sources:
#'  1) MAP estimates using friction surface
#'  2) Self-reported travel times of bite patients reporting to Moramanga ARMC
#'  3) IPM driving times (from Helene and Luciano)                                                
# ------------------------------------------------------------------------------------------------ #

# Set-up
library(dplyr)
library(ggplot2)
library(rgdal)
select <- dplyr::select
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")

# Step 1 
# Pull in Mora times
mora <- read.csv("data/processed/bitedata/moramanga_ttimes.csv")
mora %>%
  left_join(select(mada_communes@data, commcode, district, commune, pop, ttimes_wtd, catchment), 
            by = c("commcode" = "commcode")) %>%
  mutate(ttimes_wtd = ttimes_wtd/60) %>% 
  filter(catchment == "Moramanga") -> mora # transform ttimes to hours

mora %>%
  group_by(commune) %>%
  summarise(mean_ttimes = mean(hours, na.rm = TRUE), 
            ttimes_wtd = ttimes_wtd[1],
            nobs = n(), 
            min = min(hours, na.rm = TRUE),
            max = max(hours, na.rm = TRUE)) -> mora_means

ggplot() +
  geom_point(data = mora, aes(x = reorder(commune, ttimes_wtd), y = hours, 
                                    color = ttimes_wtd),
             shape = 18, size = 2) +
  geom_point(data = mora, aes(x = reorder(commune, ttimes_wtd), y = ttimes_wtd), 
             shape = 0, size = 2) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_color_viridis_c(option = "magma", direction = -1) +
  coord_flip() +
  cowplot::theme_minimal_hgrid()
  
ggplot() +
  geom_pointrange(data = mora_means, 
                  aes(x = ttimes_wtd, y = mean_ttimes, ymax = max, ymin = min, 
                      fill = mean_ttimes, size = log(nobs)/5), color = "grey50", 
                  alpha = 0.75, shape = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "Grey") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_size_identity()

# Contacts
mora_bites <- read.csv("data/processed/bitedata/moramanga.csv")
mora_bites$ttimes_wtd <- mada_communes$ttimes_wtd[match(mora_bites$commcode, mada_communes$commcode)]

# Step 2 
# use get travel times to get the travel times from point A to point B in drive times from IPM

# Step 3 
# plot and compare

# Save session info

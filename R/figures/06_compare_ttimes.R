# ------------------------------------------------------------------------------------------------ #
#' Comparing estimates of travel times from multiple sources:
#'  1) MAP estimates using friction surface
#'  2) Self-reported travel times of bite patients reporting to Moramanga ARMC
#'  3) IPM driving times (from Helene and Luciano)                                                
# ------------------------------------------------------------------------------------------------ #

# Set-up
library(tidyverse)
library(rgdal)
library(lubridate)
library(raster)
library(rasterVis)
library(patchwork)
library(ggridges)
library(cowplot)
select <- dplyr::select

# data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
base_times <- raster("output/ttimes/baseline_ttimes.tif")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")

# Raw ttimes + prop pop  ----------------------------------------------------------------------
# Colors
ttime_cols <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e', 
                '#7a0177', '#49006a')
ttime_breaks <- c(-0.1, 1, 2, 3, 4, 6, 8, 10, 15, Inf)
ttime_labs <- c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6",  "6 - 8", "8 - 10", "10 - 15", "15 +")
names(ttime_cols) <- ttime_labs

figM3.A <- gplot(base_times) + 
  geom_raster(aes(fill = cut(value/60, breaks = ttime_breaks, labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  geom_point(data = ctar_metadata, aes(x = LONGITUDE, y = LATITUDE), color = "darkgrey", 
             shape = 4,
             stroke = 2) +
  theme_map() +
  labs(tag = "A")

# Proportion of pop 
baseline_df <- fread("output/ttimes/baseline_grid.gz")
baseline_df %>%
  filter(!is.na(pop), !is.infinite(ttimes), !is.na(ttimes)) %>%
  mutate(cut_times = cut(ttimes/60, breaks = ttime_breaks,
                         labels = ttime_labs)) %>%
  group_by(cut_times) %>%
  summarize(prop_pop = sum(prop_pop, na.rm = TRUE)) -> prop_pop_ttimes

figM3.B <- ggplot(data = prop_pop_ttimes, aes(x = cut_times, y = prop_pop,
                                              fill = cut_times)) +
  geom_col(color = "grey") +
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE) +
  ylim(c(0, 0.6)) +
  labs(x = "Travel times (hrs)", y = "Proportion of population") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(tag = "B")

figM3 <- (figM3.A / figM3.B) + plot_layout(heights = c(2, 1))

# for inline figs
ggsave("figs/main/M3_base_ttimes.jpeg", figM3, height = 10, width = 5)
# for plos
ggsave("figs/main/M3_base_tttimes.tiff", figM1, dpi = 300, height = 7, width = 4,
       compression = "lzw", type = "cairo")


# Groundtruthing ------------------------------------------------------------------------------
# Compare Mora and IPM ttimes
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

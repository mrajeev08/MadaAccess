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
  labs(tag = "A") +
  coord_quickmap()

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
                    drop = FALSE, guide = "none") +
  ylim(c(0, 0.6)) +
  labs(x = "Travel times (hrs)", y = "Proportion of\n population") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(tag = "B")

figM3 <- (figM3.A / figM3.B) + plot_layout(heights = c(2, 1))

# for inline figs
ggsave("figs/main/M3_base_ttimes.jpeg", figM3, height = 10, width = 5)
# for plos
ggsave("figs/main/M3_base_tttimes.tiff", figM3, dpi = 300, height = 7, width = 4,
       compression = "lzw", type = "cairo")


# Groundtruthing ------------------------------------------------------------------------------

# Get distances 
# Get ttimes_wtd
# Get ttimes_unwtd

# Compare corrs 

# Compare to estimated (means or raw obs?)

# Compare Mora and IPM ttimes
# Pull in Mora times
mora <- read.csv("data/processed/bitedata/moramanga_ttimes.csv")
mada_communes$catchment <- ctar_metadata$CTAR[mada_communes$catchment]
mora %>%
  left_join(select(mada_communes@data, commcode, district, lat_cent, long_cent, 
                   commune, pop, ttimes_wtd, catchment), 
            by = c("commcode" = "commcode")) %>%
  mutate(ttimes_wtd = ttimes_wtd/60) %>% 
  filter(catchment == "Moramanga") %>%
  mutate(multiple = rowSums(dplyr::select(., car:Pus)),
         mode = case_when(multiple > 1 ~ "Multiple",
                          car == 1 ~ "Car", 
                          Motorbike == 1 ~ "Motorbike", 
                          foot == 1 ~ "Foot",
                          Bus == 1 ~ "Bus", 
                          Bicycle == 1 ~ "Bicycle", 
                          Pus == 1 ~ "Pus-pus",
                          Other == 1 ~ "Other")) -> gtruth_mora # transform ttimes to hours

gtruth_mora %>%
  group_by(commcode) %>%
  summarise(mean_ttimes = mean(hours, na.rm = TRUE), 
            ttimes_wtd = ttimes_wtd[1],
            nobs = n(), 
            min = min(hours, na.rm = TRUE),
            max = max(hours, na.rm = TRUE)) %>% 
  left_join(select(mada_communes@data, commcode, district, lat_cent, 
                   long_cent, commune, pop, catchment), 
            by = c("commcode" = "commcode")) -> mora_means


gtruth_IPM <- read.csv("output/ttimes/groundtruth_IPM.csv")
gtruth_IPM$mins_estimated[is.infinite(gtruth_IPM$mins_estimated)] <- NA
ggplot() +
  geom_point(data = gtruth_IPM, aes(x = mins_observed/60, y = mins_estimated/60), shape = 1) +
  geom_point(data = mora_means, aes(x = mean_ttimes, y = ttimes_wtd, size = log(nobs)), alpha = 0.5, 
             shape = 16) +
  scale_size_identity() +
  geom_smooth(data = gtruth_IPM, aes(x = mins_observed/60, y = mins_estimated/60), method = "lm") +
  geom_smooth(data = mora_means, aes(x = mean_ttimes, y = ttimes_wtd), method = "lm") +
  xlim(c(0, max(mora_means$mean_ttimes, na.rm = TRUE))) +
  ylim(c(0, max(mora_means$mean_ttimes, na.rm = TRUE)))

ggplot() +
    geom_point(data = gtruth_IPM, aes(x = mins_observed/60, y = mins_estimated/60), shape = 16) +
    geom_point(data = gtruth_mora, aes(x = hours, y = ttimes_wtd), shape = 17) +
    geom_smooth(data = gtruth_IPM, aes(x = mins_observed/60, y = mins_estimated/60), method = "lm") +
    geom_smooth(data = gtruth_mora, aes(x = hours, y = ttimes_wtd), method = "lm") +
    xlim(c(0, max(mora_means$mean_ttimes, na.rm = TRUE))) +
    ylim(c(0, max(mora_means$mean_ttimes, na.rm = TRUE)))
  

ggplot() +
  geom_pointrange(data = mora_means, 
                  aes(x = ttimes_wtd, y = mean_ttimes, ymax = max, ymin = min, 
                      fill = mean_ttimes, size = log(nobs)/5), color = "grey50", 
                  alpha = 0.75, shape = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "Grey") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_size_identity()

ggplot(data = filter(gtruth_mora, !is.na(mode)), aes(x = mode)) + geom_bar()
ggplot(data = filter(gtruth_mora, !is.na(mode)), aes(x = mode, y = hours)) + geom_boxplot()
ggplot(data = filter(gtruth_mora, !is.na(mode)), aes(x = mode, y = hours)) + 
  geom_violin() +
  scale_y_continuous(trans = "log")


# ttimes to points to compare for IPM drive times ---------------------------------------------
ttimes_IPM <- read.csv("data/raw/ttimes_IPM.csv")
point_mat_to <- as.matrix(dplyr::select(ttimes_IPM, x = LongAriv, y = LatAriv))
point_mat_from <- as.matrix(dplyr::select(ttimes_IPM, x = LongDep, y = LatDep))

# for each start and end point
# takes ~ 6 seconds per point
cl <- makeCluster(3)
registerDoParallel(cl)

system.time ({
  foreach(points_to = iter(point_mat_to, by = "row"),
          points_from = iter(point_mat_from, by = "row"),
          .packages = c("raster", "gdistance", "data.table")) %dopar% {
            ttimes <- get.ttimes(friction = friction_masked, shapefile = mada_districts,
                                 coords = points_to, trans_matrix_exists = TRUE,
                                 filename_trans = "data/processed/rasters/trans_gc_masked.rds")
            extract(ttimes, SpatialPoints(points_from)) 
          } -> ttimes_est
})

stopCluster(cl) 

ttimes_IPM %>%
  mutate(arrival = mdy_hm(paste(Date, Harrival)),
         depart = mdy_hm(paste(Date, Hdepart)),
         mins_observed = arrival - depart, 
         mins_estimated = unlist(ttimes_est)) -> groundtruth
write.csv(groundtruth, "output/ttimes/groundtruth_IPM.csv")


# Get distance ests ---------------------------------------------------------------------------

# Compare distance to weighted & unweighted travel times @commune level -----------------------

# Corr plot ----------------------------------------------------------------------------------

# As predictor of bites -----------------------------------------------------------------------

# Plot raw data -------------------------------------------------------------------------------

# Save session info ---------------------------------------------------------------------------

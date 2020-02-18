# ------------------------------------------------------------------------------------------------ #
#' Main section on travel times 
# ------------------------------------------------------------------------------------------------ #

# Set-up
library(tidyverse)
library(rgdal)
library(raster)
library(patchwork)
library(cowplot)
library(foreach)
library(data.table)
select <- dplyr::select
source('R/functions/out.session.R')
source('R/functions/ttime_functions.R')

# data
ctar_metadata <- read.csv("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")
base_times <- raster("output/ttimes/base_ttimes.tif")
pop1x1 <- raster("data/processed/rasters/wp_2015_1x1.tif")
friction_masked <- raster("data/processed/rasters/friction_mada_masked.tif")


# Figure: raw ttimes + prop pop  ----------------------------------------------------------------
ttime_cols <- c('#fff7f3', '#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e', 
                '#7a0177', '#49006a')
ttime_breaks <- c(-0.1, 1, 2, 3, 4, 6, 8, 10, 15, Inf)
ttime_labs <- c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6",  "6 - 8", "8 - 10", "10 - 15", "15 +")
names(ttime_cols) <- ttime_labs

base_df <- as.data.frame(base_times, xy = TRUE)
base_df$pop <- getValues(pop1x1)
  
ttimes_A <- ggplot() + 
  geom_raster(data = base_df, aes(x, y, 
                                  fill = cut(base_ttimes/60, breaks = ttime_breaks, 
                                             labels = ttime_labs))) + 
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, na.value = "black") +
  scale_alpha_continuous(range = c(0.1, 1), 
                         trans = "log", breaks = c(10, 1000, 1e4, 1e5), na.value = 1,
                         guide = guide_legend(title = "log(pop)", override.aes = list(fill = '#dd3497',
                                                                                      linetype = 1,
                                                                                      color = '#dd3497'))) +
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

# write.csv(prop_pop_ttimes, "output/stats/prop_pop_ttimes") # write prop_pop_ttimes for stats

prop_B <- ggplot(data = prop_pop_ttimes, aes(x = cut_times, y = prop_pop,
                                              fill = cut_times)) +
  geom_col(color = "grey") +
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
                    drop = FALSE, guide = "none") +
  ylim(c(0, 0.6)) +
  labs(x = "Travel times (hrs)", y = "Proportion of\n population") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(tag = "B")

ttimes_base <- (ttimes_A | prop_B) + plot_layout(widths = c(2, 1), guides = "collect")

# for inline figs
ggsave("figs/main/ttimes_base.jpeg", ttimes_base, height = 5, width = 7)
# for plos
ggsave("figs/main/ttimes_base.tiff", ttimes_base, dpi = 300, height = 5, width = 7,
       compression = "lzw", type = "cairo")


# Comparing ttimes wtd/un/distance ------------------------------------------------------------
gtruth <- read.csv("output/ttimes/gtruth_ttimes.csv")

gtruth %>%
  filter(type == "commune_wtd", !is.na(ttimes_reported)) %>%
  group_by(commcode) %>%
  summarize(mean = mean(ttimes_reported), 
            nobs = n(), 
            max = max(ttimes_reported, na.rm = TRUE), min = min(ttimes_reported, na.rm = TRUE)) %>%
  left_join(select(gtruth, type, commcode, ttimes_est, ttimes_un, distance)) %>%
  unique() -> mora_means

gtruth %>%
  filter(type == "point") %>%
  mutate(nobs = 5, mean = ttimes_reported) %>%
  bind_rows(mora_means) -> all

gtruth_comp <- ggplot(data = all, 
       aes(x = ttimes_est, y = mean, size = log(nobs), shape = type, 
           color = type)) +
  geom_point(alpha = 0.75) +
  geom_linerange(aes(ymax = max, ymin = min), size = 1, alpha = 0.75) +
  scale_size_identity() +
  geom_smooth(method = "lm", size = 1) +
  scale_shape_discrete(solid = TRUE, labels = c("Commune mean", "Grid cell"),
                       name = "Type of estimate") +
  scale_color_manual(values = c("#665565", "#CC575F"),
                                  labels = c("Commune mean", "Grid cell"), 
                       name = "Type of estimate") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  guides(shape = guide_legend(override.aes = list(linetype = c(0, 0), fill = "NA",
                                                  size = 2))) +
  theme_minimal_hgrid() +
  labs(x = "Estimated travel times (hrs)", y = "Reported travel times (hrs)")
 
# for inline figs
ggsave("figs/main/gtruth.jpeg", gtruth_comp, height = 5, width = 5)
# for plos
ggsave("figs/main/gtruth.tiff", gtruth_comp, dpi = 300, height = 5, width = 5,
       compression = "lzw", type = "cairo")

# Supplementary  ------------------------------------------------------------------------------

# Raw data (IPM)

# Raw data (Mora)

# Plots comparing ttimes_wtd / unwtd / distance @ commune/district/grid scales
all$ttimes_wtd <- ifelse(all$type == "commune_wtd", all$ttimes_est, NA)
all$ttimes_un <- ifelse(all$type == "point", all$ttimes_est, all$ttimes_un)

ggplot(data = filter(all, type == "commune_wtd"), 
       aes(x = ttimes_wtd, y = mean, size = log(nobs), shape = type), alpha = 0.75) +
  geom_point() +
  scale_size_identity() +
  geom_smooth(method = "lm") +
  facet_wrap(~ type, scales = "free_x", drop = FALSE)

ggplot(data = all, 
       aes(x = ttimes_un, y = mean, size = log(nobs), shape = type), alpha = 0.75) +
  geom_point() +
  scale_size_identity() +
  geom_smooth(method = "lm", , size = 1) +
  facet_wrap(~ type, scales = "free_x")

points <- c(NA, summary(lm(mean ~ ttimes_un, data =filter(all, type == "point")))$r.squared,
            summary(lm(mean ~ log(distance), data =filter(all, type == "point")))$r.squared)
communes <- c(summary(lm(mean ~ ttimes_wtd, data =filter(all, type == "commune_wtd")))$r.squared, 
              summary(lm(mean ~ ttimes_un, data =filter(all, type == "commune_wtd")))$r.squared,
              summary(lm(mean ~ log(distance), data =filter(all, type == "commune_wtd")))$r.squared)  
metric <- c("Weighted travel times (hrs)", "Unweighted travel times (hrs)", "Distance (km)")

write.csv(data.frame(metric, points, communes), "output/stats/access_met_r2.csv")

ggplot(data = all, 
       aes(x = distance, y = mean, size = log(nobs), shape = type), alpha = 0.75) +
  geom_point() +
  scale_size_identity() +
  geom_smooth(method = "lm", size = 1) +
  facet_wrap(~ type, scales = "free_x")

ggplot() +
  geom_point(data = all, 
             aes(x = ttimes_un, y = mean, 
                 color = mean, size = log(nobs), shape = type),
             alpha = 0.75) +
  scale_color_viridis_c(option = "magma", direction = -1) +
  scale_size_identity() +
  geom_abline(slope = 1, intercept = 0, linetype = 2)

dist <- ggplot() +
  geom_point(data = all, 
             aes(x = distance, y = mean, 
                 color = mean, size = log(nobs), shape = type),
             alpha = 0.75) +
  scale_color_viridis_c(option = "magma", direction = -1) +
  scale_size_identity() +
  geom_smooth(data = all, aes(x = distance, y = mean), method = "lm")

(ttimes_un + ttimes_wtd + dist) + plot_layout(nrow = 3)

summary(lm(ttimes_reported ~ distance, all))$r.squared

cor(mora_means$mean, mora_means$ttimes_est, use = "complete.obs")
cor(gtruth$ttimes_un, gtruth$ttimes_reported, use = "complete.obs")
cor(gtruth$distance, gtruth$ttimes_reported, use = "complete.obs")

GGally::ggpairs(data = all, columns = c("ttimes_est", "ttimes_un", "distance", "ttimes_reported"))

gtruth %>%
  filter(type != "point", !is.na(mode)) %>%
  group_by(mode) %>%
  summarize(ttimes_est = mean(ttimes_est, na.rm = TRUE), 
            ttimes_reported = mean(ttimes_reported, na.rm = TRUE),
            nobs = n()) -> mode_sum
gtruth_mora <- filter(gtruth, type != "point", !is.na(mode))
ggplot(data = mode_sum, aes(x = mode, y = nobs, fill = ttimes_reported)) + geom_col()

ggplot(data = gtruth_mora, aes(x = mode, y = ttimes_reported)) + 
  geom_boxplot(outlier.color = NA) +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, shape = 1)

ggplot(data = gtruth_mora, aes(x = mode, y = ttimes_reported)) + 
  geom_violin() +
  ggbeeswarm::geom_quasirandom(alpha = 0.5, shape = 1)

ggplot(data = gtruth_mora, aes(x = reorder(mode, mode, function(x) -length(x)))) + geom_bar()
ggplot(data = gtruth_mora, aes(x = reorder(mode, mode, function(x) -length(x)))) + 
  geom_bar() +
  facet_wrap(~commcode, scales = 'free')


# ------------------------------------------------------------------------------------------------ #
#' Making maps for figure 1 of ctar and patient locations
#' Details: network style figure for vizualizing where patients are reporting to in each 
#' district
# ------------------------------------------------------------------------------------------------ #

# Libraries
library(patchwork)
library(tidyverse)
library(rgdal)
library(data.table)
library(cowplot)
select <- dplyr::select

# Read in raw data (not processed)
district_bites <- fread("output/bites/district_bites.csv")
mora_bites <- fread("output/bites/mora_bites.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")

# colors by catchment
catch_cols <- c("#FCC56F","#004D43", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
                "#21abcd", "#B79762", "#FFDBE5", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
                "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
                "#F2300F", "#FF2F80","#61615A", "#4A3B53", "#6B7900", "#00C2A0", "#FFAA92",
                "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
catch_fills <- catch_cols
names(catch_cols) <- ctar_metadata$CTAR
names(catch_fills) <- ctar_metadata$CTAR

# District bite incidence estimates -----------------------------------------------------------
district_bites %>%
  select(group_name = distcode, pop, catchment, ttimes_wtd, avg_bites, min_bites, max_bites, 
         sd_bites, 
         nobs) %>%
  mutate(dataset = "National", 
         nobs = ifelse(is.na(nobs), 1, nobs)) -> bites_plot

mora_bites %>%
  select(group_name = distcode, pop, catchment, ttimes_wtd, avg_bites) %>%
  group_by(group_name, catchment) %>%
  summarize(avg_bites = sum(avg_bites)) %>%
  filter(avg_bites > 10) %>%
  mutate(pop = mada_districts$pop[match(group_name, mada_districts$distcode)],
         ttimes_wtd = mada_districts$ttimes_wtd[match(group_name, mada_districts$distcode)],
         dataset = "Moramanga", nobs = 4) %>%
  bind_rows(bites_plot) -> all_bites

size_labs <- c("1" = 1.75, "2" = 2.25, "3" = 3, "4" = 3.75)

bites_district <- ggplot(all_bites, aes(x = ttimes_wtd/60, fill = catchment)) +
  geom_linerange(aes(ymin = min_bites/pop*1e5, ymax = max_bites/pop*1e5, color = catchment)) +
  geom_point(aes(y = avg_bites/pop*1e5, shape = factor(dataset), 
                 size = factor(nobs)), color = "grey50", alpha = 0.75) +
  scale_size_manual(values = size_labs, labels = names(size_labs),
                    name = "Number of \nobservations:", 
                    guide = guide_legend(override.aes = list(shape = 21, fill = "grey"))) +
  scale_shape_manual(values = c(25, 21), name = "Dataset:") +
  scale_fill_manual(values = catch_cols, guide = "none") + 
  scale_color_manual(values = catch_cols, guide = "none") +
  ylab("Annual bites per 100k \n at district scale") +
  xlab("Travel times (hrs)") +
  labs(tag = "A") +
  theme_minimal_hgrid() +
  theme(legend.position = "top", legend.box = "vertical")


# Commune bite incidence (Moramanga) ----------------------------------------------------------
bites_commune <- ggplot(mora_bites, aes(x = ttimes_wtd/60)) +
  geom_point(aes(y = avg_bites/pop*1e5, fill = catchment), color = "grey50", shape = 25, size = 4, 
             alpha = 0.75) +
  scale_fill_manual(values = catch_cols, guide = "none") + 
  ylab("Annual bites per 100k\n at commune scale") +
  xlab("Travel times (hrs)") +
  labs(tag = "B") +
  theme_minimal_hgrid() +
  theme(legend.position = "top", legend.box = "vertical")

# fig of bite incidence across scales (RN: M4)
bite_inc_scales <- bites_district / bites_commune
ggsave("figs/main/M4_biteinc.jpeg", height = 7, width = 5)
ggsave("figs/main/M4_biteinc.tiff", dpi = 300, height = 7, width = 5, 
       compression = "lzw", type = "cairo")

# outliers
all_bites %>% 
  filter(ttimes_wtd > 60*3 & avg_bites/pop*1e5 > 150) %>% 
  mutate(bite_inc = avg_bites/pop*1e5) -> outliers
write.csv(outliers, "output/stats/outliers.csv")

# Save session info
out.session(path = "R/figures/M4_biteinc.R", filename = "output/log_local.csv")
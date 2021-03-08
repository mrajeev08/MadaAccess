# ------------------------------------------------------------------------------
#' Processing data to out to access-story for D3.js
#' out to json when possible
#' filter to only one model (national commune)
#' do reporting rather than deaths
# ------------------------------------------------------------------------------
library(jsonlite)
library(sf)
library(dplyr)
library(data.table)
library(tidyr)
library(readr)
library(ggplot2)
library(raster)

test <- st_read("analysis/out/access-story/world.geojson")
plot(test, max.plot = 1)

# out dir
fp <- function(x) here::here("analysis/out/access-story", x)

# Burden data from GARC ----
garc_data <- fromJSON("https://endrabiesnow.org/data/map-data.json")
garc_data %>%
  dplyr::select(iso = CODE, dog_vac = "Dog vaccination coverage", country = Country,
         rabies_risk = `Canine Rabies Risk`, pep = `PEP treatments / year`,
         deaths = `Deaths / year`, costs = `Total costs of canine rabies`) %>%
  write_json(fp("garc_data.json"))

# ctar points ----
ctar_pts <- read_csv("data-raw/out/clinics/ctar_metadata.csv")
ctar_pts %>%
  dplyr::select(CTAR, color, distcode, lat, long) %>%
  st_as_sf(coords = c("long", "lat")) %>%
  st_write(fp("ctar_pts.json"), layer_options = "NATIVE_DATA=YES",
           append = FALSE)

# bite data ----
bites <- read_csv("analysis/out/bites/district_bites.csv")
exps <- read_csv("analysis/out/mods/preds/expectations.csv")
exps %>%
  filter(interaction(data_source, intercept, OD) %in% "National.fixed.TRUE" &
           scale %in% "Commune") %>%
  write_json(fp("mod.json"))

bites %>%
  dplyr::select(distcode, ttimes = ttimes_wtd, catchment, avg_bites) %>%
  write_json(fp("bites.json"))

# circle packing data frame ----
circle_pack <- bind_rows(data.frame(reported = rbinom(100, 1, 0.3), rabid = TRUE),
                         data.frame(reported = rep(1, 60), rabid = FALSE))
write_json(circle_pack, fp("circle_pack.json"))

# Madagascar districts -----
mada_districts <- st_read("analysis/out/shapefiles/mada_districts_simple.shp")
mada_districts %>%
  select(district, distcode, long_cent, lat_cent, catchment, ttimes_wtd,
         pop) -> mada_districts

# Travel times at add + 10, + 50, + 100, + 1000 ----
all_results <- fread("analysis/out/preds/admin_preds.gz")
all_results <- all_results[scenario %in% c(0, 10, 50, 100, 1000)]
all_results_map <- all_results[scale %in% "Commune"]
all_results_map %>%
  mutate(distcode = substr(names, 1, 7)) %>%
  group_by(distcode, scenario) %>%
  summarize(ttimes = mean(ttimes),
            reporting = mean(reporting_mean)) -> districts_summarized

districts_summarized %>%
  pivot_wider(-ttimes, values_from = reporting, names_from = scenario,
              names_prefix = "scen") %>%
  left_join(mada_districts, .) -> mada_districts

st_write(mada_districts, fp("mada_districts.geojson"))

# Madagascar country outline ----
mada_outline <- st_union(mada_districts)
st_write(mada_outline, fp("mada_outline.geojson"))

# Reporting for horizontal scatter plot at both scales ----
# cols should = distcode, district
all_results %>%
  filter(scale %in% "Commune") %>%
  mutate(distcode = substr(names, 1, 7)) %>%
  select(distcode, scale, reporting = reporting_mean, ttimes, scenario) %>%
  bind_rows(mutate(districts_summarized, scale = "District")) %>%
  write_json(fp("reporting.json"))

# Reduction in deaths as clinics are added -----
natl_results <- fread("analysis/out/preds/natl_preds.gz")
natl_results %>%
  filter(scale %in% "Commune") %>%
  select(scenario, deaths_upper, deaths_lower, deaths_mean) %>%
  write_json(fp("natl.json"))

# figure to show population vs. travel times ---


# Figure: raw ttimes + prop pop  ----------------------------------------------------------------
base_times <- raster("analysis/out/ttimes/base/ttimes.tif")
pop1x1 <- raster("data-raw/out/rasters/wp_2015_1x1.tif")
base_df <- as.data.frame(base_times, xy = TRUE)
base_df$pop <- getValues(pop1x1)

ttime_cols <- c("#DEEDCF", "#BFE1B0", "#99D492", "#74C67A", "#56B870",
                "#39A96B", "#188977", "#0E4D64", "#0A2F51")
ttime_breaks <- c(-0.1, 1, 2, 3, 4, 6, 8, 10, 15, Inf)
ttime_labs <- c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6",  "6 - 8", "8 - 10", "10 - 15", "15 +")
names(ttime_cols) <- ttime_labs

pop_vs_ttimes <-
  ggplot() +
  geom_raster(data = base_df, aes(x, y,
                                  fill = cut(((ttimes*pop)/pop)/60, breaks = ttime_breaks,
                                             labels = ttime_labs),
                                  alpha = pop)) +
  scale_fill_manual(values = ttime_cols, na.translate = FALSE, guide = "none",
                    drop = FALSE, na.value = "black") +
  scale_alpha_continuous(range = c(0.2, 1),
                         trans = "log",
                         na.value = 1,
                         guide = "none") +
  cowplot::theme_map() +
  coord_quickmap()

ggsave(fp("pop_ttimes.svg"), pop_vs_ttimes)



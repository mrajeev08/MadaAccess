# ------------------------------------------------------------------------------------------------ #
#' Baseline burden predictions
# ------------------------------------------------------------------------------------------------ #

source(here::here("R", "utils.R"))
start <- Sys.time()

# libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(sf)
library(patchwork)
library(cowplot)

# Predicted burden
burden_preds <- fread(here_safe("analysis/out/preds/admin_preds.gz"))[scenario == 0]
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes_simple.shp"))
mada_districts <- st_read(here_safe("analysis/out/shapefiles/mada_districts_simple.shp"))
ctar_metadata <- fread(here_safe("data-raw/out/clinics/ctar_metadata.csv"))

# Grouped to district
burden_preds$distcode <- mada_communes$distcode[match(burden_preds$names, mada_communes$commcode)]
burden_preds$commcode <- burden_preds$names

burden_preds %>%
  filter(scale == "District") %>%
  group_by(distcode) %>%
  summarize_at(vars(bites_mean:averted_lower, pop), sum, na.rm = TRUE) -> district_deaths
burden_preds %>%
  filter(scale == "District") %>%
  group_by(distcode) %>%
  summarize_at(vars(starts_with("p_rabid"), starts_with("reporting"), starts_with("ttimes")),
    mean,
    na.rm = TRUE
  ) %>%
  left_join(district_deaths) %>%
  mutate(scale = "District") -> district_deaths

burden_to_plot <- bind_rows(burden_preds[scale == "Commune"], district_deaths)

burden_preds %>%
  group_by(scale) %>%
  summarize(natl_inc = sum(deaths_mean, na.rm = TRUE) / sum(pop, na.rm = TRUE) * 1e5) -> natl_inc

# Colors and labs
scale_labs <- c("Commune", "District")
model_cols <- c("darkgrey", "black")
names(scale_labs) <- scale_labs
names(model_cols) <- scale_labs

compare_burden <- ggplot() +
  geom_hline(
    data = natl_inc, aes(yintercept = natl_inc, color = scale), linetype = 1,
    alpha = 0.75, size = 1.2
  ) +
  geom_point(
    data = burden_to_plot,
    aes(
      x = reorder(distcode, ttimes), y = deaths_mean / pop * 1e5,
      fill = ttimes, shape = scale, alpha = scale,
      size = scale, color = scale, stroke = 1.1
    )
  ) +
  scale_fill_viridis_c(
    option = "viridis", direction = 1,
    name = "Travel times \n (hrs)", limits = c(0, 15), oob = scales::squish
  ) +
  scale_shape_manual(
    values = c(22, 23), labels = scale_labs,
    name = "Scale"
  ) +
  scale_alpha_manual(
    values = c(0.85, 1), labels = scale_labs,
    name = "Scale"
  ) +
  scale_size_manual(
    values = c(2.5, 3.5), labels = scale_labs,
    name = "Scale"
  ) +
  scale_color_manual(
    values = model_cols, labels = scale_labs,
    name = "Scale"
  ) +
  labs(
    x = "Districts (ordered by \n increasing travel times)",
    y = "Predicted incidence of \n deaths per 100k", tag = "A"
  ) +
  theme_minimal_hgrid() +
  theme(axis.text.y = element_blank()) +
  coord_flip(clip = "off")

mada_communes %>%
  select(-pop) %>%
  left_join(burden_preds[scale == "Commune"], by = c("commcode" = "commcode")) -> gg_commune_plot

mada_districts %>%
  select(-pop) %>%
  left_join(district_deaths, by = c("distcode" = "distcode")) -> gg_district_plot

col_lim <- c(
  floor(min(gg_commune_plot$deaths_mean / gg_commune_plot$pop * 1e5)),
  ceiling(max(gg_commune_plot$deaths_mean / gg_commune_plot$pop * 1e5))
)

comm_burden <- ggplot() +
  geom_sf(
    data = gg_commune_plot,
    aes(fill = deaths_mean / pop * 1e5),
    color = "white", size = 0.05
  ) +
  geom_point(
    data = ctar_metadata, aes(x = long, y = lat), color = "grey50",
    shape = 4, size = 2, stroke = 1.5
  ) +
  labs(tag = "B") +
  scale_fill_viridis_c(
    option = "magma", direction = -1,
    name = "Predicted incidence \n of deaths per 100k",
    limits = col_lim
  ) +
  theme_map()


district_burden <- ggplot() +
  geom_sf(
    data = gg_district_plot,
    aes(fill = deaths_mean / pop * 1e5),
    color = "white", size = 0.05
  ) +
  geom_point(
    data = ctar_metadata, aes(x = long, y = lat), color = "grey50",
    shape = 4, size = 2, stroke = 1.5
  ) +
  labs(tag = "C") +
  scale_fill_viridis_c(
    option = "magma", direction = -1,
    name = "Predicted incidence \n of deaths per 100k",
    limits = col_lim
  ) +
  theme_map()

burden_base <- (compare_burden | ((comm_burden / district_burden) +
  plot_layout(nrow = 2, guides = "collect"))) +
  plot_layout(widths = c(1, 2))

write_create(
  burden_base,
  "analysis/figs/main/M5_burden_base.jpeg",
  ggsave_it,
  height = 14, width = 10
)
write_create(
  burden_base,
  "analysis/figs/main/M5_burden_base.tiff",
  ggsave_it,
  dpi = 300, device = "tiff", height = 8.75, width = 7.5,
  compression = "lzw", type = "cairo"
)

# Saving session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

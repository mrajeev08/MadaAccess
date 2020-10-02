# ------------------------------------------------------------------------------
#' Main section on travel times
# ------------------------------------------------------------------------------

start <- Sys.time()
source(here::here("R", "utils.R"))

# Set-up
library(ggplot2)
library(dplyr)
library(raster)
library(patchwork)
library(cowplot)
library(data.table)
select <- dplyr::select

# data
ctar_metadata <- read.csv(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
base_times <- raster(here_safe("analysis/out/ttimes/base/ttimes.tif"))
pop1x1 <- raster(here_safe("data-raw/out/rasters/wp_2015_1x1.tif"))
friction_masked <- raster(here_safe("data-raw/out/rasters/friction_mada_masked.tif"))

# Figure: raw ttimes + prop pop  -----------------------------------------------
ttime_cols <- c(
  "#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5",
  "#f768a1", "#dd3497", "#ae017e",
  "#7a0177", "#49006a"
)
ttime_breaks <- c(-0.1, 1, 2, 3, 4, 6, 8, 10, 15, Inf)
ttime_labs <- c(
  "< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 8",
  "8 - 10", "10 - 15", "15 +"
)
names(ttime_cols) <- ttime_labs

base_df <- as.data.frame(base_times, xy = TRUE)

ttimes_A <- ggplot() +
  geom_raster(data = base_df, aes(x, y,
    fill = cut(ttimes / 60,
      breaks = ttime_breaks,
      labels = ttime_labs
    )
  )) +
  scale_fill_manual(
    values = ttime_cols, na.translate = FALSE,
    name = "Travel times \n (hrs)",
    drop = FALSE, na.value = "black", guide = "none"
  ) +
  geom_point(
    data = ctar_metadata, aes(x = long, y = lat), color = "darkgrey",
    shape = 4,
    stroke = 2
  ) +
  theme_map(font_size = 11) +
  labs(tag = "A") +
  coord_quickmap()

# Proportion of pop
baseline_df <- fread(here_safe("analysis/out/ttimes/base/grid_df.gz"))
baseline_df %>%
  filter(!is.na(pop), !is.infinite(ttimes), !is.na(ttimes)) %>%
  mutate(cut_times = cut(ttimes / 60,
    breaks = ttime_breaks,
    labels = ttime_labs
  )) %>%
  group_by(cut_times) %>%
  summarize(prop_pop = sum(prop_pop, na.rm = TRUE)) -> prop_pop_ttimes

prop_B <- ggplot(data = prop_pop_ttimes, aes(
  x = cut_times, y = prop_pop,
  fill = cut_times
)) +
  geom_col(color = "grey") +
  scale_fill_manual(
    values = ttime_cols, na.translate = FALSE, name = "Travel times \n (hrs)",
    drop = FALSE
  ) +
  ylim(c(0, 0.6)) +
  labs(x = "Travel times (hrs)", y = "Proportion of\n population") +
  theme_minimal_hgrid(font_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(tag = "B")

# Comparing ttimes wtd/un/distance ---------------------------------------------
gtruth <- read.csv(here_safe("analysis/out/ttimes/gtruth_ttimes.csv"))

gtruth %>%
  filter(type == "commune_wtd", !is.na(ttimes_reported)) %>%
  group_by(commcode) %>%
  summarize(
    mean = mean(ttimes_reported),
    nobs = n(),
    max = max(ttimes_reported, na.rm = TRUE), min = min(ttimes_reported, na.rm = TRUE),
    upper = quantile(ttimes_reported, probs = 0.975),
    lower = quantile(ttimes_reported, probs = 0.0275)
  ) %>%
  left_join(select(gtruth, type, commcode, ttimes_est, ttimes_un, distance)) %>%
  unique() -> mora_means

gtruth %>%
  filter(type == "point") %>%
  mutate(nobs = 1, mean = ttimes_reported) %>%
  bind_rows(mora_means) -> all

gtruth_comp_C <- ggplot(
  data = all,
  aes(
    x = ttimes_est, y = mean, size = log(nobs + 5), shape = type,
    fill = type
  ), color = "grey50"
) +
  geom_point(alpha = 0.75) +
  geom_linerange(aes(ymax = upper, ymin = lower), size = 1, alpha = 0.5) +
  scale_size_identity(
    breaks = log(c(5, 25, 100) + 5), labels = c("5", "25", "100+"),
    guide = "legend", name = "No. of observations \n (for patient reported \n travel times)"
  ) +
  geom_smooth(method = "lm", aes(color = type, fill = type), alpha = 0.5, size = 1) +
  scale_shape_manual(
    values = c(21, 22), labels = c("Patient reported", "Driving times"),
    name = "Type of travel time estimate"
  ) +
  scale_color_manual(
    values = c("#665565", "#CC575F"),
    labels = c("Patient reported", "Driving times"),
    name = "Type of estimate"
  ) +
  scale_fill_manual(
    values = c("#665565", "#CC575F"),
    labels = c("Patient reported", "Driving times"),
    name = "Type of estimate"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  guides(shape = guide_legend(override.aes = list(linetype = c(0, 0), size = 2))) +
  theme_minimal_hgrid(font_size = 11) +
  labs(
    x = "Estimated travel times (hrs)", y = "Reported travel times \n (hrs)",
    tag = "C"
  )

# combined
figM2_ttimes <- (ttimes_A + (prop_B / gtruth_comp_C)) + plot_layout(widths = c(1.5, 1))
# for plos
write_create(
  figM2_ttimes,
  here_safe("analysis/figs/main/M2_ttimes.jpeg"),
  ggsave_it,
  height = 10, width = 10
)
write_create(
  figM2_ttimes,
  here_safe("analysis/figs/main/M2_ttimes.tiff"),
  ggsave_it,
  dpi = 300, height = 5, width = 7.5,
  compression = "lzw", type = "cairo"
)

# save session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

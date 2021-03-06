# ------------------------------------------------------------------------------------------------ #
#' Correcting for undersubmission of forms & comparing to predicted vial ests
#' Looking at prop of bites reporting within catchment
# ------------------------------------------------------------------------------------------------ #

source(here::here("R", "utils.R"))
start <- Sys.time()

# Libraries
library(ggplot2)
library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(patchwork)
library(cowplot)

select <- dplyr::select

# Read in metadata
ctar_metadata <- fread(here_safe("data-raw/out/clinics/ctar_metadata.csv"))
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes_simple.shp"))
mada_districts <- st_read(here_safe("analysis/out/shapefiles/mada_districts_simple.shp"))
national <- fread(here_safe("data-raw/out/bitedata/national.csv"))

# Catchment maps (commune = fill & district = color ) ------------------------------------------
# colors
catch_cols <- c("#FCC56F","#004D43", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
                "#21abcd", "#B79762", "#FFDBE5", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
                "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF",
                "#F2300F", "#FF2F80","#61615A", "#4A3B53", "#6B7900", "#00C2A0", "#FFAA92",
                "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
names(catch_cols) <- ctar_metadata$CTAR


# Dissolve shapefiles
mada_districts %>%
  group_by(catchment) %>%
  summarize(geometry = st_union(geometry)) -> gg_distcatch
mada_communes %>%
  group_by(catchment) %>%
  summarize(geometry = st_union(geometry)) -> gg_commcatch

catchments_A <- ggplot() +
  geom_sf(data = gg_commcatch, aes(fill = catchment),
               color = NA, alpha = 0.5) +
  geom_sf(data = gg_distcatch, aes(color = catchment),
               fill = NA,  size = 1.2, alpha = 1) +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_color_manual(values = catch_cols, guide = "none") +
  theme_map() +
  labs(tag = "A")

# Plotting proportion of within catch vs. outside catch
baseline_dist <- select(mada_districts, pop_catch)
baseline_dist$scale <- "Districts"
baseline_comm <- select(mada_communes, pop_catch)
baseline_comm$scale <- "Communes"
catch_plot <- bind_rows(baseline_dist, baseline_comm)
catch_plot$scale <- factor(catch_plot$scale)
levels(catch_plot$scale) <- list("Districts" = "Districts", "Communes" = "Communes")

scale_levs <- c("Communes", "Districts")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs

catch_by_scale_B <- ggplot(data = catch_plot, aes(x = pop_catch, fill = scale)) +
  geom_histogram(alpha = 0.75, binwidth = 0.1, color = "white", size = 1) +
  facet_wrap(~scale, scales = "free_y") +
  scale_fill_manual(values = model_cols, guide = "none") +
  labs(x = "Proportion of population \n within assigned catchment",
       y = "Number of \n admin units", tag = "B") +
  theme_half_open() +
  theme(strip.background = element_blank())

national %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = sum(no_patients, na.rm = TRUE)) %>%
  filter(!is.na(id_ctar), distcode %in% mada_districts$distcode) -> bites_dist
bites_dist$catch <- mada_districts$catchment[match(bites_dist$distcode, mada_districts$distcode)]
bites_dist$actual_catch <- ctar_metadata$id_ctar[match(bites_dist$catch, ctar_metadata$CTAR)]

bites_dist %>%
  group_by(id_ctar) %>%
  mutate(in_catch = ifelse(actual_catch == id_ctar, 1, 0)) %>%
  summarize(total = sum(count),
            bites_catch = sum(count[in_catch == 1], na.rm = TRUE),
            prop_in_catch = bites_catch/total) %>%
  filter(total > 10) -> prop_dist

moramanga <- fread(here_safe("data-raw/out/bitedata/moramanga.csv"))
moramanga$catch <- mada_communes$catchment[match(moramanga$commcode, mada_communes$commcode)]
moramanga$actual_catch <- ctar_metadata$id_ctar[match(moramanga$catch, ctar_metadata$CTAR)]


catch_by_data_C <- ggplot(data = prop_dist, aes(x = prop_in_catch)) +
  geom_histogram(breaks = c(seq(0, 1, by = 0.1)), color = "white", size = 3, fill = "#35274A",
                 alpha = 0.75) +
  geom_vline(xintercept = sum(moramanga$actual_catch %in% "8")/nrow(moramanga),
             color = "#0B775E", alpha = 0.75,
             linetype = 1) +
  labs(x = "Proportion of bites \n from within assigned catchment", y = "Number of clinics",
       tag = "C") +
  theme_half_open()

# Output figure
catch_props <- catchments_A | (catch_by_scale_B / catch_by_data_C)

write_create(
  catch_props,
  here_safe("analysis/figs/supplementary/S2.1_catchments.jpeg"),
  ggsave_it,
  height = 7,
  width = 10
)

# Plotting sensitivity to rle cut-offs ---------------------------------------------------------
# throughput and reporting
throughput <- read.csv(here_safe("analysis/out/sensitivity/throughput.csv"))
reporting <- read.csv(here_safe("analysis/out/sensitivity/reporting.csv"))

# colors
catch_cols <- c("#FCC56F","#004D43", "#7A4900", "#CBCDD2", "#006b3c", "#EFF2F1",
                "#21abcd", "#B79762", "#FFDBE5", "#8FB0FF", "#997D87", "#FD9C54", "#8362B5",
                "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF",
                "#F2300F", "#FF2F80","#61615A", "#4A3B53", "#6B7900", "#00C2A0", "#FFAA92",
                "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
names(catch_cols) <- ctar_metadata$CTAR
throughput_cols <- c("0" = "white", "< 10" = '#edf8fb', "< 20" = '#b3cde3',
                     "< 40" = '#8c96c6', "< 80" = '#8856a7', "< 100" = '#810f7c')
throughput_brks <- c(-0.1, 0.1, 10, 20, 40, 80, 100)
throughput$date_reported <- ymd(throughput$date_reported)
throughput$include_15[throughput$include_15 == 0] <- NA
throughput$include_15[is.na(throughput$no_patients) & throughput$include_15 == 1] <- 0

throughput_A <- ggplot(data = throughput, aes(x = date_reported, y = reorder(ctar, include_15))) +
  geom_tile(aes(fill = no_patients)) +
  xlim(ymd("2014-01-01"), ymd("2017-12-31")) +
  scale_fill_distiller(trans = "sqrt", breaks = c(0, 10, 50), direction = 1,
                       name = "Number of patients") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  xlab("Year") +
  ylab("Clinic provisioning PEP") +
  labs(tag = "A")

reporting %>%
  group_by(ctar) %>%
  summarize(min = min(include_15), max = max(include_15),
            include_15 = mean(include_15),) -> rep_summ

rep_B <- ggplot(data = rep_summ, aes(x = reorder(ctar, include_15), y = include_15,
                                              color = ctar)) +
  geom_linerange(aes(ymin = min, ymax = max, color = ctar)) +
  geom_point(data = reporting, aes(x = ctar, y = include_15, color = ctar),
             alpha = 0.5, size = 3) +
  labs(y = "Estimated submission \n of forms", x = "", tag = "B") +
  geom_hline(yintercept = 0.25, linetype = 2, color = "grey") +
  coord_flip() +
  scale_color_manual(values = catch_cols, guide = "none") +
  theme_minimal_hgrid() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Estimates of vials from reporting -----------------------------------------------------------
vial_comp <- read.csv("analysis/out/sensitivity/vial_comp.csv")
vials_C <- ggplot(data = vial_comp, aes(x = reorder(ctar, reporting), shape = factor(cut_off),
                                             color = ctar)) +
  geom_point(aes(y = ((log(mean3) + log(mean4))/2) - log(vials_observed)), size = 2) +
  geom_linerange(aes(ymin = log(mean3) - log(vials_observed), ymax = log(mean4) - log(vials_observed))) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  scale_shape_manual(values = c(0, 1), name = "Correction for \n undersubmission",
                     labels = c("15" = "15 day cut-off", "Inf" = "none")) +
  labs(y = "Difference between \n log(estimated) and log(observed) vials", x = "", tag = "C") +
  coord_flip() +
  scale_color_manual(values = catch_cols, guide = "none") +
  theme_minimal_hgrid()

figS2.2_vialcomps <-
  {(throughput_A | rep_B) + plot_layout(ncol = 2, widths = c(2.5, 1))} /
  {(vials_C + plot_spacer()) + plot_layout(ncol = 2, widths = c(3, 1))}

write_create(
  figS2.2_vialcomps,
  here_safe("analysis/figs/supplementary/S2.2_vialcomps.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 8,
  width = 10
)

#' Saving session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)


# ------------------------------------------------------------------------------
#' Looking at reasonable range of exposure incidence
#' Pulling in district and commune estimates of travel times as clinics
#' are added
# ------------------------------------------------------------------------------

source(here::here("R", "utils.R"))
start <- Sys.time()

# Set up
library(data.table)
library(ggplot2)
library(dplyr)
library(cowplot)
library(foreach)
library(iterators)
library(sf)
library(patchwork)
library(glue)
select <- dplyr::select
source(here_safe("R/predict_functions.R"))

# From Claire's MSC
range_people_ph <- c(4.13, 5.654)
range_dogs_ph <- c(0.24, 0.26)

# Range of hdrs then
mora_hdrs <- range_people_ph / range_dogs_ph

# Helper functions for getting inc from hdr & vice versa
hdr_from_inc <-
  function(inc = 48,
           pop = 1e5,
           p_exp = 0.39,
           dog_inc = 0.01) {
    1 / (inc / pop / dog_inc / p_exp)
  }

inc_from_hdr <-
  function(hdr = 5,
           pop = 1e5,
           p_exp = 0.39,
           dog_inc = 0.01) {
    p_exp * dog_inc * pop / hdr
  }

# incidence from hdrs of Moramanga
inc_from_hdr(hdr = 5)
inc_from_hdr(hdr = 25)

# hdrs from exposure incidence estimated
hdr_from_inc(inc = 76)
hdr_from_inc(inc = 110)

# How to show relationship between these things ...
var_df <- expand.grid(
  hdr_val = seq(2, 40, by = 1),
  p_exp = 0.38,
  dog_inc = seq(0.005, 0.02, by = 0.001)
)

var_df %>%
  mutate(exp_inc = inc_from_hdr(
    hdr = hdr_val,
    p_exp = p_exp,
    dog_inc = dog_inc
  )) -> inc_exps

inc_labs <-
  as_labeller(
    c(
      `0.38` = "p[exp] == 0.38",
      `0.2` = "p[exp] == 0.2",
      `0.5` = "p[exp] == 0.5"
    ),
    label_parsed
  )

S4.1_exprange <-
  ggplot(data = inc_exps, aes(
    x = hdr_val,
    y = dog_inc,
    fill = cut(
      inc_exps$exp_inc,
      breaks = c(1, 15, 42, 76, 110, Inf),
      labels = c("1 - 15", "15 - 42", "42 - 76", "76 - 110", "110 +")
    ),
    color = ifelse(exp_inc > 42 &
      exp_inc < 110, "red", NA)
  )) +
  geom_tile() +
  geom_hline(yintercept = 0.01, color = "red") +
  geom_vline(xintercept = c(5, 25), linetype = 2) +
  geom_vline(
    xintercept = round(mora_hdrs),
    linetype = 2,
    color = "grey50"
  ) +
  scale_fill_brewer(name = "Human exposures \n per 100k") +
  labs(x = "Human:dog ratio", y = "Dog rabies incidence (annual)") +
  scale_color_identity(guide = "none") +
  theme_minimal_grid()

write_create(
  S4.1_exprange,
  here_safe("analysis/figs/supplementary/S4.1_exprange.jpeg"),
  ggsave_it,
  height = 8,
  width = 8
)

# Relationship + map for each scale & direction --------------------------------
scaling_df <- fread(here_safe("analysis/out/sensitivity/scaling.csv"))
mada_districts <- st_read(here_safe("analysis/out/shapefiles/mada_districts_simple.shp"))
mada_communes <- st_read(here_safe("analysis/out/shapefiles/mada_communes_simple.shp"))

# Scale
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs
names(model_cols) <- scale_levs

# Scaled incidence hypothetical
pop_vals <- seq(1000, 1e6, by = 10)
foreach(j = iter(scaling_df, by = "row"), .combine = rbind) %do% {
  inc_scaled <- constrained_inc(
    slope = j$sfactor,
    pop = pop_vals - j$trans,
    max = 76 / 1e5, min = 15 / 1e5
  )
  out <- data.table(pop = pop_vals, inc_scaled, j)
} -> inc_scaled_rel

# Scaled incidence at district & commune level
foreach(j = iter(scaling_df, by = "row"), .combine = rbind) %do% {
  if (j$scale == "Commune") {
    names <- mada_communes$commcode
    pop <- mada_communes$pop
  }

  if (j$scale == "District") {
    names <- mada_districts$distcode
    pop <- mada_districts$pop
  }

  inc_scaled <- constrained_inc(
    slope = j$sfactor,
    pop = pop - j$trans,
    max = 76 / 1e5, min = 15 / 1e5
  )
  out <- data.table(names, pop, inc_scaled, j)
} -> inc_scaled_admin

mada_communes$scale <- "Commune"
mada_districts$scale <- "District"
gg_all <- bind_rows(mada_communes, mada_districts)
gg_all %>%
  mutate(id = case_when(
    scale == "Commune" ~ commcode,
    scale == "District" ~ distcode
  )) %>%
  left_join(inc_scaled_admin, by = c("scale" = "scale", "id" = "names")) -> gg_all

scaling_labs <- c(neg = "Incidence decreases \n with pop", pos = "Incidence increases \n with pop")

neg_rel <- ggplot(data = filter(inc_scaled_rel, scaling == "neg")) +
  geom_line(aes(x = pop, y = inc_scaled * 1e5, color = scale), size = 2, alpha = 0.75) +
  geom_jitter(
    data = filter(inc_scaled_admin, scaling == "neg"),
    aes(x = pop, y = inc_scaled * 1e5, color = scale), alpha = 0.5,
    shape = 1, size = 2
  ) +
  scale_x_continuous(trans = "log", breaks = c(1000, 1e4, 1e5, 1e6)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale", guide = "none") +
  labs(x = "Population", y = "Exposure incidence \n per 100k", tag = "A") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

neg_map <- ggplot(data = filter(gg_all, scaling == "neg")) +
  geom_sf(aes(alpha = inc_scaled * 1e5, fill = scale), color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_alpha_continuous(
    range = c(0.15, 1), breaks = c(0, 15, 25, 50, 76),
    name = "Exposures \n per 100k",
    guide = guide_legend(override.aes = list(fill = "#0B775E"))
  ) +
  facet_grid(scaling ~ scale, labeller = labeller(scaling = scaling_labs)) +
  theme_map()

neg_A <- neg_rel + neg_map + plot_layout(widths = c(1, 2))

pos_rel <- ggplot(data = filter(inc_scaled_rel, scaling == "pos")) +
  geom_line(aes(x = pop, y = inc_scaled * 1e5, color = scale), size = 2, alpha = 0.75) +
  geom_jitter(
    data = filter(inc_scaled_admin, scaling == "pos"),
    aes(x = pop, y = inc_scaled * 1e5, color = scale), alpha = 0.5, shape = 1,
    size = 2
  ) +
  scale_x_continuous(trans = "log", breaks = c(1000, 1e4, 1e5, 1e6)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  labs(x = "Population", y = "Exposure incidence \n per 100k", tag = "B") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pos_map <- ggplot(data = filter(gg_all, scaling == "pos")) +
  geom_sf(aes(alpha = inc_scaled * 1e5, fill = scale), color = NA) +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_alpha_continuous(
    range = c(0.15, 1), breaks = c(0, 15, 25, 50, 76),
    name = "Exposures \n per 100k",
    guide = guide_legend(override.aes = list(fill = "#35274A"))
  ) +
  facet_grid(scaling ~ scale, labeller = labeller(scaling = scaling_labs)) +
  theme_map() +
  theme(strip.text.x = element_blank())

pos_B <- pos_rel + pos_map + plot_layout(widths = c(1, 2))

# fig S6.4
scaling_rels <- (neg_A / pos_B) + plot_layout(guides = "collect")
write_create(scaling_rels,
             "analysis/figs/supplementary/S4.2_scaling_rels.jpeg",
             ggsave_it,
             width = 10, height = 10
)

# Saving session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

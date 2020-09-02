# ------------------------------------------------------------------------------------------------ #
#' Scaling sensitivity analyses
# ------------------------------------------------------------------------------------------------ #

source("R/functions/out.session.R")
start <- Sys.time()

# Libraries and scripts
library(data.table)
library(foreach)
library(iterators)
library(sf)
library(tidyverse)
library(cowplot)
library(patchwork)
library(glue)
source("R/functions/predict_functions.R")

# Relationship + map for each scale & direction ---------------------------------
scaling_df <- fread("output/sensitivity/scaling.csv")
mada_districts <- st_read("data/processed/shapefiles/mada_districts_simple.shp")
mada_communes <- st_read("data/processed/shapefiles/mada_communes_simple.shp")

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
  mutate(id = case_when(scale == "Commune" ~ commcode, 
                        scale == "District" ~ distcode)) %>%
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
S6.4_scaling_rels <- (neg_A / pos_B) + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.4_scaling_rels.jpeg", S6.4_scaling_rels, width = 10, height = 10)

# Baseline deaths mean ----------------------------------------------------------
baseline <- fread("output/preds/admin_preds.gz")[scenario == 0]
base_scaled <- fread("output/sensitivity/burden_baseline_scaled.gz")
add_scaled <- fread("output/sensitivity/burden_addclinics_scaled.gz")
add_base <- fread("output/preds/natl_preds.gz")

# Baseline ----------------------------------
scaling_labs <- c(
  neg = "Incidence decreases \n with pop", pos = "Incidence increases \n with pop",
  base = "Baseline (no scaling)"
)

base_scaled %>%
  select(-(p_rabid_mean:rabid_exps_lower)) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  filter(grepl("deaths", name)) %>%
  pivot_wider(
    id_cols = c(scale, scenario, names, scaling, catch, pop, ttimes),
    names_from = name
  ) -> base_summ

baseline %>%
  select(-(p_rabid_mean:rabid_exps_lower)) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  filter(grepl("deaths", name)) %>%
  mutate(scaling = "base") %>%
  pivot_wider(
    id_cols = c(scale, scenario, names, scaling, catch, pop, ttimes),
    names_from = name
  ) %>%
  bind_rows(base_summ) -> base_se

# order them correctly
base_se$scaling <- factor(base_se$scaling, levels = c("neg", "base", "pos"))
base_scaling_A <- ggplot(
  data = base_se,
  aes(
    x = ttimes, color = scale, y = deaths_mean / pop * 1e5,
    group = interaction(scale, scaling), shape = scaling
  )
) +
  geom_pointrange(aes(ymin = deaths_lower / pop * 1e5, ymax = deaths_upper / pop * 1e5),
    alpha = 0.5, size = 0.1, fatten = 10
  ) +
  facet_grid(rows = "scale", scales = "free_x") +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_shape_manual(
    values = c(6, 1, 2),
    labels = scaling_labs, name = "Scaling \n of incidence"
  ) +
  theme_minimal_hgrid() +
  labs(x = "Travel times (hrs)", y = "Deaths per 100k", tag = "A") +
  theme(strip.text.y = element_blank())

# Add ARMC level ----------------------------------
add_scaled %>%
  pivot_longer(bites_mean:averted_lower) %>%
  filter(grepl("deaths", name)) %>%
  pivot_wider(
    id_cols = c(scale, scenario, scaling),
    names_from = name
  ) -> add_summ

add_base %>%
  pivot_longer(bites_mean:averted_lower) %>%
  filter(grepl("deaths", name)) %>%
  mutate(scaling = "base") %>%
  pivot_wider(
    id_cols = c(scale, scenario, scaling),
    names_from = name
  ) %>%
  bind_rows(add_summ) -> add_se

add_se %>%
  group_by(scaling, scale) %>%
  arrange(scenario) %>%
  mutate(
    deaths_mean = deaths_mean / deaths_mean[1],
    deaths_upper = deaths_upper / deaths_upper[1],
    deaths_lower = deaths_lower / deaths_lower[1]
  ) -> add_props

add_se$scaling <- factor(add_se$scaling, levels = c("neg", "base", "pos"))

add_scaling_B <- ggplot(
  data = filter(add_props, scenario != max(scenario)),
  aes(
    x = scenario, y = deaths_mean,
    color = scale, fill = scale, shape = scaling
  )
) +
  geom_pointrange(aes(ymin = deaths_lower, ymax = deaths_upper),
    alpha = 0.25,
    size = 0.1, fatten = 10
  ) +
  geom_pointrange(
    data = filter(add_props, scenario == max(scenario)),
    aes(
      x = scenario, y = deaths_mean, ymin = deaths_lower,
      ymax = deaths_upper, shape = scaling
    ),
    position = position_dodge(width = 50), show.legend = FALSE
  ) +
  facet_grid(rows = "scale", labeller = labeller(scaling = scaling_labs)) +
  scale_color_manual(
    values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
    name = "Scale", guide = "none",
  ) +
  scale_shape_manual(
    values = c(6, 1, 2),
    labels = scaling_labs, name = "Scaling \n of incidence"
  ) +
  theme_minimal_grid() +
  labs(x = "# Additional ARMC", y = "Proportion of deaths \n compared to baseline", tag = "B") +
  theme(strip.text.y = element_blank())

# fig S6.5
S6.5_scaling_se <- (base_scaling_A / add_scaling_B) + plot_layout(heights = c(1.5, 2), guides = "collect")
ggsave("figs/supplementary/S6.5_scaling_se.jpeg", S6.5_scaling_se, width = 10, height = 10)

# Saving session info
out.session(path = "R/figures/SM6_scaling_se.R", filename = "output/log_local.csv", start = start)

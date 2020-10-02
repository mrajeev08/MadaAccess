# ------------------------------------------------------------------------------
#' Scaling sensitivity analyses
# ------------------------------------------------------------------------------

start <- Sys.time()
source(here::here("R", "utils.R"))

# pkgs
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(patchwork)
source(here_safe("R/predict_functions.R"))

# Baseline deaths mean ---------------------------------------------------------
baseline <- fread(here_safe("analysis/out/preds/admin_preds.gz"))[scenario == 0]
base_scaled <- fread(here_safe("analysis/out/sensitivity/burden_baseline_scaled.gz"))
add_scaled <- fread(here_safe("analysis/out/sensitivity/burden_addclinics_scaled.gz"))
add_base <- fread(here_safe("analysis/out/preds/natl_preds.gz"))

# Baseline ----------------------------------
scaling_labs <- c(
  neg = "Incidence decreases \n with pop", pos = "Incidence increases \n with pop",
  base = "Baseline (no scaling)"
)

# Scale
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs
names(model_cols) <- scale_levs

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

# fig S6.4
scaling_se <- (base_scaling_A / add_scaling_B) + plot_layout(heights = c(1.5, 2),
                                                             guides = "collect")

write_create(
  scaling_se,
  here_safe("analysis/figs/supplementary/S6.4_scaling_se.jpeg"),
  ggsave_it,
  width = 10,
  height = 10
)

# Saving session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

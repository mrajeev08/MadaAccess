# ------------------------------------------------------------------------------
#' Univariate sensitivity analyses
# ------------------------------------------------------------------------------

start <- Sys.time()
source(here::here("R", "utils.R"))

# Libraries and scripts
library(data.table)
library(tidyr)
library(forcats)
library(ggplot2)
library(dplyr)
library(cowplot)
library(patchwork)
library(glue)

# Baseline burden (univariate) ----------------------------------------------------------------
base_se_admin <- fread(here_safe("analysis/out/sensitivity/burden_baseline_se.gz"))
baseline <- fread(here_safe("analysis/out/preds/admin_preds.gz"))[scenario == 0]
se_pars <-  fread(here_safe("analysis/out/sensitivity/se_pars.csv"))

# At national level ----------------------------------------------------------------
# Labels and colors
par_labs <- c(
  "beta_ttimes" = bquote(beta[t]), "beta_0" = bquote(beta[0]),
  "sigma_e" = bquote(sigma[e]),
  "rho_max" = bquote(rho[max]), "p_rabid" = bquote(p[rabid]),
  "p_death" = bquote(p[death]), "human_exp" = bquote(E[i])
)

# Scale
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs
names(model_cols) <- scale_levs

base_se_admin %>%
  group_by(scale, vary, direction) %>%
  summarize_at(vars(bites_mean:averted_lower), sum) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  pivot_wider(id_cols = c(scale, vary, name), names_from = direction) -> base_summ

baseline %>%
  group_by(scale) %>%
  summarize_at(vars(bites_mean:averted_lower), sum) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  left_join(base_summ) -> base_se

# order them correctly
base_se$vary <- factor(base_se$vary, levels = rev(c(
  "beta_ttimes", "beta_0", "sigma_e", "rho_max",
  "p_rabid", "p_death", "human_exp"
)))

# range of pars used
se_pars %>%
  pivot_longer(exp_min:beta_0) %>%
  mutate(value = case_when(name %in% c("exp_min", "exp_max") ~ round(value*1e5, 2),
                           !(name %in% c("exp_min", "exp_max")) ~ round(value, 2))) %>%
  filter(!(name %in% c("exp_max", "p_rab_max"))) %>%
  mutate(name = case_when(name %in% c("exp_min") ~ "human_exp",
                          name %in% c("p_rab_min") ~ "p_rabid",
                          TRUE ~ name)) %>%
  group_by(scale, par = name) %>%
  summarize(upper = max(value), lower = min(value),
            dummy = "") -> pars
# order them correctly & name them correctly
pars$par <- factor(pars$par,
                   levels = c("beta_ttimes", "beta_0", "sigma_e", "rho_max",
                              "p_rabid", "p_death", "human_exp")
)
pars$par <- fct_recode(pars$par,
                       `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
                       `sigma[e]` = "sigma_e", `rho[max]` = "rho_max",
                       `p[rabid]` = "p_rabid", `p[death]` = "p_death",
                       `E[i]` = "human_exp"
)

base_pars_A <-
  ggplot(
    data = pars,
    aes(x = dummy, color = scale, fill = scale)
  ) +
  geom_errorbar(aes(ymax = upper, ymin = lower), position = position_dodge(width = 0.1)) +
  scale_y_continuous(labels = function(y) round(y, 2)) +
  scale_color_manual(values = model_cols, guide = "none") +
  facet_wrap(~par, ncol = 1, scales = "free", strip.position = "left",
             labeller = labeller(par = label_parsed)) +
  coord_flip() +
  labs(y = "Range used", x = "Parameter", tag = "A") +
  theme_minimal_hgrid() +
  theme(strip.text.y.left = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 10),
        panel.spacing = unit(0.01, "lines"),
        )

base_natl_B <- ggplot(
  data = filter(base_se, name == "deaths_mean"),
  aes(x = vary, y = value, color = scale, fill = scale)
) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + # dummy to get legend right
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = rep("", length(par_labs))) +
  scale_color_manual(
    values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
    name = "Scale"
  ) +
  coord_flip() +
  labs(y = "Average deaths (national)", x = "", tag = "B") +
  theme_minimal_grid()

# At admin level ----------------------------------------------------------------
base_se_admin %>%
  mutate(deaths_mean = deaths_mean / pop * 1e5) %>%
  select(names, scale, ttimes, deaths_mean, vary, direction) %>%
  pivot_wider(
    id_cols = c(names, scale, vary, ttimes), names_from = direction,
    values_from = deaths_mean
  ) -> base_admin_summ
baseline %>%
  mutate(deaths_mean = deaths_mean / pop * 1e5, direction = "base") %>%
  select(names, scale, ttimes, deaths_mean, direction) %>%
  pivot_wider(
    id_cols = c(names, scale, ttimes), names_from = direction,
    values_from = deaths_mean
  ) %>%
  left_join(base_admin_summ) -> base_admin_summ

# order them correctly & name them correctly
base_admin_summ$vary <- factor(base_admin_summ$vary,
  levels = c(
    "beta_ttimes", "beta_0", "sigma_e", "rho_max",
    "p_rabid", "p_death", "human_exp"
  )
)
base_admin_summ$vary <- fct_recode(base_admin_summ$vary,
  `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
  `sigma[e]` = "sigma_e", `rho[max]` = "rho_max",
  `p[rabid]` = "p_rabid", `p[death]` = "p_death",
  `E[i]` = "human_exp"
)
base_admin_C <- ggplot(data = base_admin_summ, aes(x = ttimes, y = base, color = scale)) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  scale_color_manual(
    values = model_cols, aesthetics = c("color", "fill"),
    labels = scale_labs, guide = "none"
  ) +
  facet_grid(vary ~ scale,
    scales = "free_x",
    labeller = labeller(scale = c("Commune" = "", "District" = ""), vary = label_parsed)
  ) +
  labs(y = "Deaths per 100k", x = "Travel times (hrs)", tag = "C") +
  theme_minimal_hgrid() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  )

S6.1_base_se <- base_pars_A +
  base_natl_B +
  base_admin_C +
  plot_layout(guides = "collect", widths = c(1.25, 2, 2))

write_create(S6.1_base_se,
  here_safe("analysis/figs/supplementary/S6.1_base_se.jpeg"),
  ggsave_it,
  height = 8, width = 10
)

# Baseline burden (univariate) ----------------------------------------------------------------
add_se <- fread(here_safe("analysis/out/sensitivity/burden_addclinics_se.gz"))
add_base <- fread(here_safe("analysis/out/preds/natl_preds.gz"))

add_se %>%
  pivot_longer(bites_mean:averted_lower) %>%
  pivot_wider(id_cols = c(scale, vary, scenario, data_source, name), names_from = direction) -> add_summ

add_base %>%
  pivot_longer(bites_mean:averted_lower) %>%
  left_join(add_summ) -> add_se

add_se %>%
  group_by(scale, vary, name) %>%
  arrange(scenario) %>%
  mutate(value = value / value[1], max = max / max[1], min = min / min[1]) -> add_props

# Max reduction in deaths --------------------------------------------------------------
add_props %>%
  filter(scenario == max(scenario)) -> max_props
max_props$vary <- factor(max_props$vary,
  levels = rev(c(
    "beta_ttimes", "beta_0", "sigma_e", "rho_max",
    "p_rabid", "p_death", "human_exp"
  ))
)
addARMC_A <- ggplot(
  data = filter(max_props, name == "deaths_mean"),
  aes(x = vary, y = value, color = scale, fill = scale)
) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min),
    position = position_dodge(width = 0.25)
  ) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(
    values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
    name = "Scale"
  ) +
  coord_flip() +
  labs(y = "Propotion of deaths \n compared to baseline", x = "Parameter", tag = "A") +
  theme_minimal_grid()

# As each clinic is added --------------------------------------------------------------

# need to filter to below max!
add_props$vary <- factor(add_props$vary,
  levels = c(
    "beta_ttimes", "beta_0", "sigma_e", "rho_max",
    "p_rabid", "p_death", "human_exp"
  )
)
# order them correctly & name them correctly
add_props$vary <- fct_recode(add_props$vary,
  `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
  `sigma[e]` = "sigma_e", `rho[max]` = "rho_max",
  `p[rabid]` = "p_rabid", `p[death]` = "p_death",
  `E[i]` = "human_exp"
)

addARMC_B <- ggplot(
  data = filter(add_props, name == "deaths_mean", scenario != max(scenario)),
  aes(x = scenario, y = value, color = scale)
) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  geom_pointrange(
    data = filter(add_props, scenario == max(scenario), name == "deaths_mean"),
    aes(
      x = scenario, y = value,
      ymin = min, ymax = max, color = scale
    )
  ) +
  scale_color_manual(
    values = model_cols, aesthetics = c("color", "fill"),
    labels = scale_labs, guide = "none"
  ) +
  facet_grid(vary ~ scale,
    scales = "free_x",
    labeller = labeller(
      scale = c("Commune" = "", "District" = ""),
      vary = label_parsed
    )
  ) +
  labs(
    x = "", y = "Propotion of deaths \n compared to baseline",
    tag = "B"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  )

S6.2_addARMC_se <- addARMC_A + addARMC_B + plot_layout(guides = "collect")
write_create(S6.2_addARMC_se,
  "analysis/figs/supplementary/S6.2_addARMC_se.jpeg",
  ggsave_it,
  height = 8, width = 10
)

# Sensitivity of vials ---------------------------------------------------------------------
vials_se <- fread(here_safe("analysis/out/sensitivity/vials_se.gz"))
vials_base <- fread(here_safe("analysis/out/preds/natl_preds.gz"))

vials_se %>%
  pivot_longer(vials_mean:vials_lower) %>%
  pivot_wider(
    id_cols = c(scale, vary, name, scenario),
    names_from = direction
  ) -> vials_summ
vials_base %>%
  pivot_longer(vials_mean:vials_lower) %>%
  left_join(vials_summ) -> vials_se


# Vials baseline -----------------------------------------------------------------
vials_base <- filter(vials_se, scenario == 0)
vials_base$vary <- factor(vials_base$vary,
  levels = rev(c("beta_ttimes", "beta_0", "sigma_e"))
)

vial_se_A <- ggplot(
  data = filter(vials_base, name == "vials_mean"),
  aes(x = vary, y = value, color = scale, fill = scale)
) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min),
    position = position_dodge(width = 0.25)
  ) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(
    values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
    name = "Scale"
  ) +
  coord_flip() +
  labs(y = "Baseline vial demand (national)", x = "Parameter", tag = "A") +
  theme_minimal_grid()

# Increase in vials --------------------------------------------------------------
# order them correctly & name them correctly
vials_se$vary <- factor(vials_se$vary,
  levels = c("beta_ttimes", "beta_0", "sigma_e")
)
vials_se$vary <- fct_recode(vials_se$vary,
  `sigma[e]` = "sigma_e", `beta[0]` = "beta_0",
  `beta[t]` = "beta_ttimes"
)

vial_se_B <- ggplot(
  data = filter(
    vials_se, name == "vials_mean",
    scenario != max(scenario)
  ),
  aes(x = scenario, y = value, color = scale)
) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  geom_pointrange(
    data = filter(
      vials_se, name == "vials_mean",
      scenario == max(scenario)
    ),
    aes(
      x = scenario, y = value,
      ymin = min, ymax = max, color = scale
    )
  ) +
  scale_color_manual(
    values = model_cols, aesthetics = c("color", "fill"),
    labels = scale_labs, guide = "none"
  ) +
  facet_grid(vary ~ scale,
    scales = "free_x",
    labeller = labeller(scale = c("Commune" = "", "District" = ""), vary = label_parsed)
  ) +
  labs(
    x = "# Additional clinics \n provisioning PEP", y = "Increase in vial demand (national)",
    tag = "B"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  )

S6.3_vials_se <- vial_se_A + vial_se_B + plot_layout(guides = "collect")
write_create(S6.3_vials_se,
  "analysis/figs/supplementary/S6.3_vial_se.jpeg",
  ggsave_it,
  height = 8, width = 10
)

# Saving session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

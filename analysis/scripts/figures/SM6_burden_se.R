# ------------------------------------------------------------------------------------------------ #
#' Univariate sensitivity analyses     
# ------------------------------------------------------------------------------------------------ #

source("R/functions/out.session.R")
start <- Sys.time()

# Libraries and scripts
library(data.table)
library(tidyverse)
library(cowplot)
library(patchwork)
library(glue)

# Baseline burden (univariate) ----------------------------------------------------------------
base_se_admin <- fread("output/sensitivity/burden_baseline_se.gz")
baseline <- fread("output/preds/admin_preds.gz")[scenario == 0]

# At national level ----------------------------------------------------------------
# Labels and colors
par_labs <- c("beta_ttimes" = bquote(beta[t]), "beta_0" = bquote(beta[0]),
              "sigma_e" = bquote(sigma[e]),
              "rho_max" = bquote(rho[max]), "p_rabid" = bquote(p[rabid]),
              "p_death" = bquote(p[death]), "human_exp" = bquote(E[i]))

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
  pivot_wider(id_cols = c(scale, vary, name), names_from = direction)-> base_summ

baseline %>%
  group_by(scale) %>%
  summarize_at(vars(bites_mean:averted_lower), sum) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  left_join(base_summ) -> base_se

# order them correctly
base_se$vary <- factor(base_se$vary, levels = rev(c("beta_ttimes", "beta_0", "sigma_e", "rho_max", 
                                                    "p_rabid", "p_death", "human_exp")))

base_natl_A <- ggplot(data = filter(base_se, name == "deaths_mean"), 
                      aes(x = vary, y = value, color = scale, fill = scale)) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + # dummy to get legend right
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
                     name = "Scale") +
  coord_flip() +
  labs(y = "Average deaths (national)", x = "Parameter", tag = "A") +
  theme_minimal_grid()

# At admin level ----------------------------------------------------------------
base_se_admin %>%
  mutate(deaths_mean = deaths_mean/pop*1e5) %>%
  select(names, scale, ttimes, deaths_mean, vary, direction) %>%
  pivot_wider(id_cols = c(names, scale, vary, ttimes), names_from = direction, 
              values_from = deaths_mean) -> base_admin_summ 
baseline %>%
  mutate(deaths_mean = deaths_mean/pop*1e5, direction = "base") %>%
  select(names, scale, ttimes, deaths_mean, direction) %>%
  pivot_wider(id_cols = c(names, scale, ttimes), names_from = direction, 
              values_from = deaths_mean) %>%
  left_join(base_admin_summ) -> base_admin_summ

# order them correctly & name them correctly
base_admin_summ$vary <- factor(base_admin_summ$vary, 
                               levels = c("beta_ttimes", "beta_0", "sigma_e", "rho_max", 
                                          "p_rabid", "p_death", "human_exp"))
base_admin_summ$vary <- fct_recode(base_admin_summ$vary, 
                                   `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
                                   `sigma[e]` = "sigma_e", `rho[max]` = "rho_max", 
                                   `p[rabid]` = "p_rabid", `p[death]` = "p_death" ,
                                   `E[i]` = "human_exp")
base_admin_B <- ggplot(data = base_admin_summ, aes(x = ttimes, y = base, color = scale)) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  scale_color_manual(values = model_cols, aesthetics = c("color", "fill"),
                     labels = scale_labs, guide = "none") +
  facet_grid(vary ~ scale, scales = "free_x",
             labeller = labeller(scale = c("Commune" = "", "District" = ""), vary = label_parsed)) +
  labs(y = "Deaths per 100k", x = "Travel times (hrs)", tag = "B") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
        panel.grid = element_line(color = "white", size = 0.5))


S6.1_base_se <- base_natl_A + base_admin_B + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.1_base_se.jpeg", S6.1_base_se, height = 8, width = 10)

# Baseline burden (univariate) ----------------------------------------------------------------
add_se <- fread("output/sensitivity/burden_addclinics_se.gz")
add_base <- fread("output/preds/natl_preds.gz")

add_se %>%
  pivot_longer(bites_mean:averted_lower) %>%
  pivot_wider(id_cols = c(scale, vary, scenario, data_source, name), names_from = direction) -> add_summ

add_base %>%
  pivot_longer(bites_mean:averted_lower) %>%
  left_join(add_summ) -> add_se

add_se %>%
  group_by(scale, vary, name) %>%
  arrange(scenario) %>%
  mutate(value = value/value[1], max = max/max[1], min = min/min[1]) -> add_props

# Max reduction in deaths --------------------------------------------------------------
add_props %>%
  filter(scenario == max(scenario)) -> max_props
max_props$vary <- factor(max_props$vary, 
                         levels = rev(c("beta_ttimes", "beta_0", "sigma_e", "rho_max", 
                                        "p_rabid", "p_death", "human_exp")))
addARMC_A <- ggplot(data = filter(max_props, name == "deaths_mean"), 
                    aes(x = vary, y = value, color = scale, fill = scale)) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + 
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), 
                position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
                     name = "Scale") +
  coord_flip() +
  labs(y = "Propotion of deaths \n compared to baseline" , x = "Parameter", tag = "A") +
  theme_minimal_grid()

# As each clinic is added --------------------------------------------------------------

# need to filter to below max!
add_props$vary <- factor(add_props$vary, 
                         levels = c("beta_ttimes", "beta_0", "sigma_e", "rho_max", 
                                        "p_rabid", "p_death", "human_exp"))
# order them correctly & name them correctly
add_props$vary <- fct_recode(add_props$vary, 
                                   `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
                                   `sigma[e]` = "sigma_e", `rho[max]` = "rho_max", 
                                   `p[rabid]` = "p_rabid", `p[death]` = "p_death" ,
                                   `E[i]` = "human_exp")

addARMC_B <- ggplot(data = filter(add_props, name == "deaths_mean", scenario != max(scenario)),
                    aes(x = scenario, y = value, color = scale)) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  geom_pointrange(data = filter(add_props, scenario == max(scenario), name == "deaths_mean"),
                  aes(x = scenario, y = value, 
                      ymin = min, ymax = max, color = scale)) +
  scale_color_manual(values = model_cols, aesthetics = c("color", "fill"),
                     labels = scale_labs, guide = "none") +
  facet_grid(vary ~ scale, scales = "free_x",
             labeller = labeller(scale = c("Commune" = "", "District" = ""), 
                                 vary = label_parsed)) +
  labs(x = "", y = "Propotion of deaths \n compared to baseline",
       tag = "B") +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
        panel.grid = element_line(color = "white", size = 0.5))

S6.2_addARMC_se <- addARMC_A + addARMC_B + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.2_addARMC_se.jpeg", S6.2_addARMC_se, height = 8, width = 10)

# Sensitivity of vials ---------------------------------------------------------------------
vials_se <- fread("output/sensitivity/vials_se.gz")
vials_base <- fread("output/preds/natl_preds.gz")

vials_se %>%
  pivot_longer(vials_mean:vials_lower) %>%
  pivot_wider(id_cols = c(scale, vary, name, scenario), 
              names_from = direction) -> vials_summ
vials_base %>%
  pivot_longer(vials_mean:vials_lower) %>%
  left_join(vials_summ) -> vials_se


# Vials baseline -----------------------------------------------------------------
vials_base <- filter(vials_se, scenario == 0)
vials_base$vary <- factor(vials_base$vary, 
                        levels = rev(c("beta_ttimes", "beta_0", "sigma_e")))

vial_se_A <- ggplot(data = filter(vials_base, name == "vials_mean"), 
                    aes(x = vary, y = value, color = scale, fill = scale)) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + 
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), 
                position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
                     name = "Scale") +
  coord_flip() +
  labs(y = "Baseline vial demand (national)" , x = "Parameter", tag = "A") +
  theme_minimal_grid()

# Increase in vials --------------------------------------------------------------
# order them correctly & name them correctly
vials_se$vary <- factor(vials_se$vary, 
                          levels = c("beta_ttimes", "beta_0", "sigma_e"))
vials_se$vary <- fct_recode(vials_se$vary, `sigma[e]` = "sigma_e", `beta[0]` = "beta_0", 
                            `beta[t]` = "beta_ttimes")

vial_se_B <- ggplot(data = filter(vials_se, name == "vials_mean", 
                                  scenario != max(scenario)), 
                    aes(x = scenario, y = value, color = scale)) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  geom_pointrange(data = filter(vials_se, name == "vials_mean", 
                                scenario == max(scenario)),
                  aes(x = scenario, y = value, 
                      ymin = min, ymax = max, color = scale)) +
  scale_color_manual(values = model_cols, aesthetics = c("color", "fill"),
                     labels = scale_labs, guide = "none") +
  facet_grid(vary ~ scale, scales = "free_x", 
             labeller = labeller(scale = c("Commune" = "", "District" = ""), vary = label_parsed)) +
  labs(x = "# Additional ARMC", y = "Increase in vial demand (national)",
       tag = "B") +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
        panel.grid = element_line(color = "white", size = 0.5))

S6.3_vials_se <-vial_se_A + vial_se_B + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.3_vial_se.jpeg", S6.3_vials_se, height = 8, width = 10)

# Saving session info
out.session(path = "R/figures/SM6_burden_se.R", filename = "output/log_local.csv")


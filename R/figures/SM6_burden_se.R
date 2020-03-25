# ------------------------------------------------------------------------------------------------ #
#' Univariate sensitivity analyses     
# ------------------------------------------------------------------------------------------------ #

# Libraries and scripts
library(data.table)
library(tidyverse)
library(cowplot)
library(patchwork)
source("R/functions/out.session.R")

# Baseline burden (univariate) ----------------------------------------------------------------
base_se_admin <- fread("output/sensitivity/burden_baseline_se.gz")
baseline <- fread("output/preds/burden_base.gz")

# At national level ----------------------------------------------------------------
# Labels and colors
par_labs <- c("beta_ttimes" = bquote(beta[t]), "beta_0" = bquote(beta[0]),
              "sigma_0" = bquote(sigma[0]),
              "rho_max" = bquote(rho[max]), "p_rabid" = bquote(p[rabid]),
              "p_death" = bquote(p[death]), "human_exp" = bquote(E[i]))
par_cols <- c("beta_ttimes" = "grey50",  "beta_0" = "grey50",
              "sigma_0" = "grey50",
              "rho_max" = "black", "p_rabid" = "black",
              "p_death" = "black", "human_exp" = "black")

# Scale
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune (Moramanga)", "District (National)")
model_cols <- c("#F2300F", "#35274A")
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
base_se <- filter(base_se, interaction(vary, scale) != "sigma_0.Commune")
base_se$vary <- factor(base_se$vary, levels = rev(c("beta_ttimes", "beta_0", "sigma_0", "rho_max", 
                                                    "p_rabid", "p_death", "human_exp")))

base_natl_A <- ggplot(data = filter(base_se, name == "deaths_mean"), 
                      aes(x = vary, y = value, color = scale, fill = scale)) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + # dummy to get legend right
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
                     name = "Model scale (& data)") +
  
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
base_admin_summ <- filter(base_admin_summ, interaction(vary, scale) != "sigma_0.Commune")
base_admin_summ$vary <- factor(base_admin_summ$vary, 
                               levels = c("beta_ttimes", "beta_0", "sigma_0", "rho_max", 
                                          "p_rabid", "p_death", "human_exp"))
base_admin_summ$vary <- fct_recode(base_admin_summ$vary, 
                                   `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
                                   `sigma[0]` = "sigma_0", `rho[max]` = "rho_max", 
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
  theme_minimal_hgrid()

S6.1_base_se <- base_natl_A + base_admin_B + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.1_base_se.jpeg", S6.1_base_se, height = 8, width = 10)

# Baseline burden (univariate) ----------------------------------------------------------------
add_se_admin <- fread("output/sensitivity/burden_addclinics_se.gz")
add_base <- fread("output/preds/burden_all.gz")

add_se_admin %>%
  group_by(scale, vary, direction, scenario) %>%
  summarize_at(vars(bites_mean:averted_lower), sum) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  pivot_wider(id_cols = c(scale, vary, name, scenario), names_from = direction)-> add_summ

add_base %>%
  filter(scale != "") %>%
  group_by(scale, scenario) %>%
  summarize_at(vars(bites_mean:averted_lower), sum) %>%
  pivot_longer(bites_mean:averted_lower) %>%
  left_join(add_summ) -> add_se

add_se %>%
  group_by(scale, vary, name) %>%
  arrange(scenario) %>%
  mutate(value = value/value[1], max = max/max[1], min = min/min[1]) -> add_props

# Max reduction in deaths --------------------------------------------------------------
add_props %>%
  filter(scenario == max(scenario)) -> max_props
max_props <- filter(max_props, interaction(vary, scale) != "sigma_0.Commune")
max_props$vary <- factor(max_props$vary, 
                         levels = rev(c("beta_ttimes", "beta_0", "sigma_0", "rho_max", 
                                        "p_rabid", "p_death", "human_exp")))
addARMC_A <- ggplot(data = filter(max_props, name == "deaths_mean"), 
                    aes(x = vary, y = (1 - value)*100, color = scale, fill = scale)) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + 
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = (1 - max)*100, ymin = (1 - min)*100), 
                position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
                     name = "Model scale (& data)") +
  coord_flip() +
  labs(y = "% Reduction in deaths \n (National)" , x = "Parameter", tag = "A") +
  theme_minimal_grid()

# As each clinic is added --------------------------------------------------------------
# need to filter to below max!
max_added <- sort(unique(add_props$scenario), decreasing = TRUE)[2]
max_total <- max_added + 200
add_props$scenario[add_props$scenario == max(add_props$scenario)] <- max_total
add_props <- filter(add_props, interaction(vary, scale) != "sigma_0.Commune")
add_props$vary <- factor(add_props$vary, 
                         levels = c("beta_ttimes", "beta_0", "sigma_0", "rho_max", 
                                        "p_rabid", "p_death", "human_exp"))

# order them correctly & name them correctly
add_props$vary <- fct_recode(add_props$vary, 
                                   `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
                                   `sigma[0]` = "sigma_0", `rho[max]` = "rho_max", 
                                   `p[rabid]` = "p_rabid", `p[death]` = "p_death" ,
                                   `E[i]` = "human_exp")

addARMC_B <- ggplot(data = filter(add_props, name == "deaths_mean", scenario != max_total), 
       aes(x = scenario, y = value, color = scale)) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  geom_pointrange(data = filter(add_props, name == "deaths_mean", scenario == max_total),
                  aes(x = scenario, y = value, 
                      ymin = min, ymax = max, color = scale),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, aesthetics = c("color", "fill"),
                     labels = scale_labs, guide = "none") +
  facet_grid(vary ~ scale, scales = "free_x",
             labeller = labeller(scale = c("Commune" = "", "District" = ""), 
                                 vary = label_parsed)) +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  annotate(geom = "text", x = max_added + 100, y = -30, label = "......") +
  labs(x = "", y = "Proportion reduction in deaths \n nationally",
       tag = "B") +
  coord_cartesian(clip = "off", ylim = c(0, 1)) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S6.2_addARMC_se <- addARMC_A + addARMC_B + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.2_addARMC_se.jpeg", S6.2_addARMC_se, height = 8, width = 10)

# Sensitivity of vials ---------------------------------------------------------------------
vial_se <- fread("output/sensitivity/catch_preds_se.gz")
vials_base <- fread("output/preds/catch_preds.gz")

vial_se %>%
  group_by(scale, vary, direction, scenario) %>%
  summarize_at(vars(vials_mean_total:vials_lower_total), sum) %>%
  pivot_longer(vials_mean_total:vials_lower_total) %>%
  pivot_wider(id_cols = c(scale, vary, name, scenario), 
              names_from = direction)-> vials_summ

vials_base %>%
  filter(scale != "") %>%
  group_by(scale, scenario) %>%
  summarize_at(vars(vials_mean:vials_lower), sum) %>%
  select(scale, scenario, 
         vials_mean_total = vials_mean, 
         vials_sd_total = vials_sd, vials_upper_total = vials_upper,
         vials_lower_total = vials_lower) %>%
  pivot_longer(vials_mean_total:vials_lower_total) %>%
  left_join(vials_summ) -> vials_se

# need to filter to below max!
vials_se$scenario[vials_se$scenario == max(vials_se$scenario)] <- max_total
# also filter sigma_0
vials_se <- filter(vials_se, interaction(vary, scale) != "sigma_0.Commune")

# Vials baseline -----------------------------------------------------------------
vials_base <- filter(vials_se, scenario == 0)
vials_base$vary <- factor(vials_base$vary, 
                        levels = rev(c("beta_ttimes", "beta_0", "sigma_0")))

vial_se_A <- ggplot(data = filter(vials_base, name == "vials_mean_total"), 
                    aes(x = vary, y = value, color = scale, fill = scale)) +
  geom_ribbon(aes(ymax = max, ymin = min), color = NA, alpha = 0.25) + 
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), 
                position = position_dodge(width = 0.25)) +
  scale_x_discrete(labels = par_labs) +
  scale_color_manual(values = model_cols, labels = scale_labs, aesthetics = c("color", "fill"),
                     name = "Model scale (& data)") +
  coord_flip() +
  labs(y = "Baseline vial demand (national)" , x = "Parameter", tag = "A") +
  theme_minimal_grid()

# Increase in vials --------------------------------------------------------------
# order them correctly & name them correctly
vials_se$vary <- factor(vials_se$vary, 
                          levels = c("beta_ttimes", "beta_0", "sigma_0"))
vials_se$vary <- fct_recode(vials_se$vary, `sigma[0]` = "sigma_0", `beta[0]` = "beta_0", 
                            `beta[t]` = "beta_ttimes")

vial_se_B <- ggplot(data = filter(vials_se, name == "vials_mean_total", 
                                  scenario != max_total), 
                    aes(x = scenario, y = value, color = scale)) +
  geom_line(alpha = 0.75) +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), color = NA, alpha = 0.25) +
  geom_pointrange(data = filter(vials_se, name == "vials_mean_total", scenario == max_total),
                  aes(x = scenario, y = value, 
                      ymin = min, ymax = max, color = scale),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, aesthetics = c("color", "fill"),
                     labels = scale_labs, guide = "none") +
  facet_grid(vary ~ scale, scales = "free_x",
             labeller = labeller(scale = c("Commune" = "", "District" = ""), 
                                 vary = label_parsed)) +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  annotate(geom = "text", x = max_added + 100, y = -30, label = "......") +
  labs(x = "# Additional ARMC", y = "Increase in vial demand (national)",
       tag = "B") +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

S6.3_vials_se <-vial_se_A + vial_se_B + plot_layout(guides = "collect")
ggsave("figs/supplementary/S6.3_vial_se.jpeg", S6.3_vials_se, height = 8, width = 10)

# Saving session info
out.session(path = "R/figures/SM6_burden_se.R", filename = "output/log_local.csv")


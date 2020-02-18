# ------------------------------------------------------------------------------------------------ #
#' Univariate sensitivity analyses     
# ------------------------------------------------------------------------------------------------ #

# Libraries and scripts
library(data.table)
library(tidyverse)
library(cowplot)

# Baseline burden univariate
base_se_admin <- fread("output/sensitivity/burden_baseline_se.gz")
baseline <- fread("output/preds/burden.gz")[scenario == 0]
  
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

ggplot(data = filter(base_se, grepl("mean", name)), aes(x = vary, y = value, color = scale)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.25)) +
  facet_wrap(~name, scales = 'free') +
  coord_flip() +
  cowplot::theme_minimal_grid()

base_se_admin %>%
  mutate(deaths_mean = deaths_mean/pop*1e5) %>%
  select(scale, ttimes, deaths_mean, vary, direction) %>%
  pivot_wider(id_cols = c(scale, vary, ttimes), names_from = direction, 
              values_from = deaths_mean) -> base_admin_summ

ggplot(data = base_admin_summ, aes(x = ttimes, y = deaths_mean, color = scale)) +
  geom_pointrange(aes(ymax = min, ymin = deaths_lower/pop*1e5)) +
  facet_wrap(vary ~ direction, scales = "free") +
  cowplot::theme_minimal_grid()

# Reduction in burden as clinics are added
add_se_admin <- fread("output/sensitivity/burden_addclinics_se.gz")
add_base <- fread("output/preds/burden.gz")

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

add_props %>%
  filter(scenario == max(scenario)) -> max_props

ggplot(data = filter(max_props, grepl("mean", name)), aes(x = vary, y = value, color = scale)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.25)) +
  facet_wrap(~name, scales = 'free') +
  coord_flip() +
  cowplot::theme_minimal_grid()

ggplot(data = add_se_admin, aes(x = scenario, y = deaths_mean/pop*1e5, color = scale)) +
  geom_pointrange(aes(ymax = deaths_upper/pop*1e5, ymin = deaths_lower/pop*1e5)) +
  facet_wrap(vary ~ direction, scales = "free") +
  cowplot::theme_minimal_grid()

ggplot(data = filter(add_props, grepl("deaths", name)), 
       aes(x = scenario, y = value, color = scale, alpha = 0.5)) +
  geom_line() +
  geom_ribbon(aes(ymax = min, ymin = max, fill = scale), alpha = 1) +
  facet_wrap(~ vary, scales = "free", ncol = 1) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5))


# Figures 
# colors 
scale_levs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 
incidence_max <- 0.01*0.39/7
incidence_min <- 0.01*0.39/35
pop_plot <- seq(0, 1e6, by = 1000)
scaling_labs <- c("neg" = "Incidence decreases \n with pop (in range 11.1 - 55.7)", 
                  "pos" = "Incidence increases \n with pop (in range 11.1 - 55.7)")
figS6.1 <- ggplot(data = predicted_inc, aes(x = log(pop_plot), y = preds, 
                                            color = scale, 
                                            group = interaction(scale, scaling))) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = c(incidence_max*1e5, incidence_min*1e5), linetype = 2, color = "black") +
  scale_color_manual(values = model_cols, name = "Model scale") +
  ylab("Rabies exposures per \n 100k persons") +
  xlab("Human population size (log)") +
  facet_wrap(~scaling, labeller = labeller(scaling = scaling_labs)) +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5))
  

ggsave("figs/supplementary/S6.1.jpeg", figS6.1, device = "jpeg", height = 6, width = 8)

figS6.2 <- ggplot(data = base_se, aes(x = ttimes/60, y = deaths_mean/pop*1e5, 
                           color = scale, shape = as.factor(rho_max))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = model_cols, name = "Model scale") +
  scale_shape_manual(values = c(16, 17), name = expression(paste("ρ"[max]))) +
  facet_grid(p_rabid ~ type, 
             labeller = label_bquote(rows = paste(p[rabid], " = ", .(p_rabid)))) +  
  labs(x = "Travel times (hrs)", y = "Deaths per 100k") +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5),
        axis.text.x = element_text(hjust = 1, angle = 45))

ggsave("figs/supplementary/S6.2.jpeg", figS6.2, device = "jpeg", height = 8, width = 12)


figS6.3 <- ggplot(data = filter(incremental_se, scenario != max(scenario)),
       aes(x = scenario, y = deaths_mean/deaths_base, 
           color = scale, 
           shape = as.factor(rho_max))) +
  geom_line() +
  geom_ribbon(aes(ymin = deaths_lower/deaths_lower_base, ymax = deaths_upper/deaths_upper_base, 
                  fill = scale, group = interaction(scale, rho_max)), alpha = 0.5) +
  geom_point(data = filter(incremental_se, scenario == max(scenario)),
             aes(x = scenario, y = deaths_mean/deaths_base, 
                 color = scale, 
                 shape = as.factor(rho_max))) +
  scale_color_manual(values = model_cols, name = "Model scale") +
  scale_fill_manual(values = model_cols, name = "Model scale") +
  scale_shape_manual(values = c(16, 17), name = expression(paste("ρ"[max]))) +
  facet_grid(p_rabid ~ type, 
             labeller = label_bquote(rows = paste(p[rabid], " = ", .(p_rabid)))) +
  scale_x_continuous(breaks = c(0, 200, 400, 600), 
                     labels = c(0, 200, 400, "max (1648)")) +
  annotate(geom = "text", x = 520, y = -0.1, label = "...") +
  labs(x = "# Additional clinics", y = "Proportion reduction in burden") +
  theme_minimal_grid() +
  theme(panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
        panel.grid = element_line(color = "white", size = 0.5), 
        axis.text.x = element_text(hjust = 1, angle = 45))
ggsave("figs/supplementary/S6.3.jpeg", figS6.3, device = "jpeg", height = 7, width = 10)


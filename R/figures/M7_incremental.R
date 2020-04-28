# ------------------------------------------------------------------------------------------------ #
#' Figure M5: Scenario analysis        
#' Details: Plotting deaths and vials as clinics are added  
# ------------------------------------------------------------------------------------------------ #

# Set up 
library(tidyverse)
library(data.table)
library(patchwork)
library(cowplot) 

# read in natl preds
natl_preds <- fread("output/preds/burden_natl.gz")
natl_vials <- fread("output/preds/catch_preds_natl.csv")

# Joining and summarizing preds
natl_preds <- natl_preds[natl_vials, on = c("scenario", "scale", "data_source")]
natl_preds_sep <- natl_preds[scenario %in% c("max", "armc_per_dist", "armc_per_comm")]
natl_preds_seq <- natl_preds[!(scenario %in% c("max", "armc_per_dist", "armc_per_comm"))]
max_added <- max(as.numeric(natl_preds_seq$scenario))
max_total <- max_added + 200
natl_preds_sep %>%
  mutate(scenario_num = case_when(scenario == "max" ~ max_added + 200, 
                              scenario == "armc_per_dist" ~ 114,
                              scenario == "armc_per_comm" ~ max_added + 150),
         scenario = fct_relevel(scenario, "armc_per_dist", "armc_per_comm", "max")) -> natl_preds_sep

# Colors
# labs & cols
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

# Burden
burden <- ggplot(data = natl_preds_seq, 
                  aes(x = as.numeric(scenario), y = deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), color = NA, alpha = 0.35) +
  geom_pointrange(data = natl_preds_sep,
                  aes(x = scenario_num, y = deaths_mean,
                      ymin = deaths_lower, ymax = deaths_upper, color = scale,
                      shape = factor(scenario)),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_shape_discrete(labels = c("max" = "All CSB II", "armc_per_dist" = "ARMC per district",
                                  "armc_per_comm" = "ARMC per commune"), 
                       name = "Additional scenarios") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  labs(x = "", y = "Average annual\n deaths",
       tag = "A") +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Vials per death
vials_per_death <- ggplot(data = natl_preds_seq, 
                   aes(x = as.numeric(scenario), y = vials_mean/averted_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper/averted_upper, ymin = vials_lower/averted_lower), color = NA, alpha = 0.35) +
  geom_pointrange(data = natl_preds_sep,
                  aes(x = scenario_num, y = vials_mean/averted_mean,
                      ymax = vials_upper/averted_upper, ymin = vials_lower/averted_lower, 
                      color = scale, shape = scenario),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_shape_discrete(labels = c("max" = "All CSB II", "armc_per_dist" = "ARMC per district",
                                  "armc_per_comm" = "ARMC per commune"), 
                       name = "Additional scenarios") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  labs(x = "# Additional ARMC", y = "Average vials per \n death averted",
       tag = "C") +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Vials
vials <- ggplot(data = natl_preds_seq, 
                  aes(x = as.numeric(scenario), y = vials_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper, ymin = vials_lower), color = NA, alpha = 0.35) +
  geom_pointrange(data = natl_preds_sep,
                  aes(x = scenario_num, y = vials_mean,
                      ymax = vials_upper, ymin = vials_lower, color = scale,
                      shape = scenario),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_shape_discrete(labels = c("max" = "All CSB II", "armc_per_dist" = "ARMC per district",
                                  "armc_per_comm" = "ARMC per commune"), 
                       name = "Additional scenarios") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  labs(x = "", y = "Average annual\n vial demand",
       tag = "B") +
  coord_cartesian(clip = "off") + 
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

figM7 <- burden / vials / vials_per_death + plot_layout(guides = "collect")
ggsave("figs/main/M7_addARMC.jpeg", figM7, device = "jpeg", height = 8.75, width = 8)
ggsave("figs/main/M7_addARMC.tiff", figM7, device = "tiff", dpi = 300, height = 8.75, width = 8.75, 
       compression = "lzw", type = "cairo")


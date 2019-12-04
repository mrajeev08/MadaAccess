####################################################################################################
##' Supplementary section 5 
##' Details: sensitivity 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up
##' ------------------------------------------------------------------------------------------------
rm(list = ls())

##' Libraries and scripts
library(data.table)
library(tidyverse)

base_se <- fread("output/sensitivity/baseline_se.csv")
base_se$type <- factor(base_se$type, levels = c("min", "+++", "---", "max"))
incremental_se <- fread("output/sensitivity/incremental_se.csv")
incremental_se$type <- factor(incremental_se$type, 
                              levels = c("min", "+++", "---", "max"))
## Figures 
scale_levs <- c("Commune", "District")
model_cols <- wesanderson::wes_palettes$Rushmore1[c(3, 4)]
names(model_cols) <- scale_levs 

figS5.2 <- ggplot(data = base_se, aes(x = ttimes/60, y = deaths_mean/pop*1e5, 
                           color = as.factor(p_rabid), shape = as.factor(rho_max))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("red", "darkred"), name = expression(p["rabid"])) +
  scale_shape_manual(values = c(16, 17), name = "\u03C1_max") +
  facet_grid(scale ~ type) +
  labs(x = "Travel times (hrs)", y = "Deaths per 100k")
ggsave("figs/S5.2.jpeg", figS5.2, device = "jpeg", height = 7, width = 10)

figS5.3 <- ggplot(data = filter(incremental_se, scenario != max(scenario)),
       aes(x = scenario, y = deaths_mean/deaths_base, 
           color = as.factor(p_rabid), 
           shape = as.factor(rho_max))) +
  geom_line() +
  geom_ribbon(aes(ymin = deaths_lower/deaths_lower_base, ymax = deaths_upper/deaths_upper_base, 
                  fill = as.factor(p_rabid), group = interaction(p_rabid, rho_max)), alpha = 0.5) +
  geom_point(data = filter(incremental_se, scenario == max(scenario)),
             aes(x = scenario, y = deaths_mean/deaths_base, 
                 color = as.factor(p_rabid), 
                 shape = as.factor(rho_max))) +
  scale_color_manual(values = c("red", "darkred"), name = expression(p["rabid"])) +
  scale_fill_manual(values = c("red", "darkred"), name = expression(p["rabid"])) +
  scale_shape_manual(values = c(16, 17), name = "\u03C1_max") +
  facet_grid(scale ~ type) +
  scale_x_continuous(breaks = c(0, 200, 400, 600), 
                     labels = c(0, 200, 400, "max (1648)")) +
  annotate(geom = "text", x = 520, y = -0.1, label = "...") +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  labs(x = "# Additional clinics", y = "Proportion reduction in burden")
ggsave("figs/S5.3.jpeg", figS5.3, device = "jpeg", height = 7, width = 10)


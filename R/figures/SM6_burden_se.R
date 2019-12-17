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

predicted_inc <- fread("output/sensitivity/scaling.csv")
base_se <- fread("output/sensitivity/baseline_se.csv")
base_se$type <- factor(base_se$type)
incremental_se <- fread("output/sensitivity/incremental_se.csv")
incremental_se$type <- factor(incremental_se$type)
levels(incremental_se$type) <- 
  levels(base_se$type) <- list("Minimum incidence \n (11.1 per 100k)" = "min",
                                     "Incidence increases \n with pop" = "+++" ,
                                     "Incidence decreases \n with pop" = "---", 
                                     "Maximum incidence \n (55.7 per 100k)" = "max")
## Figures 
scale_levs <- c("Commune", "District")
model_cols <- wesanderson::wes_palettes$Rushmore1[c(3, 4)]
names(model_cols) <- scale_levs 
incidence_max <- 0.01*0.39/7
incidence_min <- 0.01*0.39/35
pop_plot <- seq(0, 1e6, by = 1000)
scaling_labs <- c("neg" = "Incidence decreases \n with pop", 
                  "pos" = "Incidence increases \n with pop")
figS6.1 <- ggplot(data = predicted_inc, aes(x = log(pop_plot), y = preds, 
                                            color = scale, 
                                            group = interaction(scale, scaling))) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = c(incidence_max*1e5, incidence_min*1e5), linetype = 2, color = "grey") +
  scale_color_manual(values = model_cols, name = "Model scale") +
  ylab("Rabies exposures per \n 100k persons") +
  xlab("Human population size (log)") +
  facet_wrap(~scaling, labeller = labeller(scaling = scaling_labs))
ggsave("figs/S6.1.jpeg", figS6.1, device = "jpeg", height = 5, width = 7)

figS6.2 <- ggplot(data = base_se, aes(x = ttimes/60, y = deaths_mean/pop*1e5, 
                           color = scale, shape = as.factor(rho_max))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = model_cols, name = "Model scale") +
  scale_shape_manual(values = c(16, 17), name = expression(paste("ρ"[max]))) +
  facet_grid(p_rabid ~ type, 
             labeller = label_bquote(rows = paste(p[rabid], " = ", .(p_rabid)))) +  
  labs(x = "Travel times (hrs)", y = "Deaths per 100k")
ggsave("figs/S6.2.jpeg", figS6.2, device = "jpeg", height = 7, width = 10)


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
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  labs(x = "# Additional clinics", y = "Proportion reduction in burden")
ggsave("figs/S6.3.jpeg", figS6.3, device = "jpeg", height = 7, width = 10)


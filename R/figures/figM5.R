####################################################################################################
##' Figure M5: Scenario analysis 
##' Details: Plotting deaths and vials as clinics are added 
##' Author: Malavika Rajeev 
####################################################################################################

##' Set up 
##' ------------------------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(patchwork)

admin_preds <- fread("output/preds/complete/burden_filled.csv")
vial_preds <- fread("output/preds/complete/vials_filled.csv")

##' Joining and summarizing preds
admin_preds %>%
  group_by(scale, scenario) %>%
  summarize_at(vars(bites_mean:averted_lower, pop), sum, na.rm = TRUE) -> natl_preds

vial_preds %>%
  group_by(scale, scenario) %>%
  summarize_at(vars(vials_mean:vials_lower), sum, na.rm = TRUE) -> natl_vials

natl_preds_all <- left_join(natl_preds, natl_vials)
natl_preds_all$scenario[natl_preds_all$scenario == max(natl_preds_all$scenario)] <- 550

##' Colors
scale_levs <- c("Commune", "District")
model_cols <- wesanderson::wes_palettes$Rushmore1[c(3, 4)]
names(model_cols) <- scale_levs 

##' Figure 5.A
figM5.A <- ggplot(data = natl_preds_all, aes(x = scenario, y = deaths_mean, color = scale, 
                                             fill = scale, 
                                  shape = scale)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = deaths_upper, ymax = deaths_lower), alpha = 0.35) +
  scale_color_manual(values = model_cols, name = "Model scale") +
  scale_fill_manual(values = model_cols, name = "Model scale") +
  scale_shape_manual(values = c(22, 23), name = "Model scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 550), 
                     labels = c(0, 100, 200, 300, 400, 472, "max (1648)")) +
  annotate("rect", xmin = 485, xmax = 540, ymin = 0, ymax = Inf, fill = "white") +
  labs(x = "# additional clinics \n providing PEP", y = "Mean predicted deaths \n nationally") +
  coord_cartesian(ylim = c(50, max(natl_preds_all$deaths_upper)))

##' Figure 5.B
figM5.B <- ggplot(data = natl_preds_all, aes(x = scenario, y = vials_mean/deaths_mean, 
                                  color = scale, fill = scale, 
                                  shape = scale)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = vials_upper/deaths_upper, ymax = vials_lower/deaths_lower), 
                 alpha = 0.35) +
  scale_color_manual(values = model_cols, name = "Model scale") +
  scale_fill_manual(values = model_cols, name = "Model scale") +
  scale_shape_manual(values = c(22, 23), name = "Model scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 550), 
                     labels = c(0, 100, 200, 300, 400, 472, "max (1648)")) +
  annotate("rect", xmin = 480, xmax = 540, ymin = 0, ymax = Inf, fill = "white") +
  labs(x = "# additional clinics \n providing PEP", y = "Mean predicted deaths \n nationally") +
  coord_cartesian(ylim = c(100, 800))

figM5 <- figM5.A /figM5.B
ggsave("figs/M5.jpeg", figM5, device = "jpeg", height = 11, width = 8)

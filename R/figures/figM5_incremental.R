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
library(cowplot) # for minimal themes

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
natl_preds_all$scenario[natl_preds_all$scenario == max(natl_preds_all$scenario)] <- 600

##' Colors
scale_levs <- c("Commune", "District")
model_cols <- wesanderson::wes_palettes$Rushmore1[c(3, 4)]
names(model_cols) <- scale_levs 

##' Figure 5.A
figM5.A <- ggplot(data = natl_preds_all, aes(x = scenario, y = deaths_mean, color = scale, fill = scale, 
                                  shape = scale)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = deaths_upper, ymax = deaths_lower), alpha = 0.35) +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_shape_manual(values = c(22, 23), guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 600), 
                     labels = c(0, 100, 200, 300, 400, 472, "max (1648)")) +
  annotate(geom = "text", x = 520, y = -30, label = "......") +
  labs(x = "# additional clinics \n providing PEP", y = "Mean deaths \n nationally",
       tag = "A") +
  coord_cartesian(clip = "off", ylim = c(0, max(natl_preds_all$deaths_upper))) +
  theme_minimal_hgrid()

##' Figure 5.B
figM5.B <- ggplot(data = natl_preds_all, aes(x = scenario, y = vials_mean/deaths_mean, 
                                  color = scale, fill = scale, 
                                  shape = scale)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymax = vials_upper/deaths_upper, ymin = vials_lower/deaths_lower), 
                 alpha = 0.35) +
  scale_color_manual(values = model_cols, name = "Model scale") +
  scale_fill_manual(values = model_cols, name = "Model scale") +
  scale_shape_manual(values = c(22, 23), name = "Model scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 600), 
                     labels = c(0, 100, 200, 300, 400, 472, "max (1648)")) +
  annotate(geom = "text", x = 520, y = 1, label = "......") +
  labs(x = "# additional clinics \n providing PEP", y = "Mean vials per \n death nationally",
       tag = "B") +
  ylim(c(0, max(natl_preds_all$vials_lower/natl_preds_all$deaths_lower))) + 
  coord_cartesian(clip = "off", ylim = c(50, 800)) +
  theme_minimal_hgrid()

##' Figure 5.C
figM5.C <- ggplot(data = natl_preds_all, aes(x = scenario, y = vials_mean, 
                                             color = scale, fill = scale, 
                                             shape = scale)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymax = vials_upper, ymin = vials_lower), 
                 alpha = 0.35) +  
  scale_color_manual(values = model_cols, guide = "none") +
  scale_fill_manual(values = model_cols, guide = "none") +
  scale_shape_manual(values = c(22, 23), guide = "none") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 600), 
                     labels = c(0, 100, 200, 300, 400, 472, "max (1648)")) +
  annotate(geom = "text", x = 520, y = 2.5e4, label = "......") +
  labs(x = "# additional clinics \n providing PEP", y = "Mean vials total \n nationally",
       tag = "B") +
  coord_cartesian(clip = "off", ylim = c(3e4, 1e5)) + 
  theme_minimal_hgrid()

figM5 <- figM5.A / figM5.B / figM5.C
ggsave("figs/main/M5.tiff", figM5, device = "jpeg", dpi = 300, height = 8.75, width = 8)

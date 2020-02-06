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
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_levs 

##' Figure 5.A
figM5.A <- ggplot(data = filter(natl_preds_all, scenario != 600), 
                  aes(x = scenario, y = deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), color = NA, alpha = 0.35) +
  geom_point(data = filter(natl_preds_all, scenario == 600), 
             aes(x = scenario, y = deaths_mean, color = scale)) +
  geom_pointrange(data = filter(natl_preds_all, scenario == 600),
                  aes(ymin = deaths_lower, ymax = deaths_upper, color = scale)) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 600), 
                     labels = c("baseline", 100, 200, 300, 400, 472, "max (1648)")) +
  annotate(geom = "text", x = 520, y = -30, label = "......") +
  labs(x = "", y = "Mean deaths \n nationally",
       tag = "A") +
  coord_cartesian(clip = "off", ylim = c(0, max(natl_preds_all$deaths_upper))) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##' Figure 5.B
figM5.B <-  ggplot(data = filter(natl_preds_all, scenario != 600), 
                   aes(x = scenario, y = vials_mean/deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper/deaths_upper, ymin = vials_lower/deaths_lower), color = NA, alpha = 0.35) +
  geom_point(data = filter(natl_preds_all, scenario == 600), 
             aes(x = scenario, y = vials_mean/deaths_mean, color = scale)) +
  geom_pointrange(data = filter(natl_preds_all, scenario == 600),
                  aes(ymax = vials_upper/deaths_upper, ymin = vials_lower/deaths_lower, color = scale)) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 600), 
                     labels = c("baseline", 100, 200, 300, 400, 472, "max (1648)")) +
  annotate(geom = "text", x = 520, y = 1, label = "......") +
  labs(x = "", y = "Mean vials per \n death nationally",
       tag = "B") +
  ylim(c(0, max(natl_preds_all$vials_lower/natl_preds_all$deaths_lower))) + 
  coord_cartesian(clip = "off", ylim = c(50, 800)) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
##' Figure 5.C
figM5.C <- ggplot(data = filter(natl_preds_all, scenario != 600), 
                  aes(x = scenario, y = vials_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper, ymin = vials_lower), color = NA, alpha = 0.35) +
  geom_point(data = filter(natl_preds_all, scenario == 600), 
             aes(x = scenario, y = vials_mean, color = scale)) +
  geom_pointrange(data = filter(natl_preds_all, scenario == 600),
                  aes(ymax = vials_upper, ymin = vials_lower, color = scale)) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 472, 600), 
                     labels = c("baseline", 100, 200, 300, 400, 472, "max (1648)")) +
  annotate(geom = "text", x = 520, y = 2.5e4, label = "......") +
  labs(x = "# additional clinics \n providing PEP", y = "Mean vials total \n nationally",
       tag = "C") +
  coord_cartesian(clip = "off", ylim = c(3e4, 1e5)) + 
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

figM5 <- figM5.A / figM5.B / figM5.C + plot_layout(guides = "collect")
ggsave("figs/main/M5.jpeg", figM5, device = "jpeg", height = 8.75, width = 8)
ggsave("figs/main/M5.tiff", figM5, device = "tiff", dpi = 300, height = 8.75, width = 8, 
       compression = "lzw", type = "cairo")

##' Output stats
##' ------------------------------------------------------------------------------------------------
##' Mean total bites/deaths/vials/averted @ National scale for two models
write.csv(natl_preds_all, "output/stats/natl_preds_all.csv", row.names = FALSE)

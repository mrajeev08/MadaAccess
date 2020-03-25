# ------------------------------------------------------------------------------------------------ #
#' Figure M5: Scenario analysis        
#' Details: Plotting deaths and vials as clinics are added  
# ------------------------------------------------------------------------------------------------ #

# Set up 
library(tidyverse)
library(data.table)
library(patchwork)
library(cowplot) 

admin_preds <- fread("output/preds/burden_all.gz")
vial_preds <- fread("output/preds/catch_preds.gz")

# Joining and summarizing preds
admin_preds %>%
  filter(scale != "") %>%
  group_by(scale, scenario) %>%
  summarize_at(vars(bites_mean:averted_lower, pop), sum, na.rm = TRUE) -> natl_preds
max_added <- sort(unique(natl_preds$scenario), decreasing = TRUE)[2]
max_total <- max_added + 200

vial_preds %>%
  group_by(scale, scenario) %>%
  summarize_at(vars(vials_mean:vials_lower), sum, na.rm = TRUE) -> natl_vials

natl_preds_all <- left_join(natl_preds, natl_vials)
natl_preds_all$scenario[natl_preds_all$scenario == max(natl_preds_all$scenario)] <- max_total
natl_preds$scenario[natl_preds$scenario == max(natl_preds$scenario)] <- max_total

# Colors
# labs & cols
scale_levs <- c("Commune", "District")
scale_labs <- c("Moramanga", "District")
model_cols <- c("#F2300F", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

# Burden
burden <- ggplot(data = filter(natl_preds, scenario != max_total), 
                  aes(x = scenario, y = deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(natl_preds, scenario == max_total),
                  aes(x = scenario, y = deaths_mean, 
                      ymin = deaths_lower, ymax = deaths_upper, color = scale),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  annotate(geom = "text", x = max_added + 100, y = -30, label = "......") +
  labs(x = "", y = "Mean deaths \n nationally",
       tag = "A") +
  coord_cartesian(clip = "off", ylim = c(0, max(natl_preds$deaths_upper))) +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Vials per death
vials_per_death <- ggplot(data = filter(natl_preds_all, scenario != max_total), 
                   aes(x = scenario, y = vials_mean/deaths_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper/deaths_upper, ymin = vials_lower/deaths_lower), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(natl_preds_all, scenario == max_total),
                  aes(x = scenario, y = vials_mean/deaths_mean,
                      ymax = vials_upper/deaths_upper, ymin = vials_lower/deaths_lower, 
                      color = scale),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  annotate(geom = "text", x = max_added + 100, y = 1, label = "......") +
  labs(x = "", y = "Mean vials per \n death nationally",
       tag = "C") +
  ylim(c(0, max(natl_preds_all$vials_lower/natl_preds_all$deaths_lower))) + 
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Vials
vials <- ggplot(data = filter(natl_preds_all, scenario != max_total), 
                  aes(x = scenario, y = vials_mean, color = scale, fill = scale)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper, ymin = vials_lower), color = NA, alpha = 0.35) +
  geom_pointrange(data = filter(natl_preds_all, scenario == max_total),
                  aes(x = scenario, y = vials_mean,
                      ymax = vials_upper, ymin = vials_lower, color = scale),
                  position = position_dodge(width = 50)) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_x_continuous(breaks = c(0, 100, 300, 500, max_added, max_total), 
                     labels = c("baseline", 100, 300, 500, max_added, "max (1648)")) +
  annotate(geom = "text", x = max_added + 100, y = 2.5e4, label = "......") +
  labs(x = "# Additional ARMC", y = "Mean vials total \n nationally",
       tag = "B") +
  coord_cartesian(clip = "off") + 
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

figM7 <- burden / vials / vials_per_death + plot_layout(guides = "collect")
ggsave("figs/main/M7_addARMC.jpeg", figM7, device = "jpeg", height = 8.75, width = 8)
ggsave("figs/main/M7_addARMC.tiff", figM7, device = "tiff", dpi = 300, height = 8.75, width = 8, 
       compression = "lzw", type = "cairo")

# Output stats
# Mean total bites/deaths/vials/averted @ National scale for two models
write.csv(natl_preds_all, "output/stats/natl_preds_all.csv", row.names = FALSE)

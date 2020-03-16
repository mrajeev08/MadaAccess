# ------------------------------------------------------------------------------------------------ #
#' Run models of bite incidence using spatial covariates 
#' Models include travel times and distance as access metrics, in addition to population 
# ------------------------------------------------------------------------------------------------ #

# source bite ests
source("R/functions/out.session.R")

# libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)
library(data.table)

# source bite ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")
preds <- read.csv("output/preds/bites/expectations.csv") # Expectations
preds <- filter(preds, interaction(data_source, OD) != "Moramanga.FALSE")

# data
district_bites$data_source <- "National"
mora_bites$data_source <- "Moramanga"
observed <- bind_rows(district_bites, mora_bites)

# labs & cols
scale_levs <- c("Moramanga.Commune", "National.Commune", "National.District")
scale_labs <- c("Moramanga", "Commune", "District")
model_cols <- c("#F2300F", "#0B775E", "#35274A")
names(scale_labs) <- scale_levs 
names(model_cols) <- scale_levs

modpreds <- ggplot(data = filter(preds, intercept == "random" | data_source == "Moramanga"), 
       aes(x = ttimes_wtd, y = exp_bites, color = interaction(data_source, scale))) +
  geom_point(data = observed, aes(x = ttimes_wtd/60, y = avg_bites/pop*1e5, 
                                  shape = data_source), color = "black", alpha = 0.5, size = 1.5,
             stroke = 1.2, inherit.aes = FALSE) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = exp_bites_lower, ymax = exp_bites_upper, 
                  fill = interaction(data_source, scale)),
              color = NA, alpha = 0.25) +
  scale_color_manual(values = model_cols, name = "Scale", 
                     labels = scale_labs) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k") +
  cowplot::theme_minimal_grid()

ggsave("figs/main/M5_modpreds.tiff", modpreds, dpi = 300, height = 4, width = 6)
ggsave("figs/main/M5_modpreds.jpeg", modpreds, height = 4, width = 6)

# Session Info
out.session(path = "R/M5_modpreds.R", filename = "output/log_local.csv")

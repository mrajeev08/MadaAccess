# ------------------------------------------------------------------------------------------------ #
#' Run models of bite incidence using spatial covariates 
#' Models include travel times and distance as access metrics, in addition to population 
# ------------------------------------------------------------------------------------------------ #

# source bite ests
source("R/functions/out.session.R")
source("R/functions/summarize_samps.R")

# libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)
library(cowplot)
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
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k", tag = "A") +
  cowplot::theme_minimal_grid()

# Plot posterior ests ------------------------------------------------------------------
all_samps_natl <- get.samps(parent_dir = "output/mods/samps/National/")
all_samps_natl <- filter(all_samps_natl, pop_predict == "flatPop", 
                         intercept == "random")

# Do the adjusted ones for Moramanga
model_ests <- read.csv("output/mods/estimates_adj_OD.csv")
model_ests <- bind_rows(filter(model_ests, pop_predict == "flatPop", data_source == "Moramanga"),
               filter(model_ests, pop_predict == "flatPop", intercept == "random"))

foreach(x = iter(model_ests, by = "row"), .combine = rbind, .options.RNG = 2577) %dorng% {
  data.table(Parameter = x$params, data_source = x$data_source, scale = x$scale, 
             value = rnorm(72000, mean = x$Mean, sd = x$sd_adj))
} -> all_samps

# all_samps <- bind_rows(all_samps_natl, mora_samps)

# so that same # of hgrid lines across these
set_breaks = function(limits) {
  round(seq(limits[1] + 0.5, limits[2] - 0.5, length.out = 3), 1)
}

beta_ttimes <- ggplot(data = filter(all_samps, Parameter == "beta_ttimes"), 
                aes(x = Parameter, y = value, 
                                      fill = interaction(data_source, scale))) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs, guide = "none") +
  ylim(c(-1.5, 0)) +
  scale_x_discrete(labels = "") +
  labs(x = bquote(beta[t] ~ "(Travel time effect)"), y = "Estimate", 
       tag = "C") +
  theme_minimal_hgrid() +
  theme(axis.ticks.x = element_blank())


beta_0 <- ggplot(data = filter(all_samps, Parameter == "beta_0"), 
       aes(x = Parameter, y = value, 
           fill = interaction(data_source, scale))) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs, guide = "none") +
  scale_x_discrete(labels = "") +
  scale_y_continuous(breaks = set_breaks) +
  labs(x = bquote(beta[0] ~ "(Intercept)"), y = "Estimate", tag = "B") +
  theme_minimal_hgrid() +
  theme(axis.ticks.x = element_blank())

sigma_0 <- ggplot(data = filter(all_samps, Parameter == "sigma_0"), 
       aes(x = Parameter, y = value, 
           fill = interaction(data_source, scale))) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values = model_cols, name = "Scale",
                    labels = scale_labs, guide = "none") +
  scale_x_discrete(labels = "") +
  labs(x = bquote(sigma[0] ~ "(Catchment random effect)"), y = "Estimate", tag = "D") +
  theme_minimal_hgrid() +
  theme(axis.ticks.x = element_blank())

posts <- beta_0/beta_ttimes/sigma_0
mods <- (modpreds | posts) + plot_layout(guides = "collect")

ggsave("figs/main/M5_mods.tiff", mods, dpi = 300, height = 8, width = 8)
ggsave("figs/main/M5_mods.jpeg", mods, height = 8, width = 8)

# Session Info
out.session(path = "R/M5_modpreds.R", filename = "output/log_local.csv")

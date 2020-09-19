# ------------------------------------------------------------------------------------------------ #
#' Run models of bite incidence using spatial covariates
#' Models include travel times and distance as access metrics, in addition to population
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()

# source bite ests
source(here::here("R", "utils.R"))
source(here_safe("R/summarize_samps.R"))

# libraries
library(foreach)
library(iterators)
library(tidyverse)
library(glue)
library(patchwork)
library(cowplot)
library(data.table)

# source bite ests
district_bites <- fread(here_safe("analysis/out/bites/district_bites.csv"))
comm_covars <- fread(here_safe("analysis/out/bites/comm_covars.csv"))
mora_bites <- fread(here_safe("analysis/out/bites/mora_bites.csv"))
preds <- read.csv(here_safe("analysis/out/mods/preds/expectations.csv"))
preds <- filter(preds, interaction(data_source, intercept, OD) %in% c(
  "Moramanga.fixed.FALSE",
  "National.fixed.TRUE"
))
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

ggplot(
  data = preds,
  aes(x = ttimes, y = mean_bites, color = interaction(data_source, scale))
) +
  geom_point(
    data = observed, aes(
      x = ttimes_wtd / 60, y = avg_bites / pop * 1e5,
      shape = data_source
    ), color = "black", alpha = 0.5, size = 1.5,
    stroke = 1.2, inherit.aes = FALSE
  ) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(
    ymin = bites_lower, ymax = bites_upper,
    fill = interaction(data_source, scale)
  ),
  color = NA, alpha = 0.25
  ) +
  scale_color_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_fill_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  labs(x = "Travel time (hrs)", y = "Reported bites per 100k", tag = "A") +
  cowplot::theme_minimal_grid() -> mod_preds

# Plot posterior ests ------------------------------------------------------------------
all_samps_natl <- summarize_samps(parent_dir = "analysis/out/mods/samps/National/")
all_samps_mora <- summarize_samps(parent_dir = "analysis/out/mods/samps/Moramanga/")
all_samps_natl %>%
  filter(pop_predict == "flatPop", intercept == "fixed", OD == TRUE) %>%
  bind_rows(filter(all_samps_mora, pop_predict == "flatPop", OD == FALSE)) -> all_samps

# so that same # of hgrid lines across these
set_breaks <- function(limits) {
  round(seq(limits[1], limits[2], length.out = 5), 1)[2:4]
}

# hack for patchwork bug
ggplot(data = all_samps, aes(x = Parameter, y = value, fill = interaction(data_source, scale))) +
  geom_violin(alpha = 0.5) +
  scale_x_discrete(labels = c(
    "beta_0" = bquote(beta[0] ~ "(Intercept)"),
    "beta_ttimes" = bquote(beta[t] ~ "(Travel time effect)"),
    "sigma_e" = bquote(sigma[e] ~ "(Overdispersion)")
  )) +
  scale_fill_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs, guide = "none"
  ) +
  scale_y_continuous(breaks = set_breaks) +
  facet_wrap(~Parameter,
    scales = "free", ncol = 1,
    labeller = as_labeller(c("beta_0" = "", "beta_ttimes" = "", "sigma_e" = ""))
  ) +
  labs(y = "Posterior estimates", tag = "B") +
  theme_minimal_hgrid() -> posts

mods <- (mod_preds | posts) + plot_layout(guides = "collect")

write_create(
  mods,
  "analysis/figs/main/M5_mods.tiff",
  ggsave_it,
  dpi = 300, height = 8, width = 7.5,
  compression = "lzw", type = "cairo"
)
write_create(
  mods,
  "analysis/figs/main/M5_mods.jpeg",
  ggsave_it,
  height = 8, width = 7.5
)

# Session Info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

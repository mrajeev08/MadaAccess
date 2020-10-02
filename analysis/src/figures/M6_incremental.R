# ------------------------------------------------------------------------------------------------ #
#' Figure M7: Impact of expanded access
# ------------------------------------------------------------------------------------------------ #

source(here::here("R", "utils.R"))
start <- Sys.time()

# Set up
library(ggplot2)
library(data.table)
library(patchwork)
library(cowplot)

# read in natl preds
natl_preds <- fread(here_safe("analysis/out/preds/natl_preds.gz"))

# Colors
# labs & cols
scale_levs <- c("Commune", "District")
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(scale_labs) <- scale_levs
names(model_cols) <- scale_levs

# Vials
vials <- ggplot(
  data = natl_preds[scenario != max(scenario)],
  aes(x = as.numeric(scenario), y = vials_mean, color = scale, fill = scale)
) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper, ymin = vials_lower), color = NA, alpha = 0.35) +
  geom_pointrange(
    data = natl_preds[scenario == max(scenario)],
    aes(
      x = as.numeric(scenario), y = vials_mean,
      ymax = vials_upper, ymin = vials_lower, color = scale
    ),
    position = position_dodge(width = 50)
  ) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  labs(
    x = "", y = "Average annual\n vial demand",
    tag = "B"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Burden
burden <- ggplot(
  data = natl_preds[scenario != max(scenario)],
  aes(x = as.numeric(scenario), y = deaths_mean, color = scale, fill = scale)
) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), color = NA, alpha = 0.35) +
  geom_pointrange(
    data = natl_preds[scenario == max(scenario)],
    aes(
      x = as.numeric(scenario), y = deaths_mean,
      ymin = deaths_lower, ymax = deaths_upper, color = scale
    ),
    position = position_dodge(width = 50)
  ) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  labs(
    x = "", y = "Average annual\n deaths",
    tag = "A"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Vials per death
vials_per_death <- ggplot(
  natl_preds[scenario != max(scenario)],
  aes(x = as.numeric(scenario), y = vials_mean / averted_mean, color = scale, fill = scale)
) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymax = vials_upper / averted_upper, ymin = vials_lower / averted_lower), color = NA, alpha = 0.35) +
  geom_pointrange(
    data = natl_preds[scenario == max(scenario)],
    aes(
      x = as.numeric(scenario), y = vials_mean / averted_mean,
      ymax = vials_upper / averted_upper, ymin = vials_lower / averted_lower,
      color = scale
    ),
    position = position_dodge(width = 50)
  ) +
  scale_color_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  scale_fill_manual(values = model_cols, labels = scale_labs, name = "Scale") +
  labs(
    x = "# Additional ARMC", y = "Average vials per \n death averted",
    tag = "C"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


addARMC <- burden / vials / vials_per_death + plot_layout(guides = "collect")
write_create(
  addARMC,
  "analysis/figs/main/M6_addARMC.jpeg",
  ggsave_it,
  device = "jpeg", height = 8.75, width = 8
)
write_create(
  addARMC,
  "analysis/figs/main/M6_addARMC.tiff",
  ggsave_it,
  device = "tiff", dpi = 300, height = 8.75, width = 7.5,
  compression = "lzw", type = "cairo"
)

# Saving session info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)

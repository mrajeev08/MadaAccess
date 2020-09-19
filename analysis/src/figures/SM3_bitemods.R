# ------------------------------------------------------------------------------------------------ #
#' Candidate models of bite incidence
#' + sensitivity of model selection to correction for undersubmission of forms
# ------------------------------------------------------------------------------------------------ #

source(here::here("R", "utils.R"))
start <- Sys.time()

# source
source(here_safe("R/summarize_samps.R"))

# libraries
library(tidyverse)
library(cowplot)
library(data.table)
library(foreach)
library(coda)
library(iterators)
select <- dplyr::select

# source bite ests
district_bites <- fread(here_safe("analysis/out/bites/district_bites.csv"))
comm_covars <- fread(here_safe("analysis/out/bites/comm_covars.csv"))
mora_bites <- fread(here_safe("analysis/out/bites/mora_bites.csv"))

# correlations between pop + travel times ----------------------------------------------------
comm_covars$scale <- "Commune"
district_bites$scale <- "District"
covars <- bind_rows(district_bites, comm_covars)
covars$scale <- factor(covars$scale, levels = c("District", "Commune"))

ggplot(data = covars, aes(x = ttimes_wtd / 60, y = pop, color = scale)) +
  geom_point() +
  facet_wrap(~scale, scales = "free") +
  labs(x = "Travel times (hrs)", y = "Population") +
  theme_minimal_grid() +
  scale_color_manual(values = c("#35274A", "#0B775E"), guide = "none") -> S3.1_corrs

write_create(
  S3.1_corrs,
  here_safe("analysis/figs/supplementary/S3.1_corrs.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 5,
  width = 7
)

# candidate models of bite incidence --------------------------------------------------
scale_levs <- c("Moramanga.Commune", "National.Commune", "National.District")
scale_labs <- c("Moramanga", "Commune", "District")
model_cols <- c("#F2300F", "#0B775E", "#35274A")
names(scale_labs) <- scale_levs
names(model_cols) <- scale_levs

# Fitted predictions
preds_grouped <- read.csv(here_safe("analysis/out/mods/preds/fitted_grouped_all.csv"))
preds_grouped$mod_intercept <- preds_grouped$intercept
fitted_preds <- ggplot(
  data = filter(preds_grouped, OD == FALSE),
  aes(
    x = log(avg_bites + 0.1), y = log(mean_bites + 0.1),
    color = interaction(data_source, scale)
  )
) +
  geom_pointrange(aes(
    ymin = log(bites_lower + 0.1),
    ymax = log(bites_upper + 0.1)
  ), alpha = 0.5) +
  scale_color_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  facet_grid(pop_predict ~ mod_intercept,
    scales = "free_x",
    labeller = labeller(mod_intercept = c(
      "fixed" = "Fixed intercept",
      "random" = "Random intercept"
    )), drop = TRUE
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  xlab("log(Observed bites)") +
  ylab("log(Predicted bites)") +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  )

write_create(
  fitted_preds,
  here_safe("analysis/figs/supplementary/S3.2_fitted.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 10,
  width = 8
)

# Out of fit predictions
outfit_mora <- read.csv(here_safe("analysis/out/mods/preds/outfit_mora.csv"))
outfit_mada <- read.csv(here_safe("analysis/out/mods/preds/outfit_grouped_mada.csv"))

# Trying nested facet labels
outfit_mada$type <- "National"
outfit_mora$type <- "Moramanga"
outfit_mora$avg_bites <- outfit_mora$observed
outfit_all <- bind_rows(outfit_mada, outfit_mora)
outfit_all$mod_intercept <- factor(outfit_all$intercept)
levels(outfit_all$mod_intercept) <- list("Fixed intercept" = "fixed", "Random intercept" = "random")

type_labs <- c(
  "National" = "underline('National data predicted \n by Moramanga models')",
  "Moramanga" = "underline('Moramanga data predicted by National models')"
)

outfit_preds <- ggplot(
  data = filter(outfit_all, OD == FALSE),
  aes(
    x = log(avg_bites + 0.1), y = log(mean_bites + 0.1),
    color = interaction(data_source, scale)
  )
) +
  geom_pointrange(aes(
    ymin = log(bites_lower + 0.1),
    ymax = log(bites_upper + 0.1)
  ), alpha = 0.5) +
  scale_color_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  ggh4x::facet_nested(pop_predict ~ type + mod_intercept,
    scales = "free",
    labeller = labeller(type = as_labeller(type_labs, label_parsed))
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
  labs(x = "log(Observed bites) per 100k", y = "log(Predicted bites) per 100k") +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "gray92"),
    panel.grid = element_line(color = "white", size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_text(margin = margin(t = 15))
  )

write_create(
  outfit_preds,
  here_safe("analysis/figs/supplementary/S3.3_outfit.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 8,
  width = 12
)

# Comparing models with overdispersion and without -----------------------------------------

# Posteriors are now similar for all models
all_samps_natl <- summarize_samps(parent_dir = "analysis/out/mods/samps/National/")
all_samps_mora <- summarize_samps(parent_dir = "analysis/out/mods/samps/Moramanga/")
all_samps_mora %>%
  bind_rows(all_samps_natl) %>%
  filter(pop_predict == "flatPop") %>%
  mutate(Parameter = fct_recode(Parameter,
    `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
    `sigma[0]` = "sigma_0", `sigma[e]` = "sigma_e"
  )) -> all_samps
ggplot(
  data = filter(all_samps, !(grepl("alpha", Parameter))),
  aes(
    x = intercept, y = value,
    fill = interaction(data_source, scale)
  )
) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_x_discrete(labels = c(
    "fixed" = "Fixed",
    "random" = "Random"
  )) +
  labs(x = "Intercept type", y = "Posterior estimates") +
  facet_grid(Parameter ~ OD,
    scales = "free_y",
    labeller = labeller(
      OD = c(
        "FALSE" = "No overdispersion",
        "TRUE" = "Estimating\n overdispersion"
      ),
      Parameter = label_parsed
    )
  ) +
  theme_minimal_hgrid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  ) -> posts_all
write_create(
  posts_all,
  here_safe("analysis/figs/supplementary/S3.4_posts_all.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 8,
  width = 8
)

# And generates similar predictions (except for Mora data!)
preds <- read.csv(here_safe("analysis/out/mods/preds/expectations.csv")) # Expectations
district_bites$data_source <- "National"
mora_bites$data_source <- "Moramanga"
observed <- bind_rows(district_bites, mora_bites)

ggplot(data = preds, aes(x = ttimes, y = mean_bites, color = interaction(data_source, scale))) +
  geom_ribbon(aes(
    ymin = bites_lower, ymax = bites_upper,
    fill = interaction(data_source, scale)
  ),
  color = NA, alpha = 0.5
  ) +
  geom_point(
    data = observed, aes(
      x = ttimes_wtd / 60, y = avg_bites / pop * 1e5,
      shape = data_source
    ), color = "black", alpha = 0.5, size = 1.5,
    stroke = 1.2, inherit.aes = FALSE
  ) +
  geom_line(size = 1.1) +
  scale_color_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_fill_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k") +
  facet_grid(intercept ~ OD,
    scales = "free",
    labeller = labeller(
      OD = c(
        "FALSE" = "No overdispersion",
        "TRUE" = "Estimating\n overdispersion"
      ),
      intercept = c(
        "fixed" = "Fixed intercept",
        "random" = "Random intercept"
      )
    )
  ) +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  ) -> preds_OD

write_create(
  preds_OD,
  here_safe("analysis/figs/supplementary/S3.5_preds_OD.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 8,
  width = 8
)

# Random effects are no longer identifiable (although sigma_0 is estimated as non-zero)
all_samps_natl %>%
  filter(pop_predict == "flatPop", intercept == "random", grepl("alpha|beta_0", Parameter)) %>%
  mutate(Parameter = fct_recode(Parameter, `beta[0]` = "beta_0")) -> intercepts

ggplot(data = intercepts, aes(
  y = value, x = Parameter, fill = interaction(data_source, scale),
  color = interaction(data_source, scale)
)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  scale_x_discrete(labels = function(l) parse(text = l)) +
  scale_color_manual(
    aesthetics = c("color", "fill"),
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  facet_wrap(~OD,
    labeller = as_labeller(c(
      "FALSE" = "No overdispersion",
      "TRUE" = "Estimating\n overdispersion"
    )),
    scales = "free"
  ) +
  labs(x = "Intercept", y = "Estimates") +
  coord_flip() +
  theme_minimal_hgrid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  ) -> posts_intercepts

write_create(
  posts_intercepts,
  here_safe("analysis/figs/supplementary/S3.6_posts_intercepts.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 8,
  width = 8
)

# And random effects mods w/out OD generates similar predictions to fixed effect mods with OD
ggplot(
  data = filter(preds, interaction(OD, intercept, data_source) %in%
    c("TRUE.fixed.National", "FALSE.random.National")),
  aes(x = ttimes, y = mean_bites, color = interaction(data_source, scale))
) +
  geom_ribbon(aes(
    ymin = bites_lower, ymax = bites_upper,
    fill = interaction(data_source, scale)
  ),
  color = NA, alpha = 0.5
  ) +
  geom_point(
    data = observed, aes(
      x = ttimes_wtd / 60, y = avg_bites / pop * 1e5,
      shape = data_source
    ), color = "black", alpha = 0.5, size = 1.5,
    stroke = 1.2, inherit.aes = FALSE
  ) +
  geom_line(size = 1.1) +
  scale_color_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_fill_manual(
    values = model_cols, name = "Scale",
    labels = scale_labs
  ) +
  scale_shape_manual(values = c(6, 1), name = "Dataset") +
  labs(x = "Travel time (hrs)", y = "Predicted bites per 100k") +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  ) +
  facet_wrap(OD ~ intercept,
    labeller = labeller(
      OD = c(
        "FALSE" = "No overdispersion",
        "TRUE" = "Estimating overdispersion"
      ),
      intercept = c(
        "fixed" = "Fixed intercept",
        "random" = "Random intercept"
      )
    )
  ) -> preds_ODcomp

write_create(
  preds_ODcomp,
  here_safe("analysis/figs/supplementary/S3.7_preds_ODcomp.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 6,
  width = 8
)

# Sensitivity of model ests to reporting cut-offs ---------------------------------------------
bitedata_se <- fread(here_safe("analysis/out/mods/bitedata_se.csv"))
preds_se <- fread(here_safe("analysis/out/mods/expectations_se.csv"))

# labs
scale_labs <- c("Commune", "District")
model_cols <- c("#0B775E", "#35274A")
names(model_cols) <- scale_labs
cutoff_labs <- c("7" = "7 days", "15" = "15 days", "Inf" = "Uncorrected")

# Posterior estimates
all_samps_se <- summarize_samps(parent_dir = "analysis/out/mods/samps/National_se/")
all_samps_se %>%
  mutate(
    Parameter = fct_recode(Parameter,
      `beta[t]` = "beta_ttimes", `beta[0]` = "beta_0",
      `sigma[e]` = "sigma_e"
    ),
    rep_cutoff = gsub("[^0-9]", "", all_samps_se$filenames),
    rep_cutoff = fct_relevel(fct_recode(rep_cutoff,
      `Uncorrected` = "", `15 days` = "15",
      `7 days` = "7"
    ), "7 days", "15 days", "Uncorrected")
  ) -> all_samps_se

ggplot(data = filter(all_samps_se, !(grepl("alpha", Parameter))), aes(
  x = scale, y = value,
  fill = scale
)) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values = model_cols, name = "Model scale", labels = scale_labs) +
  scale_x_discrete(labels = NULL) +
  labs(x = "", y = "Posterior estimates") +
  facet_grid(Parameter ~ rep_cutoff,
    scales = "free_y",
    labeller = labeller(Parameter = label_parsed)
  ) +
  theme_minimal_hgrid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  ) -> posts_rep_se

write_create(
  posts_rep_se,
  here_safe("analysis/figs/supplementary/S3.8_posts_rep_se.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 8,
  width = 8
)

# Expected relationships
ggplot(
  data = preds_se,
  aes(x = ttimes, y = mean_bites, color = scale)
) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = bites_upper, ymax = bites_lower, fill = scale),
    color = NA, alpha = 0.5
  ) +
  geom_point(
    data = bitedata_se, aes(x = ttimes_wtd / 60, y = avg_bites / pop * 1e5), color = "black",
    alpha = 0.5, size = 1.5, stroke = 1.2, shape = 1
  ) +
  scale_color_manual(values = model_cols, name = "Scale") +
  scale_fill_manual(values = model_cols, name = "Scale") +
  labs(x = "Travel  times (hrs)", y = "Predicted bites per 100k") +
  theme(text = element_text(size = 20)) +
  facet_wrap(~rep_cutoff, nrow = 1, labeller = labeller(rep_cutoff = cutoff_labs)) +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(color = "NA", size = 1.2, fill = "grey92"),
    panel.grid = element_line(color = "white", size = 0.5)
  ) -> preds_rep_se


write_create(
  preds_rep_se,
  here_safe("analysis/figs/supplementary/S3.9_preds_rep_se.jpeg"),
  ggsave_it,
  device = "jpeg",
  height = 6,
  width = 8
)

# Session Info
out_session(logfile = here_safe("logs/log_local.csv"), start = start, ncores = 1)



source("R/functions/predict_functions.R")

library(bayesplot)

library(ggplot2)
library(ggridges)
library(cowplot)

samps <- load.mcmc(pop_predict = "flatPop", data_source = "National", intercept = "random", 
                   scale = "Commune", OD_suff = "", parent_dir = "output/mods/samps/")
samps <- load.mcmc(pop_predict = "flatPop", data_source = "National", intercept = "random", 
                   scale = "Commune", OD_suff = "_OD", parent_dir = "output/mods/samps/")


# Plot posterior ests ------------------------------------------------------------------
all_samps_natl <- summarize.samps(parent_dir = "output/mods/samps/National/")
intercepts <- filter(all_samps_natl, pop_predict == "flatPop", 
                         intercept == "random", 
                         grepl("alpha|beta_0", Parameter))
intercepts$Parameter <- as.character(intercepts$Parameter)
intercepts$Parameter[intercepts$Parameter == "beta_0"] <- "beta[0]"

ggplot(data = filter(intercepts, OD == FALSE), aes(x = value, y = Parameter, fill = scale)) +
  geom_density_ridges(scale = 2, color = NA, alpha = 0.75) +
  scale_y_discrete(labels = function(l) parse(text = l)) +
  facet_wrap(~ OD, labeller = as_labeller(c("FALSE" = "No overdispersion", 
                                            "TRUE" = "Estimating overdispersion"))) +
  scale_color_manual(name = "Model scale") + 
  labs(y = "Intercept", x = "Estimates") +
  theme_minimal_hgrid()

ggplot(data = intercepts_90, aes(y = value, x = Parameter, fill = OD)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(OD ~scale)

check <- as.array(samps)
check <- aperm(check, c(1, 3, 2)) 
dimnames(check) <- list(iterations = NULL, chains = c("chain:1", "chain:2", "chain:3"), parameters = dimnames(check)[[3]])
mcmc_intervals(check, regex_pars = "alpha|beta_0")
mcmc_pairs(check, regex_pars = "beta|sigma")

samps_comm <- load.mcmc(pop_predict = "flatPop", data_source = "National", intercept = "random", 
                   scale = "Commune", OD_suff = "", parent_dir = "output/mods/samps/")
samps_comm_OD <- load.mcmc(pop_predict = "flatPop", data_source = "National", intercept = "random", 
                   scale = "Commune", OD_suff = "_OD", parent_dir = "output/mods/samps/")
samps_dist <- load.mcmc(pop_predict = "flatPop", data_source = "National", intercept = "random", 
                        scale = "Commune", OD_suff = "", parent_dir = "output/mods/samps/")
samps_dist <- load.mcmc(pop_predict = "flatPop", data_source = "National", intercept = "random", 
                        scale = "Commune", OD_suff = "", parent_dir = "output/mods/samps/")

check <- as.array(samps_comm)
check <- aperm(check, c(1, 3, 2)) 
dimnames(check) <- list(iterations = NULL, chains = c("chain:1", "chain:2", "chain:3"), parameters = dimnames(check)[[3]])
mcmc_intervals(check, regex_pars = "alpha|beta_0")
mcmc_areas(check, prob = 0.8, prob_outer = 1.0, regex_pars = "alpha|beta_0") + bayesplot_theme_(cowplot::theme_minimal_hgrid)

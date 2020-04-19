# ------------------------------------------------------------------------------------------------ #
#' Looking at reasonable range of exposure incidence
#' Details: Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

# Set up ----------------------------------------------------------------------------------
library(data.table)
library(tidyverse)
library(cowplot)
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")
select <- dplyr::select

# From Claire's MSC
range_people_ph <- c(4.13, 5.654)
range_dogs_ph <- c(0.24, 0.26)
# Range of hdrs then
mora_hdrs <- range_people_ph/range_dogs_ph

# incidence from hdrs of Moramanga
inc_from_hdr(hdr = 5)
inc_from_hdr(hdr = 25)

# hdrs from exposure incidence estimated
hdr_from_inc(inc = 76)
hdr_from_inc(inc = 110)

# How to show relationship between these things ...
var_df <- expand.grid(hdr_val = seq(2, 40, by = 1), p_exp =  c(0.2, 0.38, 0.5), 
            dog_inc =  seq(0.005, 0.025, by = 0.001))
var_df %>%
  mutate(exp_inc = inc_from_hdr(hdr = hdr_val, p_exp = p_exp, dog_inc = dog_inc)) -> inc_exps
inc_labs <- as_labeller(c(`0.38`= "p[exp] == 0.38", `0.2` = "p[exp] == 0.2", `0.5` = "p[exp] == 0.5"), 
                          label_parsed)

S4.1_exprange <- ggplot(data = inc_exps, aes(x = hdr_val, y = dog_inc, 
                            fill = cut(inc_exps$exp_inc, breaks = c(1, 15, 42, 76, 110, Inf),
                                       labels = c("1 - 15", "15 - 42", "42 - 76", "76 - 110", "110 +")), 
                            color = ifelse(exp_inc > 42 & exp_inc < 110, "red", NA))) +
  geom_tile() +
  geom_hline(yintercept = 0.01, color = "darkred") +
  geom_vline(xintercept = c(5, 25)) +
  geom_vline(xintercept = round(mora_hdrs), linetype = 2, color = "grey50") +
  scale_fill_brewer(name = "Human exposures \n per 100k") +
  labs(x = "Human:dog ratio", y = "Dog rabies incidence (annual)") +
  scale_color_identity(guide = "none") +
  facet_wrap(~p_exp, nrow = 3, labeller = labeller(p_exp = inc_labs)) +
  theme_minimal_grid()

ggsave("figs/supplementary/S4.1_exprange.jpeg", S4.1_exprange, height = 8, width = 8)

# Saving session info
out.session(path = "R/figures/SM4_exprange.R", filename = "output/log_local.csv")



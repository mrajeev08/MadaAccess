####################################################################################################
##' Looking at reporting cut offs and translating to vial estimates 
##' Details:  
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(patchwork)
library(cowplot)
select <- dplyr::select
source("R/functions/utils.R")

## Read in metadata
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")

##' Plotting sensitivity to rle cut-offs
##' ------------------------------------------------------------------------------------------------
## throughput and reporting
throughput <- read.csv("output/sensitivity/throughput.csv")
reporting <- read.csv("output/sensitivity/reporting.csv")

##' Colors
catch_cols <- ctar_metadata$color
names(catch_cols) <- ctar_metadata$CTAR
throughput_cols <- c("0" = "white", "< 10" = '#edf8fb', "< 20" = '#b3cde3',
                     "< 40" = '#8c96c6', "< 80" = '#8856a7', "< 100" = '#810f7c')
throughput_brks <- c(-0.1, 0.1, 10, 20, 40, 80, 100)
throughput$date_reported <- ymd(throughput$date_reported)

figS2.1A <- ggplot(data = throughput, aes(x = date_reported, y = reorder(ctar, include_15))) + 
  geom_tile(aes(fill = ifelse(include_15 == 0, NA, as.character(ctar)),
                  alpha = cut(no_patients, breaks = throughput_brks, labels = names(throughput_cols)))) +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_alpha_manual(values = seq(0.5, 1, length.out = 7),
                    labels = names(throughput_cols), name = "Number of \n patients",
                    na.translate = FALSE, guide = "none") +
  xlim(ymd("2014-01-01"), ymd("2017-12-31")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  xlab("Year") +
  ylab("ARMC") +
  labs(tag = "A")

figS2.1B <- ggplot(data = reporting, aes(x = reorder(ctar, include_15), y = include_15, 
                                              color = ctar)) +
  geom_boxplot() +
  geom_point(alpha = 0.5) +
  labs(y = "Estimated reporting", x = "") +
  geom_hline(yintercept = 0.25, linetype = 2, color = "grey") +
  coord_flip() +
  scale_color_manual(values = catch_cols, guide = "none") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) 

##' Estimates of vials from reporting 
##' ------------------------------------------------------------------------------------------------
##' ! READ IN VIALS TO PLOT ! ##
vial_comp <- read.csv("output/sensitivity/vial_comp.csv")
figS2.1C <- ggplot(data = vial_comp, aes(x = reorder(ctar, reporting), shape = factor(cut_off), 
                                             color = ctar)) +
  geom_point(aes(y = ((log(mean3) + log(mean4))/2) - log(vials_observed)), size = 2) +
  geom_linerange(aes(ymin = log(mean3) - log(vials_observed), ymax = log(mean4) - log(vials_observed))) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  scale_shape_manual(values = c(0, 1), name = "Correction for \n underreporting", 
                     labels = c("15" = "15 day cut-off", "Inf" = "none")) +
  labs(y = "Difference between \n log(estimated) and log(observed) vials", x = "", tag = "B") +
  coord_flip() +
  scale_color_manual(values = catch_cols, guide = "none")

figS2.1 <- 
  {(figS2.1A | figS2.1B) + plot_layout(ncol = 2, widths = c(2.5, 1))} / 
  {(figS2.1C + plot_spacer()) + plot_layout(ncol = 2, widths = c(3, 1))} 
ggsave("figs/supplementary/S2.1.jpeg", figS2.1, device = "jpeg", height = 6, width = 8.75)

##' Contacts 
##' ------------------------------------------------------------------------------------------------
##' Differences in travel times
mora_bites <- fread("data/processed/bitedata/moramanga.csv")
mora_bites$ttimes <- mada_communes$ttimes_wtd[match(mora_bites$commcode, mada_communes$commcode)]

figS2.2 <- ggplot(data = filter(mora_bites, type != "transfer"), aes(x = factor(known_cat1), y = ttimes/60)) +
  geom_boxplot() +
  theme_minimal_hgrid() +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Category I", y = "Travel times (hrs)")

## figS2.2
ggsave("figs/supplementary/S2.2.jpeg", figS2.2, device = "jpeg", height = 5, width = 5)

##' Contact comparison
contacts_natl <- read.csv("output/sensitivity/contacts_natl.csv")
contacts_mora <- read.csv("output/sensitivity/contacts_mora.csv")

##' figS2.3A
figS2.3A <- ggplot(data = contacts_natl, aes(x = ctar, y = no_patients, color = as.factor(estimated_cat1))) +
  scale_color_manual(name = "Exclude", values = c("navy", "red"), labels = c("No", "Yes")) +
  ylab("Number of patients per day") +
  xlab("ARMC") +
  geom_jitter(alpha = 0.5, width = 0.25) +
  coord_flip() +
  labs(tag = "A")

##' fig S2.3B
figS2.3B <- ggplot(contacts_mora, aes(x = as.numeric(sd), y = excluded, color = prop)) +
  geom_line() +
  scale_color_manual(name = "Type of patient", values = c("blue", "red"), 
                     labels = c("Cat II/II", "Cat I")) + 
  xlab("Number of standard \n deviations above the mean") +
  ylab("Proportion excluded") +
  labs(tag = "B")

figS2.3 <- figS2.3A | figS2.3B

## figS2.3
ggsave("figs/supplementary/S2.3.jpeg", figS2.3, device = "jpeg", height = 7, width = 9)

##' Saving session info
out.session(path = "R/figures/SM2_bitedata.R", filename = "sessionInfo.csv")


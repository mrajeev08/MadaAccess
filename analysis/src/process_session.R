# ------------------------------------------------------------------------------------------------ #
#' Processing out session & getting packages
# ------------------------------------------------------------------------------------------------ #

library(tidyverse)
library(knitr)
library(lubridate)
library(RCurl)
library(glue)

# Do out_session here so any packages used in this script get included too!
source(here::here("R", "utils.R"))
out_session("R/process_session.R", filename = "analysis/out/log_local")

# Find all .R & .Rmd files 
files <- list.files(recursive = TRUE)
files <- files[grep(".R$|.Rmd$", files)]
files <- files[grep("archive", files, invert = TRUE)] # excluding these that are archived

# Pull in local log
log_local <- read.csv("analysis/out/log_local.csv", stringsAsFactors = FALSE)
log_local <- as.data.frame(apply(log_local, 2, function (x) as.character(x)), 
                           stringsAsFactors = FALSE)

# Pull down log from cluster
# Make sure you're vpn'd in before doing this
# system("rsync -rvlzt --update mrajeev@della.princeton.edu:~/MadaAccess/log_cluster.csv ~/Documents/Projects/MadaAccess/output/")
log_cluster <- read.csv("analysis/out/log_cluster.csv", stringsAsFactors = FALSE)
log_cluster <- as.data.frame(apply(log_cluster, 2, function (x) as.character(x)),
                             stringsAsFactors = FALSE)
log_cluster %>%
  filter(path %in% files) %>%
  mutate(timestamp = parse_date_time(timestamp, "Ymd HMS")) %>%
  group_by(path) %>%
  filter(timestamp == max(timestamp)) -> log_cluster

# Process the logs to get the package list
log_local %>%
  filter(path %in% files) %>%
  mutate(timestamp = parse_date_time(timestamp, "Ymd HMS")) %>%
  group_by(path) %>%
  filter(timestamp == max(timestamp)) %>%
  bind_rows(log_cluster) -> log_scripts

# write log out of when scripts last ran
write.csv(log_scripts, "analysis/out/logs/log_last_ran.csv", row.names = FALSE)

log_scripts %>%
  pivot_longer(-(ran:running), names_to = "pkgs", values_to = "version") %>%
  ungroup() %>%
  filter(version != "", !is.na(version)) %>%
  distinct(pkgs, version) -> pkgs_used

# Package sources (will change as versions get pushed to archive)
source_urls <- glue::glue("https://cran.r-project.org/src/contrib/Archive/{pkgs_used$pkgs}/{pkgs_used$pkgs}_{pkgs_used$version}.tar.gz")
exists <- RCurl::url.exists(source_urls)
source_urls[!exists] <- glue::glue("https://cran.r-project.org/src/contrib/{pkgs_used$pkgs[!exists]}_{pkgs_used$version[!exists]}.tar.gz")
exists <- RCurl::url.exists(source_urls)

# these are any that were installed from github
github_lookup <- c(patchwork = "https://github.com/thomasp85/patchwork") 
names(source_urls) <- pkgs_used$pkgs
source_urls[!exists] <- github_lookup[match(names(github_lookup), names(source_urls[!exists]))] 

pkgs_used$source <- source_urls

# Write table & bib out for citing in supplement 
write.csv(pkgs_used, "analysis/out/logs/pkgs_used.csv", row.names = FALSE)
write_bib(pkgs_used$pkgs, "docs/bib/refs_pkgs.bib")

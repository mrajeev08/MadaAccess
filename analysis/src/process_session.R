# ------------------------------------------------------------------------------
#' Processing out session & getting packages
# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(knitr)
library(lubridate)
source(here::here("R", "utils.R"))

# Find all .R & .Rmd files
files <- list.files(recursive = TRUE)
files <- files[grep(".R$|.Rmd$", files)]
files <- files[grep("archive", files, invert = TRUE)] # excluding these that are archived

# Pull in local log
log_local <- read_csv("logs/log_local.csv")

# Pull down log from cluster
# Make sure you're vpn'd in before doing this
# system("rsync -rvlzt --update mrajeev@della.princeton.edu:~/MadaAccess/log_cluster.csv ~/Documents/Projects/MadaAccess/logs/")
log_cluster <- read_csv("logs/log_cluster.csv")

# Process the logs to get the package list
log_cluster %>%
  filter(path %in% files) %>%
  mutate(timestamp = parse_date_time(timestamp, "Ymd HMS")) %>%
  group_by(path) %>%
  filter(timestamp == max(timestamp)) -> log_cluster

log_local %>%
  filter(path %in% files) %>%
  mutate(timestamp = parse_date_time(timestamp, "Ymd HMS")) %>%
  group_by(path) %>%
  filter(timestamp == max(timestamp)) %>%
  bind_rows(log_cluster) -> log_scripts

# Do unique rows to get when last ran & requirements
log_scripts %>%
  select(-packages, -version, -status) %>%
  dplyr::distinct() -> log_last_ran

# write log out of when scripts last ran
write_create(log_last_ran,
             here_safe("logs/log_last_ran.csv"),
             write.csv,
             row.names = FALSE)

log_scripts %>%
  ungroup() %>%
  select(packages, version, status) %>%
  distinct() %>%
  filter(status == "attached") %>%
  group_by(packages) %>%
  filter(version == min(version)) -> pkgs_used

# Package sources (including github)
installed <- sapply(pkgs_used$packages, require, character.only = TRUE,
                    quietly = TRUE)
out <- sessioninfo::session_info()
pkgs_used %>%
  mutate(source = out$packages$source[match(packages,
                                            out$packages$package)]) -> pkgs_used

# Write table & bib out for citing in supplement
write_create(pkgs_used,
             here_safe("logs/pkgs_used.csv"),
             write.csv,
             row.names = FALSE)
write_create(pkgs_used$packages,
             here_safe("analysis/paper/bib/refs_pkgs.bib"),
             write_bib)

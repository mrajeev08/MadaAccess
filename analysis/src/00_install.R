# ------------------------------------------------------------------------------
#' Install needed packages
# ------------------------------------------------------------------------------

# Get list of needed packages
pkgs_used <- read.csv("analysis/out/logs/pkgs_used.csv", stringsAsFactors = FALSE)
installed <- sapply(pkgs_used$pkgs, require, character.only = TRUE, quietly = TRUE)
to_install <- pkgs_used$pkgs[!installed]

# Install needed packages
install.packages(to_install, repos = "http://cloud.r-project.org")

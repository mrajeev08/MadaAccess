# ------------------------------------------------------------------------------------------------ #
#' Install needed packages                                                                                       
# ------------------------------------------------------------------------------------------------ #

# Get list of needed packages
pkgs_used <- read.csv("output/logs/pkgs_used.csv", stringsAsFactors = FALSE)
installed <- sapply(pkgs_used$pkgs, require, character.only = TRUE, quietly = TRUE)
to_install <- pkgs_used$pkgs[!installed]

# Install needed packages
install.packages(to_install, repos = "http://cloud.r-project.org")

# Comparing installed versions 
sapply(pkgs_used$pkgs, require, character.only = TRUE, quietly = TRUE) # run this again to load pkgs
version_comp <- unlist(lapply(pkgs_used$pkgs, function(x) as.character(packageVersion(x))))
version_installed <- gsub("-", ".", as.character(pkgs_used$version))

# If any installed are older, update these
to_update <- pkgs_used$pkgs[version_comp < version_installed]
update.packages(to_update, repos = "http://cloud.r-project.org")
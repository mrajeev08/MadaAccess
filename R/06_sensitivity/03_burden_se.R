# ------------------------------------------------------------------------------------------------ #
#' Getting sensitivity to parameter assumptions for burden & impact of expanding access                          
# ------------------------------------------------------------------------------------------------ #

# check seff to see how much mem I should get ~ 3 gb or default?

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
Sys.time()

# Set up 
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
library(triangle)
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")
select <- dplyr::select

# Filter to only do ones for which travel times have changed
commune_master <- fread("output/ttimes/commune_maxcatch.gz")
setorder(commune_master, commcode, scenario)
commune_master[, diff_comm := ttimes_wtd - shift(ttimes_wtd, 1), by = commcode]
commune_master[, diff_dist := ttimes_wtd_dist - shift(ttimes_wtd_dist, 1), by = commcode]
comm_run <- commune_master[diff_comm != 0 | scenario %in% c(0, 1648)]
dist_run <- commune_master[diff_dist != 0 | scenario %in% c(0, 1648)]
rm(commune_master) # cleaning up memory!
gc()

# Bring in sensitivity parameters
se_pars <- fread("output/sensitivity/se_pars.csv")
nrow(se_pars)

# Predictions ---------------------------------------------------------------------------
# This takes a good bit of time (abt 20 min) & RAM so running on cluster
# So summarizing stats ahead of time
# i.e. need the admin level burden for the baseline scenario == 0 
# and the national level burden for the rest of the scenarios (split this way?)

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(j = iter(se_pars, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach', 'triangle', 'foreach', 'dplyr',
                      'tidyr')) %dopar% {
          
  print(j)
  if(j$scale == "Commune"){
    admin <- comm_run # these are data.tables so hopefully will not copy only point!
    ttimes <- admin$ttimes_wtd/60
  }
  
  if(j$scale == "District"){
    admin <- dist_run
    ttimes <- admin$ttimes_wtd_dist/60
  }
  
  bite_mat <- predict.bites(ttimes = ttimes, pop = admin$pop, 
                                 catch = admin$catchment, names = admin$commcode, 
                                 beta_ttimes = j$beta_ttimes, beta_0 = j$beta_0, 
                                 beta_pop = 0, sigma_0 = j$sigma_0, known_alphas = NA, 
                                 pop_predict = "flatPop", intercept = "random", 
                                 trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")
  
  all_mats <-  predict.deaths(bite_mat, pop = admin$pop,
                              p_rab_min = j$p_rab_min, p_rab_max = j$p_rab_max,
                              rho_max = j$rho_max, exp_min = j$exp_min, exp_max = j$exp_max,
                              prob_death = j$p_death, dist = "triangle")

  all_mats <- c(list(bites = bite_mat), all_mats)

  foreach(i = 1:length(all_mats), .combine = 'cbind') %do% {
    mat <- all_mats[[i]]
    labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
    mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
    sd <- apply(mat, 1, sd, na.rm = TRUE)
    upper <- mean + 1.96*sd/sqrt(ncol(mat))
    lower <- mean - 1.96*sd/sqrt(ncol(mat))
    out <- data.table(mean, upper, lower)
    names(out) <- labels
    out
  } -> admin_comm
  
  admin_comm <- data.table(names = admin$commcode,
                           ttimes = ttimes, pop = admin$pop, 
                           catch = admin$catchment, scenario = admin$scenario, 
                           scale = j$scale, vary = j$vary, direction = j$direction, admin_comm)
  
  max_clinics <- max(admin$scenario[admin$scenario != 1648])
  
  admin_base <- admin_comm[scenario == 0] 
    
  admin_comm %>%
    complete(scenario = 1:max_clinics, names) %>% # from 1 to last
    group_by(names) %>%
    arrange(scenario) %>%
    fill(3:ncol(admin_comm), .direction = "down") -> admin_comm
  
  ## This should be true!
  admin_comm %>%
    group_by(scenario) %>%
    summarize(n_admin = n()) %>%
    summarize(all(n_admin == 1579)) # should be true for all of them!
  
  admin_comm %>%
    group_by(scenario) %>%
    summarize_at(vars(bites_mean:averted_lower, pop), sum, na.rm = TRUE) -> natl_preds
  
  natl_preds <- data.table(natl_preds, scale = j$scale, vary = j$vary, direction = j$direction)
  
  list(base = admin_base, natl = natl_preds)
  
} -> burden_se

# Write out data
fwrite(burden_se[["base"]], "output/sensitivity/burden_baseline_se.gz") 
fwrite(burden_se[["natl"]], "output/sensitivity/burden_addclinics_se.gz") 

# Close out
file_path <- "R/06_sensitivity/03_burden_se.R"
out.session(path = file_path, filename = "log_cluster.csv")

# Parse these from bash for where to put things
sync_to <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
sync_from <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/burden*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()

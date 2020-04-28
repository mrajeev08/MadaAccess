# ------------------------------------------------------------------------------------------------ #
#' Vial sensitivity to bite model predictions
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 3000 -sp "./R/06_sensitivity/04_vial_se.R" -jn vials_se -wt 5m -n@
  
# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# Libraries and scripts
library(doParallel)
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(dplyr)
library(tidyr)
select <- dplyr::select
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")

# Set-up these two vectors for reading in data using cmd line args
scenario_loop <- unique(fread("output/ttimes/commune_maxcatch.csv")$lookup)
colnames_max <- colnames(fread("output/ttimes/commune_maxcatch.csv"))
colnames_all <- colnames(fread("output/ttimes/commune_allcatch.csv"))

# Vials given commune/district ttimes -----------------------------------------------------------------
# Bring in sensitivity parameters
vial_se_pars <- fread("output/sensitivity/se_pars.csv")[grep("beta|sigma", vary)]

# make df with the lookup + scale
# reverse so bigger dfs don't slow everything down
lookup <- expand_grid(loop = rev(scenario_loop), vial_se_pars)


foreach(j = iter(lookup, by = "row"), .combine = rbind, 
        .packages = c('data.table', 'foreach', 'dplyr'), .options.RNG = 2607) %dorng% {
          
          # read in max and all catch
          comm <- fread(cmd = paste("grep -w ", j$loop, 
                                    " output/ttimes/commune_maxcatch.csv", 
                                    sep = ""), col.names = colnames_max)
          comm_all <- fread(cmd = paste("grep -w ", j$loop, 
                                        " output/ttimes/commune_allcatch.csv", 
                                        sep = ""), col.names = colnames_all)
          if(j$scale == "Commune") {
            ttimes <- comm$ttimes_wtd/60
          } 
          
          if(j$scale == "District") {
            ttimes <- comm$ttimes_wtd_dist/60
          }
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = j$beta_ttimes, beta_0 = j$beta_0, 
                                    beta_pop = j$beta_pop, sigma_e = j$sigma_e, sigma_0 = j$sigma_0,
                                    known_alphas = NA, OD = j$OD,
                                    pop_predict = j$pop_predict, intercept = j$intercept, 
                                    trans = 1e5, known_catch = FALSE, nsims = 1000, 
                                    par_type = "point_est", pred_type =  "exp")
          
          bites <- data.table(commcode = comm$commcode, scenario = comm$scenario,
                              bite_mat)
          
          check <- comm_all[bites, on = c("commcode", "scenario")]
          
          cols <- names(check[, .SD, .SDcols = grepl("result", names(check), fixed = TRUE)])
          
          check[, (cols) := lapply(.SD, function(x) x*prop_pop_catch), .SDcols = cols]
          
          bites_by_catch <- check[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, 
                                  by = c("catchment", "scenario")]
          
          catch_mat <- as.matrix(bites_by_catch[, .SD, .SDcols = cols])
          
          vial_preds <- sapply(catch_mat, get.vials)
          
          # vials
          vials <- matrix(unlist(vial_preds["vials", ]), nrow = nrow(catch_mat), 
                          ncol = ncol(catch_mat))
          vials_natl <- colSums(vials, na.rm = TRUE) # for each sim
          vials_natl_mean <- mean(vials_natl)
          vials_natl_upper <- quantile(vials_natl, prob = 0.975, na.rm = TRUE)
          vials_natl_lower <- quantile(vials_natl, prob = 0.025, na.rm = TRUE)
          
          natl_preds <- data.table(scenario = bites_by_catch$scenario[1], 
                                   scale = j$scale, data_source = j$data_source, vary = j$vary, 
                                   direction = j$direction, 
                                   vials_mean = vials_natl_mean, vials_upper = vials_natl_upper, 
                                   vials_lower = vials_natl_lower)
    } -> natl_vials_se

fwrite(natl_vials_se, "output/sensitivity/natl_vials_se.gz")

# Close out
file_path <- "R/06_sensitivity/05_vial_se.R"
out.session(path = file_path, filename = "output/log_cluster.csv", start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/natl_vials_se.gz"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


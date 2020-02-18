# ------------------------------------------------------------------------------------------------ #
#' Estimating vials
#' Details: Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
Sys.time()

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
colnames <- colnames(fread("output/ttimes/commune_maxcatch.csv"))

# Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "random") -> model_means

# Vials given commune/district ttimes -----------------------------------------------------------------

# make df with the lookup + scale
# reverse so bigger dfs don't slow everything down
lookup <- expand.grid(loop = rev(scenario_loop), scale = c("Commune", "District"))

foreach(j = iter(lookup, by = "row"), .combine = rbind, 
        .packages = c('data.table', 'foreach')) %dopar% {
          
          params <- model_means[model_means$scale == j$scale, ]
          
          # read in max and all catch
          comm <- fread(cmd = paste("grep -w ", j$loop, 
                                    " output/ttimes/commune_maxcatch.csv", 
                                    sep = ""), col.names = colnames)
          comm_all <- fread(cmd = paste("grep -w ", j$loop, 
                                        " output/ttimes/commune_maxcatch.csv", 
                                        sep = ""), col.names = colnames)
          if(j$scale == "Commune") {
            ttimes <- comm$ttimes_wtd/60
          } 
          
          if(j$scale == "District") {
            ttimes <- comm$ttimes_wtd_dist/60
          }
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                                    beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                                    pop_predict = params$pop_predict, intercept = params$intercept, 
                                    trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")
          
          bites <- data.table(commcode = comm$commcode, scenario = comm$scenario,
                              bite_mat)
          
          check <- comm_all[bites, on = c("commcode", "scenario")]
          
          cols <- names(check[, .SD, .SDcols = result.1:result.1000])
          
          check[, (cols) := lapply(.SD, function(x) rpois(length(x), 
                                                x*prop_pop_catch)), .SDcols = cols]
          bites_by_catch <- check[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, 
                                  by = c("catchment", "scenario")]
          
          catch_mat <- as.matrix(bites_by_catch[, .SD, .SDcols = cols])
          
          vial_preds <- sapply(catch_mat, get.vials)
          
          # vials
          vials <- matrix(unlist(vial_preds["vials", ]), nrow = nrow(catch_mat), 
                          ncol = ncol(catch_mat))
          vials_mean <- rowMeans(vials, na.rm = TRUE)
          vials_sd <- apply(vials, 1, sd, na.rm = TRUE)
          vials_upper <- vials_mean + 1.96*vials_sd/sqrt(length(vials))
          vials_lower <- vials_mean - 1.96*vials_sd/sqrt(length(vials))
          
          # throughput
          tp <- matrix(unlist(vial_preds["throughput", ]), nrow = nrow(catch_mat), 
                       ncol = ncol(catch_mat))
          tp_mean <- rowMeans(tp, na.rm = TRUE)
          tp_sd <- apply(tp, 1, sd, na.rm = TRUE)
          tp_upper <- tp_mean + 1.96*tp_sd/sqrt(length(vials))
          tp_lower <- tp_mean - 1.96*tp_sd/sqrt(length(vials))
          
          # bites
          bites_mean <- rowMeans(catch_mat, na.rm = TRUE)
          bites_sd <- apply(catch_mat, 1, sd, na.rm = TRUE)
          bites_upper <- bites_mean + 1.96*bites_sd/sqrt(length(bites))
          bites_lower <- bites_mean - 1.96*bites_sd/sqrt(length(bites))
          
          out <- data.table(catchment = bites_by_catch$catchment, 
                            scenario = bites_by_catch$scenario, scale = j$scale,
                            vials_mean, vials_sd, vials_upper, vials_lower, 
                            tp_mean, tp_sd, tp_upper, 
                            tp_lower, bites_mean, bites_sd, bites_upper, bites_lower)
          out
} -> catch_preds

fwrite(catch_preds, "output/preds/catch_preds.gz")

# Close out
file_path <- "R/05_predictions/02_vials.R"
out.session(path = file_path, filename = "log_cluster.csv")

# Parse these from bash for where to put things
sync_to <- "~/Documents/Projects/MadaAccess/output/preds/"
sync_from <- "mrajeev@della.princeton.edu:~/MadaAccess/output/preds/catch_preds.gz"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


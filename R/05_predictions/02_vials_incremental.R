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
select <- dplyr::select
source("R/functions/predict_functions.R")
source("R/functions/out.session.R")

# Set-up these two dfs to be unzipped & with a look up var!
commune_allcatch <- fread("output/ttimes/commune_allcatch.gz")
commune_allcatch$lookup <- paste("scenario", commune_allcatch$scenario,
                                 sep = "_")
fwrite(commune_allcatch, "output/ttimes/commune_allcatch.csv")
commune_maxcatch <- fread("output/ttimes/commune_maxcatch.gz")
commune_maxcatch$lookup <- paste("scenario", commune_maxcatch$scenario,
                                 sep = "_")
fwrite(commune_maxcatch, "output/ttimes/commune_maxcatch.csv")
scenario_loop <- unique(commune_allcatch$lookup)
colnames <- colnames(commune_maxcatch)
rm(commune_allcatch, commune_maxcatch)
gc()

# Get model means for commune and district models
model_ests <- read.csv("output/mods/estimates.csv")
model_ests %>%
  dplyr::select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  tidyr::spread(key = params, value = Mean, fill = 0) %>%
  dplyr::filter(pop_predict == "flatPop", data_source == "National", 
                intercept == "random") -> model_means

# Vials given commune ttimes -----------------------------------------------------------------
params <- model_means[model_means$scale == "Commune", ]

# Getting bites by catchment
foreach(i = 1:length(scenario_loop), .combine = rbind, 
        .packages = c('data.table', 'dplyr', 'foreach')) %dopar% {

  # read in max and all catch
  comm <- fread(cmd = paste("grep -w ", scenario_loop[i], " output/ttimes/commune_maxcatch.csv", 
                            sep = ""), col.names = colnames)
  comm_all <- fread(cmd = paste("grep -w ", scenario_loop[i], " output/ttimes/commune_maxcatch.csv", 
                                sep = ""), col.names = colnames)
  
  bite_mat <- predict.bites(ttimes = comm$ttimes_wtd/60, pop = comm$pop, 
                            catch = comm$catchment, names = comm$commcode, 
                            beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
                            beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
                            pop_predict = params$pop_predict, intercept = params$intercept, 
                            trans = 1e5, known_catch = FALSE, nsims = 10, type = "inc")
  
  bites <- data.table(commcode = comm$commcode, scenario = comm$scenario,
                      bite_mat)
  
  check <- comm_all[bites, on = c("commcode", "scenario")]
  
  cols <- names(check[, .SD, .SDcols = result.1:result.10])
  
  check[, (cols) := lapply(.SD, function(x) rpois(length(x), 
                                        x*prop_pop_catch)), .SDcols = cols]
  bites_by_catch <- check[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, 
                          by = c("catchment", "scenario")]
  bites_by_catch
} -> catch_bites

# write with fread!
fwrite(catch_bites, "output/preds/bites_by_catch_comm.gz")
print(paste("check class:", class(catch_bites)))
print(colnames(catch_bites), sep = "\n")
catch_mat <- as.matrix(catch_bites[, .SD, .SDcols = result.1:result.10])

rm(catch_bites)
gc()

# data.table way 
print("These are how many rows that need to be done:")
print(nrow(catch_mat))

# Getting vials by catchment
foreach(bycatch = iter(catch_mat, by = "row"), .combine = rbind, 
        .packages = 'data.table') %dopar% {
  # vials
  vial_preds <- sapply(bycatch, get.vials)
  vials <- unlist(vial_preds["vials", ])
  vials_mean <- mean(vials, na.rm = TRUE)
  vials_sd <- sd(vials, na.rm = TRUE)
  vials_upper <- vials_mean + 1.96*vials_sd/sqrt(length(vials))
  vials_lower <- vials_mean - 1.96*vials_sd/sqrt(length(vials))
  
  # throughput
  throughput <- unlist(vial_preds["throughput", ])
  throughput_mean <- mean(throughput, na.rm = TRUE)
  throughput_sd <- sd(throughput, na.rm = TRUE)
  throughput_upper <- throughput_mean + 1.96*throughput_sd/sqrt(length(throughput))
  throughput_lower <- throughput_mean - 1.96*throughput_sd/sqrt(length(throughput))
  
  # avg bites annually
  bites_mean <- mean(bycatch, na.rm = TRUE)
  bites_sd <- sd(bycatch, na.rm = TRUE)
  bites_upper <- bites_mean + 1.96*bites_sd/sqrt(length(bites))
  bites_lower <- bites_mean - 1.96*bites_sd/sqrt(length(bites))
  
  out <- data.table(vials_mean, vials_sd, vials_upper, vials_lower, 
                    throughput_mean, throughput_sd, throughput_upper, 
                    throughput_lower, bites_mean, bites_sd, bites_upper, 
                    bites_lower)
} -> vials

vials <- data.table(catchment = bites_by_catch$catchment, 
                    scenario = bites_by_catch$scenario, vials, scale = "Commune")

# write comm vials
fwrite(vials, "output/preds/vials_comm.gz")

# # Vials given district ttimes ----------------------------------------------------------------
# params <- model_means[model_means$scale == "District", ]
# 
# foreach(i = 1:length(scenario_loop), .combine = rbind) %do% {
#   # read in max and all catch
#   comm <- fread(cmd = paste("grep -w ", scenario_loop[i], " output/ttimes/commune_maxcatch.csv", 
#                             sep = ""), col.names = colnames)
#   comm_all <- fread(cmd = paste("grep -w ", scenario_loop[i], " output/ttimes/commune_maxcatch.csv", 
#                                 sep = ""), col.names = colnames)
#   
#   bite_mat <- predict.bites(ttimes = comm$ttimes_wtd_dist/60, pop = comm$pop, 
#                             catch = comm$catchment, names = comm$commcode, 
#                             beta_ttimes = params$beta_ttimes, beta_0 = params$beta_0, 
#                             beta_pop = 0, sigma_0 = params$sigma_0, known_alphas = NA, 
#                             pop_predict = params$pop_predict, intercept = params$intercept, 
#                             trans = 1e5, known_catch = FALSE, nsims = 1000, type = "inc")
#   
#   bites <- data.table(commcode = comm$commcode, scenario = comm$scenario,
#                       bite_mat)
#   check <- comm_all[bites, on = c("commcode", "scenario")]
#   check <- mutate_at(check, vars(result.1:result.1000), function(x) rpois(length(x), 
#                                                                           x*check$prop_pop_catch))
#   check %>%
#     group_by(catchment, scenario) %>%
#     summarize_at(vars(result.1:result.1000), sum, na.rm = TRUE) -> bites_by_catch
#   
#   bite_mat <- as.matrix(select(ungroup(bites_by_catch), result.1:result.1000))
#   
#   # Make this foreach loop parallel so that you can minimize time spent waiting on tasks!
#   foreach(bycatch = iter(bite_mat, by = "row"), .combine = rbind,
#           .options.RNG = 323) %dorng% {
#             # vials
#             vial_preds <- sapply(bycatch, get.vials)
#             vials <- unlist(vial_preds["vials", ])
#             vials_mean <- mean(vials, na.rm = TRUE)
#             vials_sd <- sd(vials, na.rm = TRUE)
#             vials_upper <- vials_mean + 1.96*vials_sd/sqrt(length(vials))
#             vials_lower <- vials_mean - 1.96*vials_sd/sqrt(length(vials))
#             
#             # throughput
#             throughput <- unlist(vial_preds["throughput", ])
#             throughput_mean <- mean(throughput, na.rm = TRUE)
#             throughput_sd <- sd(throughput, na.rm = TRUE)
#             throughput_upper <- throughput_mean + 1.96*throughput_sd/sqrt(length(throughput))
#             throughput_lower <- throughput_mean - 1.96*throughput_sd/sqrt(length(throughput))
#             
#             # avg bites annually
#             bites_mean <- mean(bycatch, na.rm = TRUE)
#             bites_sd <- sd(bycatch, na.rm = TRUE)
#             bites_upper <- bites_mean + 1.96*bites_sd/sqrt(length(bites))
#             bites_lower <- bites_mean - 1.96*bites_sd/sqrt(length(bites))
#             
#             out <- data.table(vials_mean, vials_sd, vials_upper, vials_lower, 
#                               throughput_mean, throughput_sd, throughput_upper, 
#                               throughput_lower, bites_mean, bites_sd, bites_upper, 
#                               bites_lower)
#           } -> vials
#   
#   out <- data.table(catchment = bites_by_catch$catchment, 
#                     scenario = bites_by_catch$scenario, vials, scale = "District")
# } -> vials_district
# 
# vials_all <- rbind(vials_district, vials_commune)
# fwrite(vials_all, "output/preds/vials.gz")
# 
# # Close out 
# file_path <- "R/05_predictions/02_vials.R"

# out.session(path = file_path, filename = "output/log_cluster.csv")
closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


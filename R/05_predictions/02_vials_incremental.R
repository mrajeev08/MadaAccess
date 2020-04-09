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
model_ests <- read.csv("output/mods/estimates_adj_OD.csv")
model_ests %>%
  select(params, Mean, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_means
model_means <- bind_rows(filter(model_means, data_source == "National", scale == "District",
                                intercept == "random"), 
                         filter(model_means, data_source == "Moramanga"))
model_ests %>%
  select(params, sd_adj, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = sd_adj, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_sds_adj
model_ests %>%
  select(params, SD, pop_predict, intercept, data_source, scale) %>%
  spread(key = params, value = SD, fill = 0) %>%
  filter(pop_predict == "flatPop") -> model_sds_unadj
model_sds <- bind_rows(filter(model_sds_unadj, data_source == "National", scale == "District",
                              intercept == "random"), filter(model_sds_adj, 
                                                             data_source == "Moramanga"))

# Vials given commune/district ttimes -------------------------------------------------------

# make df with the lookup + scale (reverse vec so big ones at end don't slow things down)
lookup <- expand.grid(loop = rev(scenario_loop), scale = c("Commune", "District"))

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(j = iter(lookup, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach'), .options.RNG = 2687) %dorng% {
          
          mean <- model_means[model_means$scale == j$scale, ]
          sd <- model_sds[model_sds$scale == j$scale, ]
          
          # read in max and all catch
          comm <- fread(cmd = paste("grep -w ", j$loop, 
                                    " output/ttimes/commune_maxcatch.csv", 
                                    sep = ""), col.names = colnames)
          comm_all <- fread(cmd = paste("grep -w ", j$loop, 
                                        " output/ttimes/commune_maxcatch.csv", 
                                        sep = ""), col.names = colnames)
          
          if(j$scale == "Commune") {
            ttimes <- comm$ttimes_wtd/60
            OD_dist <- TRUE # not accounting for overdispersion
          } 
          
          if(j$scale == "District") {
            ttimes <- comm$ttimes_wtd_dist/60
            OD_dist <- FALSE # accounting for overdispersion
          }
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = mean$beta_ttimes, beta_ttimes_sd = sd$beta_ttimes,
                                    beta_0 = mean$beta_0, beta_0_sd = sd$beta_0, 
                                    beta_pop = 0, beta_pop_sd = sd$beta_pop, 
                                    sigma_0 = mean$sigma_0, known_alphas = NA, 
                                    pop_predict = mean$pop_predict, intercept = mean$intercept, 
                                    trans = 1e5, known_catch = FALSE, nsims = 10, type = "inc",
                                    dist = OD_dist)
          
          bites <- data.table(commcode = comm$commcode, scenario = comm$scenario,
                              bite_mat)
          
          check <- comm_all[bites, on = c("commcode", "scenario")]
          
          cols <- names(check[, .SD, .SDcols = grepl("result", names(check), fixed = TRUE)])
          
          check[, (cols) := lapply(.SD, function(x) x*prop_pop_catch), .SDcols = cols]
          bites_by_catch <- check[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, 
                                  by = c("catchment", "scenario")]
          

          
          # Get mean proportion of bites from each district
          check_prop <- comm_all[bites_by_catch, on = c("catchment", "scenario")]
          
          # make sure both check prop & check are in same order
          check_prop[, row_id := interaction(commcode, scenario, catchment)]
          check[, row_id := interaction(commcode, scenario, catchment)]
          setorder(check, row_id)
          setorder(check_prop, row_id)
          
          # take proportion
          check <- as.matrix(check[, cols, with = FALSE])
          total <- as.matrix(check_prop[, cols, with = FALSE])
          prop_bites <- check/total
          
          prop_preds <- data.table(prop_bites = rowMeans(prop_bites, na.rm = TRUE), 
                                   prop_bites_sd = apply(prop_bites, 1, sd, na.rm = TRUE),
                                   prop_bites_upper = apply(prop_bites, 1, quantile, prob = 0.975,
                                                            na.rm = TRUE),
                                   prop_bites_lower = apply(prop_bites, 1, quantile, prob = 0.025,
                                                            na.rm = TRUE),
                                   commcode = check_prop$commcode, 
                                   catchment = check_prop$catchment, 
                                   scenario = check_prop$scenario, scale = j$scale)
          
          catch_mat <- as.matrix(bites_by_catch[, .SD, .SDcols = cols])
          
          vial_preds <- sapply(catch_mat, get.vials)
          
          # vials
          vials <- matrix(unlist(vial_preds["vials", ]), nrow = nrow(catch_mat), 
                          ncol = ncol(catch_mat))
          vials_mean <- rowMeans(vials, na.rm = TRUE)
          vials_sd <- apply(vials, 1, sd, na.rm = TRUE)
          vials_upper <- apply(vials, 1, quantile, prob = 0.975, na.rm = TRUE)
          vials_lower <- apply(vials, 1, quantile, prob = 0.025, na.rm = TRUE)
          
          # throughput
          tp <- matrix(unlist(vial_preds["throughput", ]), nrow = nrow(catch_mat), 
                       ncol = ncol(catch_mat))
          tp_mean <- rowMeans(tp, na.rm = TRUE)
          tp_sd <- apply(tp, 1, sd, na.rm = TRUE)
          tp_upper <- apply(tp, 1, quantile, prob = 0.975, na.rm = TRUE)
          tp_lower <- apply(tp, 1, quantile, prob = 0.025, na.rm = TRUE)
          
          # bites
          bites_mean <- rowMeans(catch_mat, na.rm = TRUE)
          bites_sd <- apply(catch_mat, 1, sd, na.rm = TRUE)
          bites_upper <- apply(catch_mat, 1, quantile, prob = 0.975, na.rm = TRUE)
          bites_lower <- apply(catch_mat, 1, quantile, prob = 0.025, na.rm = TRUE)
          
          catch_preds <- data.table(catchment = bites_by_catch$catchment, 
                            scenario = bites_by_catch$scenario, scale = j$scale,
                            data_source = j$data_source,
                            vials_mean, vials_sd, vials_upper, vials_lower, 
                            tp_mean, tp_sd, tp_upper, 
                            tp_lower, bites_mean, bites_sd, bites_upper, bites_lower)
          list(catch_preds = catch_preds, prop_preds = prop_preds)
} -> preds

catch_preds <- preds[["catch_preds"]]
prop_preds <- preds[["prop_preds"]]
fwrite(catch_preds, "output/preds/catch_preds.gz")
fwrite(prop_preds, "output/preds/catch_props.csv")

# Close out
file_path <- "R/05_predictions/02_vials.R"
out.session(path = file_path, filename = "log_cluster.csv")

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/preds/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/preds/catch*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


# ------------------------------------------------------------------------------------------------ #
#' Estimating vials
#' Details: Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 3000 -sp "./R/05_predictions/02_vials_incremental.R" -jn vials -wt 5m -n@

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

# Vials given commune/district ttimes -------------------------------------------------------
# make df with the lookup + mod pars (reverse vec so big ones at end don't slow things down)
lookup <- expand.grid(loop = rev(scenario_loop), scale = c("Commune", "District"), 
                      pop_predict = "flatPop", intercept = "fixed", data_source = "National", 
                      OD = TRUE)

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(j = iter(lookup, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach', "glue"), .options.RNG = 2687) %dorng% {
          
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
          
          posts <- as.data.frame(get.samps(pop_predict = j$pop_predict, 
                                           data_source = j$data_source,
                                           intercept = j$intercept, 
                                           scale = j$scale, suff = ifelse(j$OD == TRUE, "_OD", ""),
                                           parent_dir = "output/mods/samps/", nsims = 1000))
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0, 
                                    beta_pop = posts$beta_pop,
                                    sigma_e = posts$sigma_e, sigma_0 = posts$sigma_0,
                                    known_alphas = NA, known_catch = FALSE,
                                    pop_predict = j$pop_predict, intercept = j$intercept, 
                                    nsims = 1000, pred_type = "exp", par_type = "posterior", 
                                    OD = j$OD)
          
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
          vials_natl <- colSums(vials, na.rm = TRUE) # for each sim
          vials_natl_mean <- mean(vials_natl)
          vials_natl_upper <- quantile(vials_natl, prob = 0.975, na.rm = TRUE)
          vials_natl_lower <- quantile(vials_natl, prob = 0.025, na.rm = TRUE)
          
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
          
          natl_preds <- data.table(scenario = bites_by_catch$scenario[1], 
                                   scale = j$scale, data_source = j$data_source, 
                                   vials_mean = vials_natl_mean, vials_upper = vials_natl_upper, 
                                   vials_lower = vials_natl_lower)
          
          list(catch_preds = catch_preds, prop_preds = prop_preds, natl_preds = natl_preds)
} -> preds

catch_preds <- preds[["catch_preds"]]
prop_preds <- preds[["prop_preds"]]
natl_preds <- preds[["natl_preds"]]
fwrite(catch_preds, "output/preds/catch_preds.gz")
fwrite(prop_preds, "output/preds/catch_props.csv")
fwrite(natl_preds, "output/preds/catch_preds_natl.csv")

# Close out
file_path <- "R/05_predictions/02_vials.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/preds/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/preds/catch*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


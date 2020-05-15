# ------------------------------------------------------------------------------------------------ #
#' Getting incremental estimates of burden  
#' Details: Pulling in district and commune estimates of travel times as clinics are added 
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 4000 -sp "./R/05_predictions/01_burden_incremental.R" -jn burden -wt 5m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# Set up ------------------------------------------------------------------------------------
library(data.table)
library(magrittr)
library(foreach)
library(doRNG)
library(iterators)
library(tidyverse)
library(triangle)
library(glue)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")
select <- dplyr::select

# Set-up these two vectors for reading in data using cmd line args
scenario_loop <- unique(fread("output/ttimes/commune_maxcatch.csv")$lookup)
colnames <- colnames(fread("output/ttimes/commune_maxcatch.csv"))

# Vials given commune/district ttimes -------------------------------------------------------
# make df with the lookup + mod pars (reverse vec so big ones at end don't slow things down)
lookup <- expand.grid(loop = rev(scenario_loop), scale = c("Commune", "District"), 
                      pop_predict = "flatPop", intercept = "fixed", data_source = "National", 
                      OD = TRUE)

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

# All predictions -----------------------------------------------------------------------
foreach(j = iter(lookup, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach', "triangle", "glue"), .options.RNG = 2677) %dorng% {
  
          # read in max and all catch
          comm <- fread(cmd = paste("grep -w ", j$loop, 
                                    " output/ttimes/commune_maxcatch.csv", 
                                    sep = ""), col.names = colnames)
          
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
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop_admin, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0, 
                                    beta_pop = posts$beta_pop,
                                    sigma_e = posts$sigma_e, sigma_0 = posts$sigma_0,
                                    known_alphas = NA, known_catch = FALSE,
                                    pop_predict = j$pop_predict, intercept = j$intercept, 
                                    nsims = 1000, pred_type = "exp", par_type = "posterior", 
                                    OD = j$OD)
          
          all_mats <-  predict.deaths(bite_mat, pop = comm$pop_admin,
                                      p_rab_min = 0.2, p_rab_max = 0.6,
                                      rho_max = 0.98, exp_min = 15/1e5, exp_max = 76/1e5,
                                      prob_death = 0.16, dist = "triangle")
          
          all_mats <- c(list(bites = bite_mat), all_mats)
          
          foreach(i = 1:length(all_mats), .combine = 'cbind') %do% {
            mat <- all_mats[[i]]
            labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
            mean <- rowMeans(mat, na.rm = TRUE) # mean for each row = admin unit
            upper <- apply(mat, 1, quantile, prob = 0.975)
            lower <- apply(mat, 1, quantile, prob = 0.025)
            out <- data.table(mean, upper, lower)
            names(out) <- labels
            out
          } -> admin_preds
          
          natl_mats <- all_mats[c("bites", "deaths", "averted")]
          
          foreach(i = 1:length(natl_mats), .combine = 'cbind') %do% {
            mat <- natl_mats[[i]]
            labels <- paste0(names(all_mats)[i], "_", c("mean", "upper", "lower"))
            totals <- colSums(mat, na.rm = TRUE) # sums at natl level
            mean <- mean(totals)
            upper <- quantile(totals, prob = 0.975)
            lower <- quantile(totals, prob = 0.025)
            out <- data.table(mean, upper, lower)
            names(out) <- labels
            out
          } -> natl_preds
          
          natl_preds <- data.table(scenario = comm$scenario[1], scale = j$scale, 
                                  data_source = j$data_source, natl_preds)
          
          admin_preds <- data.table(names = comm$commcode,
                                   ttimes = ttimes, pop = comm$pop_admin, 
                                   catch = comm$catchment, scenario = comm$scenario, 
                                   scale = j$scale, data_source = j$data_source,
                                   admin_preds)
          
          list(admin_preds = admin_preds, natl_preds = natl_preds)
          
  } -> preds_all

admin_preds <- preds_all[["admin_preds"]]
natl_preds <- preds_all[["natl_preds"]]

fwrite(admin_preds, "output/preds/burden_all.gz")
fwrite(natl_preds, "output/preds/burden_natl.gz")

burden_base <- filter(admin_preds, scenario == 0)
fwrite(burden_base, "output/preds/burden_base.gz")

# Saving session info
out.session(path = "R/05_predictions/01_burden_incremental.R", filename = "output/log_cluster.csv",
            start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/preds/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/preds/burden*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()


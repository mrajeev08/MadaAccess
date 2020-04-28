# ------------------------------------------------------------------------------------------------ #
#' Getting sensitivity to parameter assumptions for burden & impact of expanding access                          
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 4000 -sp "./R/06_sensitivity/02_burden_se.R" -jn burden_se -wt 5m -n@

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
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")
select <- dplyr::select

# Set-up these two vectors for reading in data using cmd line args
scenario_loop <- unique(fread("output/ttimes/commune_maxcatch.csv")$lookup)
colnames <- colnames(fread("output/ttimes/commune_maxcatch.csv"))

# Bring in sensitivity parameters
se_pars <- fread("output/sensitivity/se_pars.csv")

# make df with the lookup + scale
# reverse so bigger dfs don't slow everything down
lookup <- expand_grid(loop = rev(scenario_loop), se_pars)

multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

# All predictions -----------------------------------------------------------------------
foreach(j = iter(lookup, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach', "triangle"), .options.RNG = 2677) %dorng% {
          
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
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = j$beta_ttimes, beta_0 = j$beta_0, 
                                    beta_pop = j$beta_pop, sigma_e = j$sigma_e, sigma_0 = j$sigma_0,
                                    known_alphas = NA, OD = j$OD,
                                    pop_predict = j$pop_predict, intercept = j$intercept, 
                                    trans = 1e5, known_catch = FALSE, nsims = 1000,  
                                    par_type = "point_est", pred_type =  "exp")
          
          all_mats <-  predict.deaths(bite_mat, pop = comm$pop,
                                      p_rab_min = j$p_rab_min, p_rab_max = j$p_rab_max,
                                      rho_max = j$rho_max, exp_min = j$exp_min, exp_max = j$exp_max,
                                      prob_death = j$p_death, dist = "triangle")
          
          all_mats <- c(list(bites = bite_mat), all_mats)
          
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
                                   data_source = j$data_source, vary = j$vary, 
                                   direction = j$direction, natl_preds)
          
          # only save the baseline preds @ the admin level
          if (j$loop == "scenario_0") {
            
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
            
            admin_preds <- data.table(names = comm$commcode,
                                      ttimes = ttimes, pop = comm$pop, 
                                      catch = comm$catchment, scenario = comm$scenario, 
                                      scale = j$scale, data_source = j$data_source, vary = j$vary, 
                                      direction = j$direction, 
                                      admin_preds)
            
          } else {
            admin_preds <- NULL
          }
    
          list(base = admin_preds, natl = natl_preds)
          
  } -> burden_se

# Write out data
fwrite(burden_se[["base"]], "output/sensitivity/burden_baseline_se.gz") 
fwrite(burden_se[["natl"]], "output/sensitivity/burden_addclinics_se.gz") 

# Close out
file_path <- "R/06_sensitivity/03_burden_se.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/burden*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")

# ------------------------------------------------------------------------------------------------ #
#' Scaling of incidence and how this might drive burden                          
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 30 -mem 4000 -sp "./R/06_sensitivity/03_scaling_se.R" -jn scaling_se -wt 5m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# libraries
library(doRNG)
library(glue)
library(rgdal)
library(data.table)
library(tidyverse)
library(foreach)
library(iterators)
library(triangle)
source("R/functions/out.session.R")
source("R/functions/predict_functions.R")

# Scaling factors ----------------------------------------------------------------------------
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

# Communes
pop <- mada_communes$pop - min(mada_communes$pop)
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 76/1e5
incidence_min <- 15/1e5
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ 0 + pop, offset=rep(15/1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset=rep(76/1e5, length(pop)))
neg_comm <- data.table(scaling = "neg", sfactor = neg$coefficients[1], 
                       scale = "Commune", type = "---", trans = min(mada_communes$pop))
pos_comm <- data.table(scaling = "pos", sfactor = pos$coefficients[1], 
                       scale = "Commune", type = "+++", trans = min(mada_communes$pop))

# Districts
pop <- mada_districts$pop - min(mada_districts$pop)
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ 0 + pop, offset=rep(15/1e5, length(pop)))
neg <- lm(neg_scale ~ 0 + pop, offset=rep(76/1e5, length(pop)))
neg_dist <- data.table(scaling = "neg", sfactor = neg$coefficients[1], 
                       scale = "District", type = "---", trans = min(mada_districts$pop))
pos_dist <- data.table(scaling = "pos", sfactor = pos$coefficients[1], 
                       scale = "District", type = "+++", trans = min(mada_districts$pop))
scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)
write.csv(scaling_df, "output/sensitivity/scaling.csv", row.names = FALSE)

# Get model means for commune and district models
scenario_loop <- unique(fread("output/ttimes/commune_maxcatch.csv")$lookup)
colnames_max <- colnames(fread("output/ttimes/commune_maxcatch.csv"))
colnames_all <- colnames(fread("output/ttimes/commune_allcatch.csv"))

lookup <- expand_grid(loop = rev(scenario_loop), scaling_df)
lookup <- data.table(lookup, pop_predict = "flatPop", data_source = "National", intercept = "fixed", 
                     OD = TRUE)

# Burden predictions --------------------------------------------------------------------------
multicomb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}

foreach(j = iter(lookup, by = "row"), .combine = multicomb, 
        .packages = c('data.table', 'foreach', 'triangle', 'dplyr', 'glue'), .options.RNG = 2607) %dorng% {
          
          # read in max and all catch
          comm <- fread(cmd = paste("grep -w ", j$loop, 
                                    " output/ttimes/commune_maxcatch.csv", 
                                    sep = ""), col.names = colnames_max)
          comm_all <- fread(cmd = paste("grep -w ", j$loop, 
                                        " output/ttimes/commune_allcatch.csv", 
                                        sep = ""), col.names = colnames_all)
          
          if(j$scale == "Commune") {
            ttimes <- comm$ttimes_wtd/60
            comm$scale_pop <- comm$pop
          } 
          
          if(j$scale == "District") {
            ttimes <- comm$ttimes_wtd_dist/60
            comm[, scale_pop := sum(pop), by = "distcode"]
          }
          
          posts <- as.data.frame(get.samps(pop_predict = j$pop_predict, 
                                           data_source = j$data_source,
                                           intercept = j$intercept, 
                                           scale = j$scale, suff = ifelse(j$OD == TRUE, "_OD", ""),
                                           parent_dir = "output/mods/samps/", nsims = 1000))
          
          bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop, 
                                    catch = comm$catchment, names = comm$commcode, 
                                    beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0, 
                                    beta_pop = posts$beta_pop, sigma_e = posts$sigma_e, 
                                    sigma_0 = posts$sigma_0,
                                    known_alphas = NA, OD = j$OD,
                                    pop_predict = j$pop_predict, intercept = j$intercept, 
                                    trans = 1e5, known_catch = FALSE, nsims = 1000,  
                                    par_type = "posterior", pred_type =  "exp")
          
          inc_scaled <- constrained_inc(slope = j$sfactor, pop = comm$scale_pop - j$trans, 
                                        max = 76/1e5, min = 15/1e5)
          
          all_mats <-  predict.deaths(bite_mat, pop = comm$pop,
                                      p_rab_min = 0.2, p_rab_max = 0.6,
                                      rho_max = 0.98, exp_scaled = inc_scaled,
                                      prob_death = 0.16, dist = "triangle")
          
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
                                   scaling = j$scaling, data_source = j$data_source,
                                   natl_preds)
          
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
                                      scale = j$scale, 
                                      scaling = j$scaling, data_source = j$data_source,
                                      admin_preds, exp_scaled = inc_scaled)
            
          } else {
            admin_preds <- NULL
          }
          
          list(base = admin_preds, natl = natl_preds)
          
    } -> burden_scaled_se

# Write out data
fwrite(burden_scaled_se[["base"]], "output/sensitivity/burden_baseline_scaled.gz") 
fwrite(burden_scaled_se[["natl"]], "output/sensitivity/burden_addclinics_scaled.gz") 

# Close out
file_path <- "R/06_sensitivity/04_scaling_se.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/burden*"

closeCluster(cl)
mpi.quit()
print("Done remotely:)")

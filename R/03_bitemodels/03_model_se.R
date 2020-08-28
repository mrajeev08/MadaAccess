# ------------------------------------------------------------------------------------------------ #
#' Sensitivity analyses for model ests      
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 6 -sp "./R/03_bitemodels/04_model_se.R" -jn mod_se -wt 2m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# Set up
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(doParallel)
library(doRNG)
library(glue)
select <- dplyr::select

# Functions
source("R/functions/out.session.R")
source("R/functions/data_functions.R")
source("R/functions/estimate_pars.R")
source("R/functions/predict_functions.R")

# Read in data
national <- fread("data/processed/bitedata/national.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/processed/clinics/ctar_metadata.csv")
mada_communes <- st_read("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- st_read("data/processed/shapefiles/mada_districts_simple.shp")

# Prep shapefiles & metadata
ctar_metadata <- ctar_to_exclude(national, ctar_metadata, min_forms = 10)
mada_districts$exclude_dist <- ctar_metadata$exclude_dist[match(mada_districts$catchment,
                                                                ctar_metadata$CTAR)]
mada_communes %>%
  left_join(select(st_drop_geometry(mada_districts), distcode, exclude_dist, 
                   ctch_dist = catchment)) -> mada_communes

# What we want to look at is correcting data vs. not correcting data
rep_cut <- c(7, 15, Inf) # exclude all periods with 7/15 consecutive zero days or don't exclude any

# 1. bite data -----------------------------------------------------------------------------
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY = FALSE)
}

foreach(p = 1:length(rep_cut), .combine = comb, .multicombine = TRUE, 
        .packages = c('dplyr', 'tidyr', 'lubridate')) %dopar% {
          
          rep_cutoff <- rep_cut[p]
          
          # National data: district bites & commcovars
          natl <- clean_natl(national, mada_districts, mada_communes, ctar_metadata, 
                             reporting_thresh = 0.25, 
                             tput_thresh = rep_cutoff)
          district_bites <- natl$district_bites
          comm_covars <- natl$comm_covars
          
          district_bites$rep_cutoff <- comm_covars$rep_cutoff <- rep_cutoff
          
          # Output as list and then do multicombine
          list(district_bites, comm_covars)
        } -> master_data

bitedata_se <- master_data[[1]]
commcovar_se <- master_data[[2]]

write.csv(bitedata_se, "output/mods/bitedata_se.csv", row.names = FALSE)
write.csv(commcovar_se, "output/mods/commcovar_se.csv", row.names = FALSE)

# Model estimates --------------------------------------------------------------------------
mods <- expand_grid(pop_predict = "flatPop", intercept = "fixed", 
                    scale = c("District", "Commune"), data_source = "National_se",
                    rep_cutoff = c(7, 15, Inf),
                    OD = TRUE)
mods %>%
  mutate(sum_it = case_when(scale %in% "District" ~ FALSE,
                            scale %in% "Commune" ~ TRUE), 
         covars_df = case_when(scale %in% "District" ~ list(bitedata_se),
                               scale %in% "Commune" ~ list(commcovar_se)), 
         data_df = list(bitedata_se)) -> mods

seeds <- 200 + 1:nrow(mods)

# For each of those opts
mods_all <- 
  foreach(j = iter(mods, by = "row"), 
          jags_seed = iter(seeds), .combine = "rbind", .packages = c("rjags", "glue")) %dopar% {
            
            covar_df <- filter(j$covars_df[[1]], rep_cutoff == j$rep_cutoff)
            data_df <- filter(j$data_df[[1]], rep_cutoff == j$rep_cutoff)
            ttimes <- covar_df$ttimes_wtd/60
            
            inc_prior <- log(mean(data_df$avg_bites/data_df$pop))
            prior_list <- list(beta_0 = glue("beta_0 ~ dnorm({inc_prior}, 10^-3)"))
            
            out <- estimate.pars(bites = data_df$avg_bites,
                                 ttimes = ttimes, pop = covar_df$pop, 
                                 start = data_df$start, end = data_df$end,
                                 ncovars = nrow(covar_df), group = covar_df$group,
                                 nlocs = nrow(data_df), catch = covar_df$catch, 
                                 ncatches = max(data_df$catch), pop_predict = j$pop_predict, 
                                 intercept = j$intercept, summed = j$sum_it, OD = j$OD, 
                                 data_source = j$data_source,
                                 scale = j$scale, trans = 1e5, burn = 5000, 
                                 chains = 3, adapt = 2500, iter = 60000, thinning = 5,
                                 dic = TRUE, save = TRUE, centered = FALSE, 
                                 pass_priors = prior_list, seed = jags_seed, 
                                 suffix = paste0("_repcut", j$rep_cutoff))
            
            samps <- out[["samps"]]
            dic <- out[["dic"]]
            
            dic_est <- mean(dic$deviance) + mean(dic$penalty)
            
            samp_summ <- summary(samps)
            params <- rownames(samp_summ$statistics)
            diag <- gelman.diag(samps)
            hpd_90 <- HPDinterval(as.mcmc(do.call(rbind, samps)), prob = 0.9) #hpds
            
            samp_df <- data.frame(params = params, samp_summ$statistics, 
                                  neff = effectiveSize(samps), quant_2.5 = samp_summ$quantiles[, 5],
                                  quant_97.5 = samp_summ$quantiles[, 1], psrf_est = diag$psrf[, 1],
                                  hpd_lower90 = hpd_90[, 1], hpd_upper90 = hpd_90[, 2],
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf, 
                                  pop_predict = j$pop_predict, intercept = j$intercept, 
                                  summed = j$sum_it, data_source = j$data_source, OD = j$OD, 
                                  scale = j$scale, dic = dic_est, rep_cutoff = j$rep_cutoff)
          }

warnings()

write.csv(mods_all, "output/mods/estimates_se.csv", row.names = FALSE)

# Get expectations given a range of travel times and pop of 1e5 --------------------------------
# for flat pop models only
ttimes_plot <- seq(0, 15, by = 0.05)

expectations <- 
  foreach(j = iter(mods, by = "row"), .combine = rbind, .options.RNG = 1222) %dorng% {
    
    posts <- as.data.frame(get.samps(pop_predict = j$pop_predict, 
                                     data_source = j$data_source,
                                     intercept = j$intercept, 
                                     scale = j$scale, 
                                     suff = paste0("_OD_repcut", j$rep_cutoff),
                                     parent_dir = "output/mods/samps/", nsims = 10000))
    
    # at these travel times what are the expected ranges of bites predicted
    bite_mat <- predict.bites(ttimes = ttimes_plot, pop = 1e5, 
                              catch = 1, names = NA,
                              beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0, 
                              beta_pop = posts$beta_pop, sigma_0 = posts$sigma_0, 
                              sigma_e = posts$sigma_e, known_alphas = NA, 
                              pop_predict = j$pop_predict, OD = j$OD, 
                              intercept = j$intercept, trans = 1e5, known_catch = FALSE, 
                              par_type = "posterior", nsims = 10000, error_n = 1, 
                              pred_type = "bites")
    
    mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
    upper <- apply(bite_mat, 1, quantile, prob = 0.95, na.rm = TRUE)
    lower <- apply(bite_mat, 1, quantile, prob = 0.05, na.rm = TRUE)
    sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
    data.table(ttimes = ttimes_plot, pop = 1e5, mean_bites = mean, sd_bites = sd, 
               bites_upper = upper, bites_lower = lower,
               pop_predict = j$pop_predict, intercept = j$intercept, scale = j$scale, 
               data_source = j$data_source, OD = j$OD, rep_cutoff = j$rep_cutoff)
  }

write.csv(expectations, "output/mods/expectations_se.csv", row.names = FALSE)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/mods/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/mods/"

# Close out 
file_path <- "R/06_sensitivity/03_model_se.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)
closeCluster(cl)
mpi.quit()
print("Done remotely:)")

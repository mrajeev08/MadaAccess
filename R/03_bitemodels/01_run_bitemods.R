# ------------------------------------------------------------------------------------------------ #
#' Run models of bite incidence using spatial covariates
#' Details: Models include travel times in addition to population 
# ------------------------------------------------------------------------------------------------ #

#sub_cmd=-t 12 -n 18 -sp "./R/03_bitemodels/01_run_bitemods.R" -jn mods -wt 2m -n@

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
start <- Sys.time()

# libraries
library(data.table)
library(dplyr)
library(foreach)
library(iterators)
library(rjags)
library(tidyr)
library(iterators)
library(doRNG)
library(glue)

# source bite ests
district_bites <- fread("output/bites/district_bites.csv")
comm_covars <- fread("output/bites/comm_covars.csv")
mora_bites <- fread("output/bites/mora_bites.csv")
source("R/functions/estimate_pars.R")
source("R/functions/out.session.R")

# Run all Mada models -----------------------------------------------------------------------
mods <- expand_grid(pop_predict = c("addPop", "onlyPop", "flatPop"), 
                         intercept = c("random", "fixed"), 
                         scale = c("District", "Commune"), data_source = c("National", "Moramanga"), 
                    OD = c("FALSE", "TRUE"))

mods %>%
  filter(interaction(scale, data_source) != "District.Moramanga", 
         interaction(intercept, data_source) != "random.Moramanga",
         !(interaction(OD, pop_predict) %in% c("TRUE.onlyPop", "TRUE.addPop"))) %>%
  mutate(sum_it = case_when(data_source %in% "Moramanga" ~ FALSE, scale %in% "District" ~ FALSE,
                            scale %in% "Commune" ~ TRUE), 
         covars_df = case_when(data_source %in% "Moramanga" ~ list(mora_bites),
                               scale %in% "District" ~ list(district_bites),
                               scale %in% "Commune" ~ list(comm_covars)), 
         data_df = case_when(data_source %in% "Moramanga" ~ list(mora_bites), 
                             data_source %in% "National" ~ list(district_bites))) -> mods

seeds <- 100 + 1:nrow(mods)

# For each of those opts
mods_all <- 
  foreach(j = iter(mods, by = "row"), 
          jags_seed = iter(seeds), .combine = "rbind", .packages = c("rjags", "glue"), 
          .options.RNG = 321) %dorng% {
            
            covar_df <- j$covars_df[[1]]
            data_df <- j$data_df[[1]]
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
                                 pass_priors = prior_list, seed = jags_seed)
            
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
                                  scale = j$scale, dic = dic_est)
          }

warnings()

write.csv(mods_all, "output/mods/estimates.csv", row.names = FALSE)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/output/mods/"
syncfrom <- "mrajeev@della.princeton.edu:~/MadaAccess/output/mods/"

# Close out 
file_path <- "R/03_bitemodels/01_run_bitemods.R"
out.session(path = file_path, filename = "log_cluster.csv", start = start)
closeCluster(cl)
mpi.quit()
print("Done remotely:)")


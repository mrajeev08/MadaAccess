# ------------------------------------------------------------------------------
#' Run models of bite incidence using spatial covariates
#' including travel times in addition to population
# ------------------------------------------------------------------------------

# sub_cmd:=-t 12 -n 18 -jn mods -wt 2m

# Set it up
source(here::here("R", "utils.R"))
source(here_safe("R/estimate_pars.R"))
set_up <- setup_cl(mpi = TRUE)

if(!set_up$slurm) fp <- here::here else fp <- cl_safe

cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

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
district_bites <- fread(fp("analysis/out/bites/district_bites.csv"))
comm_covars <- fread(fp("analysis/out/bites/comm_covars.csv"))
mora_bites <- fread(fp("analysis/out/bites/mora_bites.csv"))


# Run all Mada models -----------------------------------------------------------------------
mods <- expand_grid(pop_predict = c("addPop", "onlyPop", "flatPop"),
                         intercept = c("random", "fixed"),
                         scale = c("District", "Commune"), data_source = c("National", "Moramanga"),
                    OD = c("FALSE", "TRUE"))

mods %>%
  filter(interaction(scale, data_source) != "District.Moramanga",
         interaction(intercept, data_source) != "random.Moramanga",
         !(interaction(OD, pop_predict) %in% c("TRUE.onlyPop", "TRUE.addPop"))) %>%
  mutate(summed = case_when(data_source %in% "Moramanga" ~ FALSE, scale %in% "District" ~ FALSE,
                            scale %in% "Commune" ~ TRUE),
         covars_df = case_when(data_source %in% "Moramanga" ~ "mora_bites",
                               scale %in% "District" ~ "district_bites",
                               scale %in% "Commune" ~ "comm_covars"),
         data_df = case_when(data_source %in% "Moramanga" ~ "mora_bites",
                             data_source %in% "National" ~ "district_bites")) -> mods

seeds <- 140 + 1:nrow(mods)

# For each of those opts
mods_all <-
  foreach(j = iter(mods, by = "row"),
          jags_seed = iter(seeds), .combine = "rbind", .packages = c("rjags", "glue"),
          .export = c("mora_bites", "district_bites", "comm_covars"),
          .options.RNG = 321) %dorng% {


            covar_df <- get(j$covars_df)
            data_df <- get(j$data_df)
            covar_df$ttimes <-covar_df$ttimes_wtd/60

            inc_prior <- log(mean(data_df$avg_bites/data_df$pop))
            prior_list <- list(beta_0 = glue("beta_0 ~ dnorm({inc_prior}, 0.1)"))

            out <- estimate_pars(data_df = data_df, covar_df = covar_df, pars = j,
                                 trans = 1e5, burn = 5000, chains = 3,
                                 adapt = 2500, iter = 60000, thinning = 5,
                                 dic = TRUE, save = TRUE, centered = FALSE,
                                 priors = prior_list, seed = jags_seed,
                                 out_dir = fp("analysis/out/mods/samps/"))

            samps <- out[["samps"]]
            dic <- out[["dic"]]

            dic_est <- mean(dic$deviance) + mean(dic$penalty)

            samp_summ <- summary(samps)
            params <- rownames(samp_summ$statistics)
            diag <- gelman.diag(samps)

            hpd_90 <- HPDinterval(as.mcmc(do.call(rbind, samps)), prob = 0.9) #hpds

            samp_df <- data.frame(params = params, samp_summ$statistics, j,
                                  neff = effectiveSize(samps), quant_2.5 = samp_summ$quantiles[, 5],
                                  quant_97.5 = samp_summ$quantiles[, 1], psrf_est = diag$psrf[, 1],
                                  hpd_lower90 = hpd_90[, 1], hpd_upper90 = hpd_90[, 2],
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf, dic = dic_est)
          }

warnings()

write_create(mods_all,
             fp("analysis/out/mods/estimates.csv"),
             write.csv, row.names = FALSE)

# Parse these from sub for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/mods"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)

print("Done:)")


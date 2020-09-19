# ------------------------------------------------------------------------------
#' Sensitivity analyses for model ests
# ------------------------------------------------------------------------------

# sub_cmd:=-t 12 -n 6 -jn mods_se -wt 2m  -md "gdal"

# Set it up
start <- Sys.time()

# set up utils
source(here::here("R", "utils.R"))
set_up <- setup_cl(mpi = TRUE)
if(!set_up$slurm) fp <- here::here else fp <- cl_safe
cl <- make_cl(set_up$ncores)
register_cl(cl)
print(paste("Cluster size:", cl_size(cl)))

# Set up
library(tidyverse)
library(data.table)
library(sf)
library(lubridate)
library(doParallel)
library(doRNG)
library(glue)
select <- dplyr::select
source(here_safe("R/estimate_pars.R"))
source(here_safe("R/data_functions.R"))
source(here_safe("R//predict_functions.R"))

# Read in data
national <- fread("data-raw/out/bitedata/national.csv")
moramanga <- fread("data-raw/out/bitedata/moramanga.csv")
ctar_metadata <- fread("data-raw/out/clinics/ctar_metadata.csv")
mada_communes <- st_read(fp("analysis/out/shapefiles/mada_communes_simple.shp"))
mada_districts <- st_read(fp("analysis/out/shapefiles/mada_districts_simple.shp"))

# Prep shapefiles & metadata
ctar_metadata <- ctar_to_exclude(national, ctar_metadata, min_forms = 10)
mada_districts$exclude_dist <- ctar_metadata$exclude_dist[match(mada_districts$catchment,
                                                                ctar_metadata$CTAR)]
mada_communes %>%
  left_join(select(st_drop_geometry(mada_districts), distcode, exclude_dist,
                   ctch_dist = catchment)) -> mada_communes

# What we want to look at is correcting data vs. not correcting data
rep_cut <- c(7, 15, Inf) # exclude all periods with 7/15 consecutive zero days or don't exclude any

# 1. bite data -----------------------------------------------------------------
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY = FALSE)
}

foreach(p = 1:length(rep_cut), .combine = comb, .multicombine = TRUE,
        .packages = c('dplyr', 'tidyr', 'lubridate', 'sf')) %dopar% {

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

write_create(bitedata_se, fp("analysis/out/mods/bitedata_se.csv"), write.csv,
             row.names = FALSE)
write_create(commcovar_se, fp("analysis/out/mods/commcovar_se.csv"), write.csv,
             row.names = FALSE)

# Model estimates --------------------------------------------------------------
mods <- expand_grid(pop_predict = "flatPop", intercept = "fixed",
                    scale = c("District", "Commune"), data_source = "National_se",
                    rep_cutoff = c(7, 15, Inf),
                    OD = TRUE)
mods %>%
  mutate(summed = case_when(scale %in% "District" ~ FALSE,
                            scale %in% "Commune" ~ TRUE),
         covars_df = case_when(scale %in% "District" ~ "bitedata_se",
                               scale %in% "Commune" ~ "commcovar_se"),
         data_df = "bitedata_se") -> mods

seeds <- 200 + 1:nrow(mods)

# For each of those opts
mods_all <-
  foreach(j = iter(mods, by = "row"),
          jags_seed = iter(seeds), .combine = "rbind",
          .packages = c("rjags", "glue", "dplyr", "foreach"),
          .export = c("bitedata_se", "commcovar_se")) %dopar% {

            covar_df <- filter(get(j$covars_df), rep_cutoff == j$rep_cutoff)
            data_df <- filter(get(j$data_df), rep_cutoff == j$rep_cutoff)
            covar_df$ttimes <- covar_df$ttimes_wtd/60

            inc_prior <- log(mean(data_df$avg_bites/data_df$pop))
            prior_list <- list(beta_0 = glue("beta_0 ~ dnorm({inc_prior}, 0.1)"))

            out <- estimate_pars(data_df = data_df,
                                 covar_df = covar_df,
                                 pars = j,
                                 data_source = j$data_source,
                                 scale = j$scale, trans = 1e5, burn = 5000,
                                 chains = 3, adapt = 2500, iter = 60000, thinning = 5,
                                 dic = TRUE, save = TRUE, centered = FALSE,
                                 pass_priors = prior_list, seed = jags_seed,
                                 out_dir = fp("analysis/out/mods/samps/"),
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
                                  j, dic = dic_est)
          }

warnings()
print("did I make it here?")
write_create(mods_all, fp("analysis/out/mods/estimates_se.csv"),
             write.csv, row.names = FALSE)

# Get expectations given a range of travel times and pop of 1e5 ----------------
# for flat pop models only
ttimes_plot <- seq(0, 15, by = 0.05)

expectations <-
  foreach(j = iter(mods, by = "row"),
          .packages = c("data.table", "glue", "dplyr", "foreach"),
          .combine = rbind, .options.RNG = 1222) %dorng% {

    posts <- as.data.frame(get_samps(pop_predict = j$pop_predict,
                                     data_source = j$data_source,
                                     intercept = j$intercept,
                                     scale = j$scale,
                                     suff = paste0("_OD_repcut", j$rep_cutoff),
                                     parent_dir = fp("analysis/out/mods/samps/"),
                                     nsims = 10000))

    # at these travel times what are the expected ranges of bites predicted
    bite_mat <- predict_bites(ttimes = ttimes_plot, pop = 1e5,
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

write_create(expectations, fp("analysis/out/mods/expectations_se.csv"),
             write.csv, row.names = FALSE)

# Parse these from bash for where to put things
syncto <- "~/Documents/Projects/MadaAccess/analysis/out/mods/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/MadaAccess/analysis/out/mods/"

# Close out
out_session(logfile = set_up$logfile, start = set_up$start, ncores = set_up$ncores)
close_cl(cl)
print("Done :)")

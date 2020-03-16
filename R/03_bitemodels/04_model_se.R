# ------------------------------------------------------------------------------------------------ #
#' Sensitivity analyses for model ests      
# ------------------------------------------------------------------------------------------------ #

# Init MPI Backend
library(doMPI)
cl <- startMPIcluster()
clusterSize(cl) # this just tells you how many you've got
registerDoMPI(cl)
Sys.time()

# Set up
library(tidyverse)
library(data.table)
library(rgdal)
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
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes_simple.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts_simple.shp")

# What we want to look at is correcting data vs. not correcting data
rep_cut <- c(7, 15, Inf) # exclude all periods with 7/15 consecutive zero days or don't exclude any

# 1. bite data -----------------------------------------------------------------------------
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY = FALSE)
}

foreach(p = 1:length(rep_cut), .combine = comb, .multicombine = TRUE, 
        .packages = c('dplyr', 'tidyr', 'lubridate')) %dopar% {
          
          rep_cutoff <- rep_cut[p]
          
          #' Getting daily throughput for each clinic
          national %>%
            filter(year(date_reported) > 2013, year(date_reported) < 2018, 
                   !is.na(distcode), !is.na(id_ctar)) %>%
            mutate(date_reported = ymd(date_reported)) %>%
            group_by(date_reported, id_ctar) %>%
            summarise(no_patients = n()) %>%
            ungroup() %>%
            complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
                     fill = list(no_patients = 0)) -> throughput
          
          #' rle.days = Helper function for getting which days to include 
          #' (moved to functions from data_functions.R)
          #' and also identify potential CAT 1 by the throughput mean/sd
          throughput %>%
            group_by(id_ctar) %>%
            arrange(date_reported, .by_group = TRUE) %>%
            mutate(include_day = rle.days(no_patients, threshold = rep_cutoff), 
                   mean_throughput = mean(no_patients[include_day == 1]),
                   sd_throughput = sd(no_patients[include_day == 1]),
                   year = year(date_reported)) -> throughput
          
          #' yearly reporting
          #' sum the total # of days included over # of days in year (365)
          throughput %>%
            group_by(year, id_ctar) %>%
            summarize(reporting = sum(include_day)/365) -> reporting
          
          #' Left join with throughput to get exclusion criteria
          national %>%
            filter(year(date_reported) > 2013, year(date_reported) < 2018,
                   distcode %in% mada_districts$distcode, 
                   id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)],
                   type == "new") %>% 
            mutate(date_reported = ymd(date_reported)) %>%
            left_join(select(throughput, date_reported, id_ctar, include_day, year)) -> bites
          
          #' Getting district level exclusion criteria
          #' if submitted less than 10 forms total
          national %>%
            filter(year(date_reported) > 2013, year(date_reported) < 2018, 
                   distcode %in% mada_districts$distcode, 
                   id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)]) %>%
            group_by(id_ctar) %>%
            summarize(total_forms = n()) %>%
            complete(id_ctar = ctar_metadata$id_ctar, fill = list(total_forms = 0)) %>%
            right_join(ctar_metadata) %>%
            mutate(total_forms = ifelse(is.na(total_forms), 0, total_forms),
                   exclude_dist = ifelse(total_forms > 10, 0, 1)) -> ctar_metadata
          
          mada_districts$exclude_dist <- ctar_metadata$exclude_dist[match(mada_districts$catchment,
                                                                          ctar_metadata$CTAR)]
          #' A check
          unique(mada_districts$catchment[mada_districts$exclude_dist == 1])
          
          #' Getting bite incidence estimates for all districts
          bites %>%
            # filter based on throughput
            filter(include_day == 1) %>% 
            group_by(year, distcode) %>%
            summarize(bites = n()) -> bites_district
          bites_district$CTAR <- mada_districts$catchment[match(bites_district$distcode, 
                                                                mada_districts$distcode)]
          bites_district$id_ctar<- ctar_metadata$id_ctar[match(bites_district$CTAR, ctar_metadata$CTAR)]
          bites_district %>%
            left_join(reporting) %>%
            filter(reporting > 0.25) %>% # dont include any district for which catchment clinic had
            # less than 25% reporting
            # correct for reporting by year and ctar reported to 
            mutate(bites = bites/reporting) %>%
            group_by(distcode) %>%
            summarize(avg_bites = mean(bites, na.rm = TRUE)) %>%
            complete(distcode = mada_districts$distcode, fill = list(avg_bites = 0)) -> bite_ests
          
          #' Join bites with district and commune covariates 
          mada_districts@data %>%
            filter(exclude_dist == 0) %>%
            mutate(catch = as.numeric(droplevels(catchment)), 
                   group = as.numeric(droplevels(distcode))) %>%
            left_join(bite_ests) %>%
            arrange(group) -> district_bites
          
          #' Communes
          mada_communes$exclude_dist <- mada_districts$exclude_dist[match(mada_communes$distcode,
                                                                          mada_districts$distcode)]
          mada_communes$ctch_dist <- mada_districts$catchment[match(mada_communes$distcode,
                                                                    mada_districts$distcode)]
          mada_communes@data %>%
            filter(exclude_dist == 0) %>%
            mutate(catch = as.numeric(droplevels(ctch_dist)),
                   group = as.numeric(droplevels(distcode))) %>%
            arrange(group) -> comm_covars
          district_bites$end <- cumsum(rle(comm_covars$group)$lengths)
          district_bites$start <- c(1, lag(district_bites$end)[-1] + 1)
          
          district_bites$rep_cutoff <- comm_covars$rep_cutoff <- rep_cutoff
          
          # Output as list and then do multicombine
          list(district_bites, comm_covars)
        } -> master_data

bitedata_se <- master_data[[1]]
commcovar_se <- master_data[[2]]

write.csv(bitedata_se, "output/sensitivity/bitedata_se.csv", row.names = FALSE)
write.csv(commcovar_se, "output/sensitivity/commcovar_se.csv", row.names = FALSE)

# Model estimates --------------------------------------------------------------------------
mods_natl <- expand_grid(pop_predict = "flatPop", 
                         intercept = c("random", "fixed"), 
                         scale = c("District", "Commune"), data_source = "National_se",
                         rep_cutoff = c(7, 15, Inf))
mods_natl %>%
  mutate(sum_it = case_when(scale %in% "District" ~ FALSE,
                            scale %in% "Commune" ~ TRUE), 
         covars_df = case_when(scale %in% "District" ~ list(bitedata_se),
                               scale %in% "Commune" ~ list(commcovar_se)), 
         data_df = list(bitedata_se)) -> mods_natl

mods_all <- 
  foreach(j = iter(mods_natl, by = "row"), .combine = "rbind", .packages = c("rjags", "dplyr"), 
          .options.RNG = 321) %dorng% {
            
            covar_df <- filter(j$covars_df[[1]], rep_cutoff == j$rep_cutoff)
            data_df <- filter(j$data_df[[1]], rep_cutoff == j$rep_cutoff)
            ttimes <- covar_df$ttimes_wtd/60
            
            out <- estimate.pars(bites = data_df$avg_bites,
                                 ttimes = ttimes, pop = covar_df$pop, 
                                 start = data_df$start, end = data_df$end,
                                 ncovars = nrow(covar_df), 
                                 nlocs = nrow(data_df), catch = covar_df$catch, 
                                 ncatches = max(data_df$catch), pop_predict = j$pop_predict, 
                                 intercept = j$intercept, summed = j$sum_it, OD = FALSE, 
                                 data_source = j$data_source,
                                 scale = j$scale, trans = 1e5, burn = 5000, 
                                 chains = 3, adapt = 2500, iter = 60000, thinning = 5,
                                 dic = TRUE, save = TRUE)
            
            samps <- out[["samps"]]
            dic <- out[["dic"]]
            
            dic_est <- mean(dic$deviance) + mean(dic$penalty)
            
            samp_summ <- summary(samps)
            params <- rownames(samp_summ$statistics)
            diag <- gelman.diag(samps)
            
            samp_df <- data.frame(params = params, samp_summ$statistics, 
                                  neff = effectiveSize(samps), quant_2.5 = samp_summ$quantiles[, 5], 
                                  quant_97.5 = samp_summ$quantiles[, 1], psrf_est = diag$psrf[, 1], 
                                  psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf, 
                                  pop_predict = j$pop_predict, intercept = j$intercept, 
                                  summed = j$sum_it, data_source = j$data_source, OD = FALSE, 
                                  scale = j$scale, dic = dic_est, rep_cutoff = j$rep_cutoff)
          }

write.csv(mods_all, "output/sensitivity/model_se.csv", row.names = FALSE)

# Get expectations ---------------------------------------------------------------------------
mods_all %>%
  select(params, Mean, pop_predict, intercept, data_source,
         scale, rep_cutoff) %>%
  spread(key = params, value = Mean, fill = 0)-> model_means
mods_all %>%
  select(params, SD, pop_predict, intercept, data_source,
         scale, rep_cutoff) %>%
  spread(key = params, value = SD, fill = 0) -> model_sds
ttimes_plot <- seq(0, 15, by = 0.05)

foreach(mean = iter(model_means, by = "row"), 
        sd = iter(model_sds, by = "row"), .packages = c("rjags", "dplyr", "data.table"),
        .combine = "bind_rows", .options.RNG = 331) %dorng% {
          
          bite_mat <- predict.bites.dist(ttimes = ttimes_plot, pop = 1e5,
                                         beta_ttimes_mean = mean$beta_ttimes, 
                                         beta_ttimes_sd = sd$beta_ttimes, 
                                         beta_0_mean = mean$beta_0, 
                                         beta_0_sd = sd$beta_0, 
                                         beta_pop_mean = 0, beta_pop_sd = 0,
                                         sigma_0 = mean$sigma_0,
                                         pop_predict = mean$pop_predict, 
                                         intercept = mean$intercept, 
                                         trans = 1e5, nsims = 1000)
          
          exp_bites <- rowMeans(bite_mat, na.rm = TRUE)
          exp_bites_upper <- apply(bite_mat, 1, quantile, prob = 0.975, na.rm = TRUE)
          exp_bites_lower <- apply(bite_mat, 1, quantile, prob = 0.025, na.rm = TRUE)
          
          out <- data.table(mean, exp_bites, exp_bites_upper, exp_bites_lower, 
                            ttimes_wtd = ttimes_plot, pop = 1e5, OD = FALSE, 
                            rep_cutoff = mean$rep_cutoff)
        } -> preds_cutoff

write.csv(preds_cutoff, "output/preds/bites/expectations_se.csv", row.names = FALSE)

# Parse these from bash for where to put things
sync_to <- "~/Documents/Projects/MadaAccess/output/sensitivity/"
sync_from <- "mrajeev@della.princeton.edu:~/MadaAccess/output/sensitivity/"

# Close out 
file_path <- "R/06_sensitivity/02_model_se.R"
out.session(path = file_path, filename = "log_cluster.csv")
closeCluster(cl)
mpi.quit()
print("Done remotely:)")
Sys.time()

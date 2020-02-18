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
source("R/functions/out.session.R")
source("R/functions/data_functions.R")
source("R/functions/estimate_pars.R")
source("R/functions/predict_functions.R")

# Read in data
national <- fread("data/processed/bitedata/national.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

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
      filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
      mutate(date_reported = ymd(date_reported)) %>%
      group_by(date_reported, id_ctar) %>%
      summarise(no_patients = n()) %>%
      ungroup() %>%
      complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
               fill = list(no_patients = 0)) -> throughput
    
    #' rle.days = Helper function for getting which days to include (moved to functions from data_functions.R)
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
foreach(p = 1:length(rep_cut), .combine = rbind, 
        .packages = c('dplyr', 'foreach', 'rjags')) %dopar% {
    
    district_bites <- filter(bitedata_se, rep_cutoff == rep_cut[p])
    comm_covars <- filter(commcovar_se, rep_cutoff == rep_cut[p])                         
    
    # Mada data
    # These all should have same index letter
    covars_list <- list(district_bites, comm_covars)
    scale <- c("District", "Commune")
    sum_it <- c(FALSE, TRUE)
    
    # The other index letters
    intercept_type <- c("random", "fixed")
    pop_predict <- c("addPop", "onlyPop", "flatPop")
    
    mods_mada <- 
      foreach(i = 1:length(covars_list), .combine = rbind) %:%
      foreach(k = 1:length(pop_predict), .combine = rbind) %:%
      foreach(l = 1:length(intercept_type), .combine = rbind) %do% {
                
                covar_df <- covars_list[[i]]
                ttimes <- covar_df$ttimes_wtd/60
                
                out <- estimate.pars(bites = round(district_bites$avg_bites), 
                                     ttimes = ttimes, pop = covar_df$pop, 
                                     start = district_bites$start, end = district_bites$end,
                                     ncovars = nrow(covar_df), 
                                     nlocs = nrow(district_bites), catch = covar_df$catch, 
                                     ncatches = max(district_bites$catch), pop_predict =  pop_predict[k], 
                                     intercept = intercept_type[l], summed = sum_it[i], 
                                     data_source = "National_se",
                                     scale = paste0(scale[i], "_rep", 
                                                    rep_cut[p]), 
                                     trans = 1e5, 
                                     chains = 3, adapt = 500, iter = 10000, thinning = 1,
                                     dic = TRUE, save = TRUE)
                
                samps <- out[["samps"]]
                dic <- out[["dic"]]
                
                dic_est <- mean(dic$deviance) + mean(dic$penalty)
                
                samp_summ <- summary(samps)
                params <- rownames(samp_summ$statistics)
                diag <- gelman.diag(samps)
                
                samp_df <- as.data.frame(list(params = params, samp_summ$statistics, 
                                              neff = effectiveSize(samps),
                                              quant_2.5 = samp_summ$quantiles[, 5], 
                                              quant_97.5 = samp_summ$quantiles[, 1],
                                              psrf_est = diag$psrf[, 1], 
                                              psrf_upper = diag$psrf[, 2], mpsrf = diag$mpsrf,  
                                              pop_predict = pop_predict[k], intercept = intercept_type[l], 
                                              summed = sum_it[i], 
                                              data_source = "National_se", 
                                              scale = scale[i], dic = dic_est, 
                                              reporting = district_bites$rep_cutoff[1]))
              }
    mods_mada
} -> master_df

write.csv(master_df, "output/sensitivity/model_se.csv", row.names = FALSE)

# 3. Run predictions ---------------------------------------------------------------------------

master_df %>%
  select(params, Mean, pop_predict, intercept, data_source,
         scale, reporting) %>%
  spread(key = params, value = Mean, fill = 0) %>%
  mutate(n = 82) -> model_means

# Predictions
foreach(i = iter(model_means, by = "row"), j = icount(), .combine = rbind, 
        .packages = c('dplyr', 'foreach', 'data.table', 'glue')) %dopar% {
            
            print(j/nrow(model_means)*100)
            
            district_bites <- filter(bitedata_se, rep_cutoff == i$reporting)
            comm_covars <- filter(commcovar_se, rep_cutoff == i$reporting)                      
            
            # Filter to covars
            if(i$scale == "District") {
              covar_df <- district_bites
              covar_df$names <- district_bites$distcode
            } else {
              covar_df <- comm_covars
              covar_df$names <- comm_covars$commcode
            }
            
            # Also transform covar
            covar_df$ttimes <- covar_df$ttimes_wtd/60
            
            if(i$intercept == "random") {
              known_alphas <-  as.numeric(i[, match(glue("alpha[{covar_df$catch}]"), colnames(i))])
            } else {
              known_alphas <- rep(NA, nrow(covar_df))
            }
            
            bite_mat <- predict.bites(ttimes = covar_df$ttimes, pop = covar_df$pop, 
                                      catch = covar_df$pop, names = covar_df$distcode,
                                      beta_ttimes = i$beta_ttimes, beta_0 = i$beta_0, 
                                      beta_pop = i$beta_pop, sigma_0 = i$sigma_0, 
                                      known_alphas = known_alphas, pop_predict = i$pop_predict, 
                                      intercept = i$intercept, trans = 1e5, known_catch = TRUE, 
                                      nsims = 1000)
            mean <- rowMeans(bite_mat, na.rm = TRUE) # mean for each row = admin unit
            sd <- apply(bite_mat, 1, sd, na.rm = TRUE)
            preds_mada <- data.table(names = covar_df$names, group_names = covar_df$distcode,
                       ttimes = covar_df$ttimes, pop = covar_df$pop, 
                       catch = covar_df$catch, mean_bites = mean, sd_bites = sd, 
                       pop_predict = i$pop_predict, intercept = i$intercept, scale = i$scale, 
                       data_source = i$data_source, reporting = i$reporting)
            
            preds_mada %>%
              group_by(group_names, data_source, scale, pop_predict, intercept, reporting) %>% 
              summarize_at(vars(contains("bites")), sum) %>%
              left_join(district_bites, by = c("group_names" = "distcode")) -> preds_mada_grouped
} -> preds_grouped

write.csv(preds_grouped, "output/sensitivity/modpreds_se.csv", row.names = FALSE)
 

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

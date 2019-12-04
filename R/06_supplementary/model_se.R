####################################################################################################
##' Sensitivity analyses for model ests
##' Details:  
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())

## Libraries
library(tidyverse)
library(data.table)
library(rgdal)
library(lubridate)
library(patchwork)
library(doParallel)
select <- dplyr::select
source("R/functions/utils.R")
source("R/functions/data_functions.R")
source("R/functions/estimate_pars.R")

## Read in data
national <- fread("data/processed/bitedata/national.csv")
moramanga <- fread("data/processed/bitedata/moramanga.csv")
ctar_metadata <- fread("data/raw/ctar_metadata.csv")
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

rep_cut <- c(Inf, 15, 5) # exclude all days with zero days or none
contact_cut <- c(1, 3, Inf) # exclude all days with greater than 1 sd above reporting vs. don't exclude contacts
cores <- detectCores() - 1
foreach(p = 1:length(rep_cut), .combine = "rbind") %:%
  foreach(m = 1:length(contact_cut), .combine = "rbind") %do% {
    
    contact_cutoff <- contact_cut[m]
    rep_cutoff <- rep_cut[p]
    
    ##' Getting daily throughput for each clinic
    national %>%
      filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
      mutate(date_reported = ymd(date_reported)) %>%
      group_by(date_reported, id_ctar) %>%
      summarise(no_patients = n()) %>%
      ungroup() %>%
      complete(date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar, 
               fill = list(no_patients = 0)) -> throughput
    
    ##' rle.days = Helper function for getting which days to include (moved to functions from data_functions.R)
    ##' and also identify potential CAT 1 by the throughput mean/sd
    throughput %>%
      group_by(id_ctar) %>%
      arrange(date_reported, .by_group = TRUE) %>%
      mutate(include_day = rle.days(no_patients, threshold = rep_cutoff), 
             mean_throughput = mean(no_patients[include_day == 1]),
             sd_throughput = sd(no_patients[include_day == 1]),
             estimated_cat1 = ifelse(no_patients >= mean_throughput + contact_cutoff*sd_throughput, 
                                     1, 0),
             year = year(date_reported)) -> throughput
    
    ##' yearly reporting
    ##' sum the total # of days included over # of days in year (365)
    throughput %>%
      group_by(year, id_ctar) %>%
      summarize(reporting = sum(include_day)/365) -> reporting
    
    ##' Left join with throughput to get exclusion criteria
    national %>%
      filter(year(date_reported) > 2013, year(date_reported) < 2018,
             distcode %in% mada_districts$distcode, 
             id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)],
             type == "new") %>% 
      mutate(date_reported = ymd(date_reported)) %>%
      left_join(select(throughput, date_reported, id_ctar, include_day, estimated_cat1, year)) -> bites
    
    ##' Getting district level exclusion criteria
    ##' if submitted less than 10 forms total
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
    ##' A check
    unique(mada_districts$catchment[mada_districts$exclude_dist == 1])
    
    ##' Getting bite incidence estimates for all districts
    bites %>%
      ## filter known contacts and estimated ones based on throughput
      filter(estimated_cat1 == 0, include_day == 1) %>% 
      group_by(year, distcode) %>%
      summarize(bites = n()) -> bites_district
    bites_district$CTAR <- mada_districts$catchment[match(bites_district$distcode, 
                                                          mada_districts$distcode)]
    bites_district$id_ctar<- ctar_metadata$id_ctar[match(bites_district$CTAR, ctar_metadata$CTAR)]
    bites_district %>%
      left_join(reporting) %>%
      filter(reporting > 0.25) %>% ## dont include any district for which catchment clinic had
      ## less than 25% reporting
      ## correct for reporting by year and ctar reported to 
      mutate(bites = bites/reporting) %>%
      group_by(distcode) %>%
      summarize(avg_bites = mean(bites, na.rm = TRUE)) %>%
      complete(distcode = mada_districts$distcode, fill = list(avg_bites = 0)) -> bite_ests
    
    ##' Join bites with district and commune covariates 
    mada_districts@data %>%
      filter(exclude_dist == 0) %>%
      mutate(catch = as.numeric(droplevels(catchment)), 
             group = as.numeric(droplevels(distcode))) %>%
      left_join(bite_ests) %>%
      arrange(group) -> district_bites
    
    ##' Communes
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
    
    ##' Mada data
    ## These all should have same index letter
    covars_list <- list(district_bites, comm_covars)
    scale <- c("District", "Commune")
    sum_it <- c(FALSE, TRUE)
    
    ## The other index letters
    intercept_type <- c("random", "fixed")
    pop_predict <- c("addPop", "onlyPop", "flatPop")
    
    # # WITH SINGLE NODE
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    
    mods_mada <- 
      foreach(i = 1:length(covars_list), .combine = "rbind") %:%
      foreach(k = 1:length(pop_predict), .combine = "rbind") %:%
      foreach(l = 1:length(intercept_type), .combine = "rbind", 
              .packages = 'rjags') %dopar% {
                
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
                                     scale = paste0(scale[i], "_contact", contact_cutoff, "_rep", 
                                                    rep_cutoff), 
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
                                              contacts = contact_cutoff, reporting = rep_cutoff))
              }
    stopCluster(cl)
    mods_mada
  } -> master_df

write.csv(master_df, "output/mods/sensitivity.csv", row.names = FALSE)

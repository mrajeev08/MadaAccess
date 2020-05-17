# ------------------------------------------------------------------------------------------------ #
#' Batch functions
#' Functions for running preds & mods across scenarios                                                                                       
# ------------------------------------------------------------------------------------------------ #

#' Run preds for set of scenarios
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

run_scenarios <- function(lookup, directory = "output/ttimes/", pred_type = c("vials", "burden"), 
                          par_type = "posterior", scaled = FALSE,
                          colnames_max, colnames_all, colnames_j, admin_to_keep, catch_keep = TRUE,
                          multicomb = function(x, ...) { mapply(rbind, x, ..., SIMPLIFY = FALSE)}, 
                          rng_seed = 23481, sims = 1000) {
  
  foreach(j = iter(lookup, by = "row"), .combine = multicomb, 
          .packages = c('data.table', 'foreach', "triangle"), .options.RNG = rng_seed) %dorng% {
            
            # read in data
            comm <- fread(cmd = paste("grep -w ", j$loop, " ", directory, "commune_maxcatch.csv", 
                                      sep = ""), col.names = colnames_max)
            
            ttimes <- ifelse(j$scale == "District", comm$ttimes_wtd_dist/60, comm$ttimes_wtd/60)
            
            # first do bite preds
            if(par_type == "posterior") {
              posts <- as.data.frame(get.samps(pop_predict = j$pop_predict, 
                                               data_source = j$data_source,
                                               intercept = j$intercept, 
                                               scale = j$scale, suff = ifelse(j$OD == TRUE, "_OD", ""),
                                               parent_dir = "output/mods/samps/", sims))
            } else {
              posts <- j
            }
                  
            bite_mat <- predict.bites(ttimes = ttimes, pop = comm$pop_admin, 
                                      catch = comm$catchment, names = comm$commcode, 
                                      beta_ttimes = posts$beta_ttimes, beta_0 = posts$beta_0, 
                                      beta_pop = posts$beta_pop,
                                      sigma_e = posts$sigma_e, sigma_0 = posts$sigma_0,
                                      known_alphas = NA, known_catch = FALSE,
                                      pop_predict = j$pop_predict, intercept = j$intercept, 
                                      sims, pred_type = "exp", par_type = par_type, 
                                      OD = j$OD)
            
            # Then burden
            if("burden" %in% pred_type) {
              
              if(scaled == TRUE) {
                
                if(j$scale == "District") {
                  comm[, scale_pop := sum(pop_admin), by = "distcode"]
                } else {
                  comm$scale_pop <- comm$pop_admin
                }
                
                inc_scaled <- constrained_inc(slope = j$sfactor, pop = comm$scale_pop - j$trans, 
                                              max = j$exp_max, min = j$exp_min)
                
              } else {
                inc_scaled <- NULL
              }
              
              all_mats <-  predict.deaths(bite_mat, pop = comm$pop_admin,
                                          p_rab_min = j$p_rab_min, p_rab_max = j$p_rab_max,
                                          rho_max = j$rho_max, exp_min = j$exp_min, exp_max = j$exp_max,
                                          prob_death = j$p_death, exp_scaled = inc_scaled,
                                          dist = "triangle")
              
              all_mats <- c(list(bites = bite_mat), all_mats)
              
              if(j$loop %in% admin_to_keep) {
                admin_preds <- summarize_mats(mats = all_mats, combine_func = 'cbind', 
                                              mean_func = function(x) rowMeans(x, na.rm = TRUE), 
                                              upper_func = function(x) apply(x, 1, quantile, prob = 0.975, na.rm = TRUE), 
                                              lower_func = function(x) apply(x, 1, quantile, prob = 0.025, na.rm = TRUE)) 
                
                admin_preds <- data.table(names = comm$commcode,
                                          ttimes = ttimes, pop = comm$pop_admin, 
                                          catch = comm$catchment, scenario = comm$scenario, 
                                          j[, colnames_j], admin_preds)
              } else {
                admin_preds <- NULL 
              }
            } else {
              all_mats <- admin_preds <- NULL 
            }
            
            # Then vials
            if("vials" %in% pred_type) {
              
              comm_all <- fread(cmd = paste("grep -w ", j$loop, " ", directory, 
                                            "commune_allcatch.csv", sep = ""), 
                                col.names = colnames_all)
              
              bites <- data.table(commcode = comm$commcode, scenario = comm$scenario,
                                  bite_mat)
              
              
              check <- comm_all[bites, on = c("commcode", "scenario")]
              cols <- names(check[, .SD, .SDcols = grepl("result", names(check), fixed = TRUE)])
              
              
              check[, (cols) := lapply(.SD, function(x) x*prop_pop_catch), .SDcols = cols]
              bites_by_catch <- check[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, 
                                      by = c("catchment", "scenario")]
              
              catch_mat <- as.matrix(bites_by_catch[, .SD, .SDcols = cols])
              
              vial_preds <- sapply(catch_mat, get.vials)
              vials <- matrix(unlist(vial_preds["vials", ]), nrow = nrow(catch_mat), 
                              ncol = ncol(catch_mat))
              tp <- matrix(unlist(vial_preds["throughput", ]), nrow = nrow(catch_mat), 
                           ncol = ncol(catch_mat))
              
              vial_mats <- list(vials = vials, tp = tp, bites = catch_mat)
              
              if(catch_keep == TRUE) {
                # Get mean proportion of bites from each district
                check_prop <- comm_all[bites_by_catch, on = c("catchment", "scenario")]
                
                # make sure both check prop & check are in same order
                check_prop[, row_id := interaction(commcode, scenario, catchment)]
                check[, row_id := interaction(commcode, scenario, catchment)]
                setorder(check, row_id)
                setorder(check_prop, row_id)
                
                # take proportion (this is by proportion of bites from each admin which goes to a clinic)
                check <- as.matrix(check[, cols, with = FALSE])
                total <- as.matrix(check_prop[, cols, with = FALSE])
                prop_bites <- check/total
                
                prop_preds <- summarize_mats(mats = list(prop_bites = prop_bites), combine_func = 'cbind', 
                                             mean_func = function(x) rowMeans(x, na.rm = TRUE), 
                                             upper_func = function(x) apply(x, 1, quantile, prob = 0.975), 
                                             lower_func = function(x) apply(x, 1, quantile, prob = 0.025)) 
                
                prop_preds <- data.table(commcode = check_prop$commcode, 
                                         catchment = check_prop$catchment, 
                                         scenario = check_prop$scenario, prop_preds, 
                                         j[, colnames_j])
                
                catch_preds <- summarize_mats(mats = vial_mats, combine_func = 'cbind', 
                                              mean_func = function(x) rowMeans(x, na.rm = TRUE), 
                                              upper_func = function(x) apply(x, 1, quantile, prob = 0.975), 
                                              lower_func = function(x) apply(x, 1, quantile, prob = 0.025)) 
                
                catch_preds <- data.table(catchment = bites_by_catch$catchment, 
                                          scenario = bites_by_catch$scenario, 
                                          j[, colnames_j], catch_preds)
              } else {
                catch_preds <- prop_preds <- NULL
              }
            
            } else {
              vials <- catch_preds <- prop_preds <- NULL
            }
            
            # Then summarize @ natl level
            natl_mats <- c(all_mats[c("bites", "deaths", "averted")], list(vials = vials))
            
            natl_preds <- summarize_mats(mats = natl_mats, combine_func = 'cbind', 
                                         mean_func = function(x) mean(colSums(x), na.rm = TRUE), 
                                         upper_func = function(x) quantile(colSums(x), prob = 0.975, na.rm = TRUE), 
                                         lower_func = function(x) quantile(colSums(x), prob = 0.025, na.rm = TRUE)) 
            
            natl_preds <- data.table(scenario = comm$scenario[1], j[, colnames_j], 
                                     natl_preds)
            
            # Some of these will be NULL but that should be ok
            list(admin_preds = admin_preds, catch_preds = catch_preds, prop_preds = prop_preds, 
                 natl_preds = natl_preds)
            
          } -> preds_all
  
  return(preds_all)
  
}

#' Helpers for processing matrices
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

summarize_mats <- function(mats, combine_func = 'cbind', 
                           mean_func = function(x) rowMeans(x, na.rm = TRUE), 
                           upper_func = function(x) apply(x, 1, quantile, prob = 0.975), 
                           lower_func = function(x) apply(x, 1, quantile, prob = 0.025)) {
  
  mats <- mats[lengths(mats) != 0]
  
  foreach(i = 1:length(mats), .combine = combine_func) %do% {
    mat <- mats[[i]]
    labels <- paste0(names(mats)[i], "_", c("mean", "upper", "lower"))
    mean <- mean_func(mat) 
    upper <- upper_func(mat)
    lower <- lower_func(mat)
    out <- data.table(mean, upper, lower)
    names(out) <- labels
    out
  } 
  
}

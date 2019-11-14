####################################################################################################
##' Post processing predictions 
##' Details: Filling in data frames by missing scenarios and admin units (or catchment depending) 
##' Author: Malavika Rajeev 
####################################################################################################

# bite_mat_dist <- predict.bites(comm_run$weighted_times/60, comm_run$pop, comm_run$base_catches, 
#                                comm_run$commune_id,
#                                params$beta_access, params$beta_0, beta_pop = 0, params$sigma_0, 
#                                known_alphas = NA, 
#                                pop_predict = "flatPop", intercept = "random",
#                                trans = 1e5, known_catch = FALSE, nsims = 1000)
# 
# bites <- data.table(bite_mat_dist, catch = comm_run$base_catches, 
#                     scenario = comm_run$scenario, commune_id = comm_run$commune_id)
# 
# bites %>%
#   complete(scenario = 0:472, commune_id) %>%
#   group_by(commune_id) %>%
#   arrange(scenario) %>%
#   fill(3:ncol(bites), .direction = "down") -> bites_filled
source("R/functions/burden_functions.R")
source("R/functions/utils.R")

mod_df %>%
  filter(pop_predict == "flatPop") %>%
  select(-bites, -covar, -pop, -names_bites, -names_covar) %>%
  mutate(covar_preds = 
           case_when(covar_name == "distance" & scale %in% c("Commune", 
                                                                         "Moramanga") ~ list(mada_communes$distance),
                                 covar_name == "ttimes" & scale %in% c("Commune", 
                                                                       "Moramanga") ~ list(mada_communes$ttms_wtd/60),
                                 covar_name == "distance" & scale == "District" ~ list(mada_districts$distance),
                                 covar_name == "ttimes" & scale == "District" ~ list(mada_districts$ttms_wtd/60)),
         names_covar_preds = case_when(scale %in% c("Commune", "Moramanga") ~ list(mada_communes$ADM3_PCODE),
                                       scale == "District" ~ list(mada_districts$distcode)),
         names_preds = case_when(scale %in% c("Commune", "Moramanga") ~ list(mada_communes$distcode),
                                 scale == "District" ~ list(mada_districts$distcode)),
         pop_preds = case_when(scale %in% c("Commune", "Moramanga") ~ list(mada_communes$pop),
                               scale == "District" ~ list(mada_districts$pop))) -> mod_df_preds
mod_df_preds <- filter(mod_df_preds, scale != "District")

preds_fixed <- foreach(i = iter(mod_df_preds, by = "row"), .combine = "rbind") %do% {
  preds <- predict.bites(names_bites = unlist(i$names_preds),
                         covar = unlist(i$covar_preds), pop = unlist(i$pop_preds), 
                         names_covar = unlist(i$names_covar_preds), sum = i$sum, 
                         pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                         beta_pop = i$beta_pop, intercept = i$intercept, trans = 1e5)
  preds$scale <- i$scale
  burden_max <- get.burden.fixed(predicted_bites = preds$predicted, pop = preds$pop, rho_max = 0.98,
                             p_rabid = 0.2, HDR = 6, dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                             prob_death = 0.16)
  colnames(burden_max) <- paste(colnames(burden_max), "max", sep = "_")
  burden_min <- get.burden.fixed(predicted_bites = preds$predicted, pop = preds$pop, rho_max = 0.98,
                                 p_rabid = 0.6, HDR = 25, dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                                 prob_death = 0.16)
  colnames(burden_min) <- paste(colnames(burden_min), "min", sep = "_")
  bind_cols(preds, burden_max, burden_min)
}

preds_fixed %>%
  group_by(names_bites, sum, pop_predict, covar_name, scale) %>%
  summarize(covar = mean(covar), pop = sum(pop), predicted = sum(predicted), 
            deaths_min = sum(deaths_min),
            deaths_max = sum(deaths_max), p_rabid_min = mean(p_rabid_adj_min), 
            p_rabid_max = mean(p_rabid_adj_max),
            averted_min = sum(averted_min), averted_max = sum(averted_max), 
            reporting_min = mean(reporting_min), reporting_max = mean(reporting_max)) %>%
  ungroup() -> check

ggplot() +
  geom_linerange(data = check, aes(x = reorder(names_bites, covar), ymin = deaths_min/pop*1e5, 
                                   ymax = deaths_max/pop*1e5, color = scale)) +
  facet_grid(scale ~ covar_name, scales = "free", 
             labeller = labeller(pop_predict = as_labeller(mod_names)), drop = TRUE) +
  expand_limits(y = 0) +
  coord_flip()

check %>%
  group_by(sum, pop_predict, covar_name, scale) %>%
  summarize(deaths_max = sum(deaths_max), deaths_min = sum(deaths_min),
            averted_min = sum(averted_min), averted_max = sum(averted_max)) -> total_preds


preds_fixed <- foreach(i = iter(mod_df_preds, by = "row"), .combine = "rbind") %do% {
  preds <- predict.bites(names_bites = unlist(i$names_preds),
                         covar = unlist(i$covar_preds), pop = unlist(i$pop_preds), 
                         names_covar = unlist(i$names_covar_preds), sum = i$sum, 
                         pop_predict = i$pop_predict, covar_name = i$covar_name, beta = i$beta, 
                         beta_pop = i$beta_pop, intercept = i$intercept, trans = 1e5)
  preds$scale <- i$scale
  burden <- get.burden(predicted_bites = preds$predicted, pop = preds$pop, p_rab_min = 0.2, p_rab_max = 0.6, rho_max = 0.98,
                       max_HDR = 25, min_HDR = 5, 
                       dog_rabies_inc = 0.01, human_exp_rate = 0.39, 
                       prob_death = 0.16, nsims = 1000)
  
  bind_cols(preds, burden)
}

preds_fixed %>%
  group_by(names_bites, sum, pop_predict, covar_name, scale) %>%
  summarize(covar = mean(covar), pop = sum(pop), predicted = sum(predicted), deaths_mean = sum(deaths_mean),
            deaths_lowerCI = sum(deaths_lowerCI), deaths_upperCI = sum(deaths_upperCI), p_rabid_mean = mean(p_rabid_mean),
            p_rabid_lowerCI = mean(p_rabid_lowerCI), p_rabid_upperCI = mean(p_rabid_upperCI), averted_mean = sum(averted_mean),
            averted_lowerCI = sum(averted_lowerCI), averted_upperCI = sum(averted_upperCI)) -> check

ggplot(data = check, aes(x = covar, y = deaths_mean/pop*1e5, color = scale)) +
  geom_point() +
  scale_color_manual(values = c("darkred", "#cc7722", "#004b49"), name = "Scale") +
  facet_grid(scale ~ covar_name, scales = "free", 
             labeller = labeller(pop_predict = as_labeller(mod_names)), drop = TRUE) +
  expand_limits(y = 0)

check %>%
  group_by(sum, pop_predict, covar_name, scale) %>%
  summarize(deaths_mean = sum(deaths_mean), deaths_upperCI = sum(deaths_upperCI), 
            deaths_lowerCI = sum(deaths_lowerCI),
            averted_lowerCI = sum(averted_lowerCI), averted_upperCI = sum(averted_upperCI), 
            averted_mean = sum(averted_mean)) -> total_preds

  
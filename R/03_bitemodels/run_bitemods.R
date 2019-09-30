####################################################################################################
##' Run models of bite incidence using spatial covariates 
##' Details: Models include travel times and distance as access metrics, in addition to population 
##' Author: Malavika Rajeev 
####################################################################################################

## libraries
library(bbmle)

## Distance model
distance.model <- function(bites, distance, beta = 1e-6, intercept = 0.1, 
                           trans = 1e5) {
  exp_bites <- exp(beta*distance + intercept)*pop
  return(-sum(dpois(round(bites), lambda = exp_bites, log = TRUE)))
}

distance_ests_mada <- mle2(distance.model, data = list(pop = bites_by_distance$pop, 
                                                 distance = bites_by_distance$distance,
                                                 bites = bites_by_distance$avg_bites),
                     start = list(beta = 1e-5, intercept = 0.1))

bites_by_distance %>%
  mutate(preds = exp(distance_ests_mada@coef["beta"]*distance + distance_ests_mada@coef["intercept"])*pop) -> check
ggplot(data = check, aes(x = avg_bites, y = preds)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

preds_mada <- data.frame(list(distance = seq(0, 500, by = 0.5), 
                           preds = exp(distance_ests_mada@coef["beta"]*seq(0, 500, by = 0.5) + 
                                         distance_ests_mada@coef["intercept"])*1e5), data = "Mada")

ggplot(data = pred_df, aes(x = distance, y = preds)) + geom_line()

distance_ests_mora <- mle2(distance.model, data = list(pop = morabites_by_distance$pop, 
                                                  distance = morabites_by_distance$distance,
                                                  bites = morabites_by_distance$avg_bites),
                      start = list(beta = 1e-5, intercept = 0.1))

morabites_by_distance %>%
  mutate(preds = exp(distance_ests_mora@coef["beta"]*distance + distance_ests_mora@coef["intercept"])*pop) -> check
ggplot(data = check, aes(x = avg_bites, y = preds)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

preds_mora <- data.frame(list(distance = seq(0, 500, by = 0.5), 
                           preds = exp(distance_ests_mora@coef["beta"]*seq(0, 500, by = 0.5) + 
                                         distance_ests_mora@coef["intercept"])*1e5), data = "Mora")
preds <- bind_rows(preds_mada, preds_mora)

ggplot(data = preds, aes(x = distance, y = preds, color = data)) + geom_line() 

## Travel times model
ttms_wtd.model <- function(bites, ttms_wtd, beta = 1e-6, intercept = 0.1, 
                           trans = 1e5) {
  exp_bites <- exp(beta*ttms_wtd + intercept)*pop
  return(-sum(dpois(round(bites), lambda = exp_bites, log = TRUE)))
}

ttms_wtd_ests_mada <- mle2(ttms_wtd.model, data = list(pop = bites_by_ttimes$pop, 
                                                       ttms_wtd = bites_by_ttimes$ttms_wtd,
                                                       bites = bites_by_ttimes$avg_bites),
                           start = list(beta = 1e-5, intercept = 0.1))

bites_by_ttimes$preds <- exp(ttms_wtd_ests_mada@coef["beta"]*ttms_wtd + ttms_wtd_ests_mada@coef["intercept"])*pop
ggplot(data = bites_by_ttimes, aes(x = avg_bites, y = preds)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

preds_mada <- data.frame(list(ttms_wtd = seq(0, 500, by = 0.5), 
                              preds = exp(ttms_wtd_ests_mada@coef["beta"]*seq(0, 500, by = 0.5) + 
                                            ttms_wtd_ests_mada@coef["intercept"])*1e5), data = "Mada")

ttms_wtd_ests_mora <- mle2(ttms_wtd.model, data = list(pop = morabites_by_ttimes$pop, 
                                                       ttms_wtd = morabites_by_ttimes$ttms_wtd,
                                                       bites = morabites_by_ttimes$avg_bites),
                           start = list(beta = 1e-5, intercept = 0.1))

morabites_by_ttimes$preds <- exp(ttms_wtd_ests_mora@coef["beta"]*ttms_wtd + ttms_wtd_ests_mora@coef["intercept"])*pop 
ggplot(data = morabites_by_ttimes, aes(x = avg_bites, y = preds)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

preds_mora <- data.frame(list(ttms_wtd = seq(0, 1000, by = 1), 
                              preds = exp(ttms_wtd_ests_mora@coef["beta"]*seq(0, 500, by = 0.5) + 
                                            ttms_wtd_ests_mora@coef["intercept"])*1e5), data = "Mora")
preds <- bind_rows(preds_mada, preds_mora)

ggplot(data = preds, aes(x = ttms_wtd, y = preds, color = data)) + geom_line() +
  geom_point(data = morabites_by_ttimes, aes(x = ttms_wtd, y = avg_bites), color = "blue") +
  geom_point(data = bites_by_ttimes, aes(x = ttms_wtd, y = avg_bites), color = "red")
  

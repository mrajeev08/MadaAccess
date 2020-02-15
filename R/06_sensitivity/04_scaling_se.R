# ------------------------------------------------------------------------------------------------ #
#' Scaling of incidence and how this might drive burden                          
# ------------------------------------------------------------------------------------------------ #


# Scaling factors ----------------------------------------------------------------------------
mada_communes <- readOGR("data/processed/shapefiles/mada_communes.shp")
mada_districts <- readOGR("data/processed/shapefiles/mada_districts.shp")

## Communes
pop <- mada_communes$pop
pop <- pop[order(pop, decreasing = FALSE)]
incidence_max <- 0.01*0.39/7
incidence_min <- 0.01*0.39/25
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ pop) ## use these and constrain
neg <- lm(neg_scale ~ pop) ## use these and constrain
neg_comm <- data.table(scaling = "neg", sfactor = neg$coefficients[2], intercept = incidence_max,
                       scale = "Commune", type = "---")
pos_comm <- data.table(scaling = "pos", sfactor = pos$coefficients[2], intercept = incidence_min,
                       scale = "Commune", type = "+++")
max_comm <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_max,
                       scale = "Commune", type = "max")
min_comm <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_min,
                       scale = "Commune", type = "min")
## Districts
pop <- mada_districts$pop
pop <- pop[order(pop, decreasing = FALSE)]
pos_scale <- seq(incidence_min, incidence_max, length.out = length(pop))
neg_scale <- seq(incidence_max, incidence_min, length.out = length(pop))
pos <- lm(pos_scale ~ pop) ## use these and constrain
neg <- lm(neg_scale ~ pop) ## use these and constrain
neg_dist <- data.table(scaling = "neg", sfactor = neg$coefficients[2], intercept = incidence_max, 
                       scale = "District", type = "---")
pos_dist <- data.table(scaling = "pos", sfactor = pos$coefficients[2], intercept = incidence_min,
                       scale = "District", type = "+++")
max_dist <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_max,
                       scale = "District", type = "max")
min_dist <- data.table(scaling = "flat", sfactor = 0, intercept = incidence_min,
                       scale = "District", type = "min")

scaling_df <- rbind(neg_comm, pos_comm, neg_dist, pos_dist)
flat_df <- rbind(max_comm, max_dist, min_comm, min_dist)
pop_plot <- seq(0, 1e6, by = 1000)
constrained_inc <- function(slope, intercept, pop, max, min){
  inc <- slope*pop + intercept
  inc[inc >= max] <- max
  inc[inc <= min] <- min
  return(inc)
}

foreach(vals = iter(scaling_df, by = "row"), .combine = rbind) %do% {
  preds <- constrained_inc(vals$sfactor, vals$intercept, pop_plot, incidence_max,
                           incidence_min)*1e5
  data.table(vals, preds, pop_plot)
} -> predicted_inc

write.csv(predicted_inc, "output/sensitivity/scaling.csv", row.names = FALSE)
incidence_df <- rbind(flat_df, scaling_df)
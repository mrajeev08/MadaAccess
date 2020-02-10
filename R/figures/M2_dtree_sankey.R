####################################################################################################
##' Figure 2 
##' Details: Sankey diagram for decision tree  
##' Author: Malavika Rajeev 
####################################################################################################

library(plotly)

plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0,1),
    y =  c(0,1)),
  orientation = "h",
  arrangement = "fixed",
  textfont = list(color = alpha("black", alpha = 0.0000000001)),
  node = list(
    label = c("E_i (rabies exposures)", "B_i (reported bites)", "R_i(reported rabies exposures)", 
              "U_i (unreported rabies exposures)", "Non-rabid reported exposures", "Deaths Averted", 
              "Rabies exposures not resulting in deaths", "Deaths due to rabies"),
    color = c("darkred", "lightblue", "purple", "darkred", "grey", "purple", "grey", "darkred"),
    x = c(0.25, 0.05, 0.45, 0.45, 0.9, 0.9, 0.9, 0.9),
    y = c(0.6, 0.2, 0.4, 0.9, 0.02, 0.15, 0.6, 0.99),
    pad = 50,
    thickness = 100,
    line = list(
      color = NA,
      width = 0
    )
  ),
  link = list(
    source = c(0, 1, 1, 0, 2, 2, 3, 3),
    target = c(2, 0, 4, 3, 6, 5, 6, 7),
    value =  c(1, 1, 0.5, 1, 0.6, 0.4, 0.4, 0.6),
    color = c("purple", "lightblue", "grey", "darkred", "grey", "purple", "grey", "darkred")
  )
)

c("pink", "red", "darkgrey", "lightblue", "grey", "blue", "grey", "red")
library(patchwork)
library(ggforce)
data <- reshape2::melt(Titanic)
data <- gather_set_data(data, 1:4)

ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

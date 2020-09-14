# ------------------------------------------------------------------------------------------------ #
#' Sankey diagram for decision tree
# ------------------------------------------------------------------------------------------------ #

start <- Sys.time()
source("R/utils.R")

# set up
library(ggplot2)
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
    color = c("#B03060", "blue", "#B03060", "darkred", "grey", "#B03060", "grey", "darkred"),
    x = c(0.35, 0.05, 0.55, 0.55, 0.9, 0.9, 0.9, 0.9),
    y = c(0.6, 0.2, 0.4, 0.9, 0.05, 0.3, 0.6, 0.95),
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
    value =  c(1, 1, 0.5, 1, 0.6, 0.4, 0.6, 0.4),
    color = c("#B03060", "#B03060", "grey", "darkred", "grey", "#B03060", "grey", "darkred")
  )
) -> plot

orca(plot, file = "figs/main/M1/sankey_nolabs.jpeg", height = 500, width = 1000)

# Close out

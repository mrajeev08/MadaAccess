####################################################################################################
##' Figure 2 
##' Details: Sankey diagram for decision tree  
##' Author: Malavika Rajeev 
####################################################################################################

library(ggplot2)
library(cowplot)

df <- data.frame(x = c(15, 42, 76), y = c(0, 2/(76 - 15), 0))
ggplot(data = df, aes(x = x, y = y)) + 
  geom_polygon(fill = "orchid", color = "NA", alpha = 1) +
  geom_vline(xintercept = 42, color = "grey", linetype = 2) +
  scale_x_continuous(breaks = df$x) +
  theme_minimal_hgrid()  +
  theme(axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Rabies exposure incidence per 100k persons", y = "")

df <- data.frame(x = c(0.2, 0.4, 0.6), y = c(0, 2/(0.6 - 0.4), 0))
ggplot(data = df, aes(x = x, y = y)) + 
  geom_polygon(fill = "maroon", color = "NA", alpha = 1) +
  geom_vline(xintercept = 0.4, color = "grey", linetype = 2) +
  scale_x_continuous(breaks = df$x) +
  theme_minimal_hgrid()  +
  theme(axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Probability of reported bite being true exposure", y = "")

df <- data.frame(x = c(0.13, 0.16, 0.20), y = c(0, 2/(0.20 - 0.13), 0))
ggplot(data = df, aes(x = x, y = y)) + 
  geom_polygon(fill = "darkred", color = "NA", alpha = 1) +
  geom_vline(xintercept = 0.16, color = "grey", linetype = 2) +
  scale_x_continuous(breaks = df$x) +
  theme_minimal_hgrid()  +
  theme(axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "Probability of death in the absence of PEP", y = "")


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
    color = c("#B03060", "lightblue", "#B03060", "darkred", "grey", "#B03060", "grey", "darkred"),
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
    color = c("#B03060", "#B03060", "grey", "darkred", "grey", "#B03060", "grey", "darkred")
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

# WES PORTER
# 6/28/2017
# RUSLE2 PROJECT

# Script Summary: Using ggplot2 to produce erosion time 
# series plots.

library(ggplot2)
library(readr)

cell83 <- read_csv("E:/Wes/Work/Rusle2/R_input/cell83.csv")

theme_update(plot.title = element_text(hjust = 0.5))

g <- ggplot(cell83, aes(Year_frac, Erosion)) + geom_point()
g <- g + ggtitle('Cell 83')
g <- g + theme(plot.title = element_text(
  size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0),
  element_text(hjust = 0.5)
))
g
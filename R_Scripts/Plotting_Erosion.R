# WES PORTER
# 6/28/2017
# RUSLE2 PROJECT

# Script Summary: Using ggplot2 to produce erosion time 
# series plots.

library(ggplot2)
library(readr)
library(ggthemes)

# Read in the cell data
cell83 <- read_csv("E:/Wes/Work/Rusle2/R_input/cell83.csv")

# Convert Scenario# into factor for color assignment purposes
cell83$Scenario <- as.factor(cell83$Scenario)
Scenario <- cell83$Scenario

# Specify the specific font family to be used in the plot
windowsFonts(Times=windowsFont("TT Arial"))

# Plot the cell data using ggplot2
g <- ggplot(cell83, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
g <- g + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
g <- g + ggtitle('Cell 83')
g <- g + labs(x = "Year", y = "Erosion [t/ac]")
g <- g + theme(plot.title = element_text(
  size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertica whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
)) 
g <- g + scale_x_continuous(
  limits = c(1982, 1994),
  breaks = c(1982, 1984, 1986, 1988, 1990, 1992, 1994)
)

g


filename <- "Cell 83"
ggsave(filename, plot = last_plot(), device = "png", width = 5, height = 4, units = "in", dpi= 300)

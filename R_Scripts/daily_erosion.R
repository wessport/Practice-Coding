# WORKING WITH RAW DAILY DATA ---------------------------------

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
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
)) 
g <- g + scale_x_continuous(
  limits = c(1982, 1994),
  breaks = c(1982, 1984, 1986, 1988, 1990, 1992, 1994)
)

g


filename <- "Cell_83.png"
ggsave(filename, plot = last_plot(), device = "png", width = 5, height = 4, units = "in", dpi= 300)


# MULTIPLOTS  ---------------------------------

# Setting up plot objects for multiplot

# Plot a particular year
selected.year <- filter(cell83,cell83$Year_frac < 1983)
# Plot 1982
p1 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p1 <- p1 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p1 <- p1 + ggtitle('1982')
p1 <- p1 + labs(x = "Year", y = "Erosion [t/ac]")
p1 <- p1 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))
p1 <- p1 + scale_x_continuous(
  limits = c(1982, 1994),
  breaks = c(1982, 1984, 1986, 1988, 1990, 1992, 1994)
)

# Plot 1983
selected.year <- filter(cell83,cell83$Year_frac < 1984 & cell83$Year_frac > 1983)
p2 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p2 <- p2 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p2 <- p2 + ggtitle('1983')
p2 <- p2 + labs(x = "Year", y = "Erosion [t/ac]")
p2 <- p2 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1984
selected.year <- filter(cell83,cell83$Year_frac < 1985 & cell83$Year_frac > 1984)
p3 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p3 <- p3 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p3 <- p3 + ggtitle('1984')
p3 <- p3 + labs(x = "Year", y = "Erosion [t/ac]")
p3 <- p3 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
)) 

# Plot 1985
selected.year <- filter(cell83,cell83$Year_frac < 1986 & cell83$Year_frac > 1985)
p4 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p4 <- p4 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p4 <- p4 + ggtitle('1985')
p4 <- p4 + labs(x = "Year", y = "Erosion [t/ac]")
p4 <- p4 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
)) 

# Plot 1986
selected.year <- filter(cell83,cell83$Year_frac < 1987 & cell83$Year_frac > 1986)
p5 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p5 <- p5 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p5 <- p5 + ggtitle('1986')
p5 <- p5 + labs(x = "Year", y = "Erosion [t/ac]")
p5 <- p5 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1987
selected.year <- filter(cell83,cell83$Year_frac < 1988 & cell83$Year_frac > 1987)
p6 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p6 <- p6 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p6 <- p6 + ggtitle('1987')
p6 <- p6 + labs(x = "Year", y = "Erosion [t/ac]")
p6 <- p6 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
)) 

# Plot 1988
selected.year <- filter(cell83,cell83$Year_frac < 1989 & cell83$Year_frac > 1988)
p7 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p7 <- p7 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p7 <- p7 + ggtitle('1988')
p7 <- p7 + labs(x = "Year", y = "Erosion [t/ac]")
p7 <- p7 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1989
selected.year <- filter(cell83,cell83$Year_frac < 1990 & cell83$Year_frac > 1989)
p8 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p8 <- p8 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p8 <- p8 + ggtitle('1989')
p8 <- p8 + labs(x = "Year", y = "Erosion [t/ac]")
p8 <- p8 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1990
selected.year <- filter(cell83,cell83$Year_frac < 1991 & cell83$Year_frac > 1990)
p9 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p9 <- p9 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p9 <- p9 + ggtitle('1990')
p9 <- p9 + labs(x = "Year", y = "Erosion [t/ac]")
p9 <- p9 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1991
selected.year <- filter(cell83,cell83$Year_frac < 1992 & cell83$Year_frac > 1991)
p10 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p10 <- p10 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p10 <- p10 + ggtitle('1991')
p10 <- p10 + labs(x = "Year", y = "Erosion [t/ac]")
p10 <- p10 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1992
selected.year <- filter(cell83,cell83$Year_frac < 1993 & cell83$Year_frac > 1992)
p11 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p11 <- p11 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p11 <- p11 + ggtitle('1992')
p11 <- p11 + labs(x = "Year", y = "Erosion [t/ac]")
p11 <- p11 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

# Plot 1993
selected.year <- filter(cell83,cell83$Year_frac < 1994 & cell83$Year_frac > 1993)
p12 <- ggplot(selected.year, aes(x = Year_frac, y = Erosion, color = Scenario, group = Scenario))
p12 <- p12 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
p12 <- p12 + ggtitle('1993')
p12 <- p12 + labs(x = "Year", y = "Erosion [t/ac]")
p12 <- p12 + theme(plot.title = element_text(
  #size = 20,
  face = "bold",
  margin = margin(10, 0, 10, 0), # Adds vertical whitespace around plot title 
  hjust = 0.5, # centers the title
  family = "Arial"# Font family
))

title <- textGrob("Cell 83", gp=gpar(fontsize=20,fontface="bold"))

mp1 <- grid.arrange(p1,p2,p3,p4, ncol=2, top=title)
mp2 <- grid.arrange(p5,p6,p7,p8, ncol=2, top=title)
mp3 <- grid.arrange(p9,p10,p11,p12, ncol=2, top=title)

# Save multiplot as png
ggsave("mp1.png", mp1, device = "png", width = 8.5, height = 5, units = "in", dpi= 300)

# WES PORTER
# 6/28/2017
# RUSLE2 PROJECT

# Script Summary: Using ggplot2 to produce erosion time 
# series plots.

library(ggplot2)
library(readr)
library(ggthemes)
library(dplyr)
library(gridExtra)
library(grid)
library(scales)
library(hydroGOF)

# Set working directory
setwd("E:/Wes/Work/Rusle2/R_input")

# Read in the raw Cell data
Cell83 <- read_csv("E:/Wes/Work/Rusle2/R_input/Cell83.csv")
Cell312 <- read_csv("E:/Wes/Work/Rusle2/R_input/Cell312.csv")
Cell522 <- read_csv("E:/Wes/Work/Rusle2/R_input/Cell522.csv")
Cell552 <- read_csv("E:/Wes/Work/Rusle2/R_input/Cell552.csv")
Cell1091 <- read_csv("E:/Wes/Work/Rusle2/R_input/Cell1091.csv")

# Read in aggregated Cell data
annual <- read_csv("E:/Wes/Work/Rusle2/R_input/aggregated_results_annual.csv")
annual$Date <- as.Date(annual$Date, '%m/%d/%Y', origin="12-31-1984")
annual$Scenario <- as.factor(annual$Scenario)

quarterly <- read_csv("E:/Wes/Work/Rusle2/R_input/aggregated_results_quarterly.csv")
quarterly$Date <- as.Date(quarterly$Date, '%m/%d/%Y')
quarterly$Scenario <- as.factor(quarterly$Scenario)

monthly <- read_csv("E:/Wes/Work/Rusle2/R_input/aggregated_results_monthly.csv")
monthly$Date <- as.Date(monthly$Date, '%m/%d/%Y')
monthly$Scenario <- as.factor(monthly$Scenario)

weekly<- read_csv("E:/Wes/Work/Rusle2/R_input/aggregated_results_weekly.csv")
weekly$Date <- as.Date(weekly$Date, '%m/%d/%Y')
weekly$Scenario <- as.factor(weekly$Scenario)


# WORKING WITH AGGREGATED DATA ---------------------------------

erosionPlotAvg <- function(cellData){
  
  # Color generator for ggplot2
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  
  # Dynamically generate default color values, but have 6 ="black".
  adj_names = sort(setdiff(unique(cellData$Scenario),"6"))
  values = gg_color_hue(length(adj_names))
  names(values) = adj_names
  values = c(values, c("6"="black"))

  if (identical(cellData,annual)| identical(cellData,quarterly)){
    
    # y-axis digit transformation function - 2 decimal places
    scaleFUN <- function(x) sprintf("%.2f", x)
    
    # Cell 83
    selected.year <- filter(cellData, cellData$Cell == 83)
    p83 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p83 <- p83 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
    p83 <- p83 + scale_colour_manual(values=values)
    p83 <- p83 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p83 <- p83 + labs(y = "Cell 83 [tn/ac]")
    p83 <- p83 + theme(
      legend.position = c(0.9, 0.7),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    ) + guides(color=guide_legend(ncol=2))
    p83 <- p83 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 312
    selected.year <- filter(cellData, cellData$Cell == 312)
    p312 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p312 <- p312 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
    p312 <- p312 + scale_colour_manual(values=values)
    p312 <- p312 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p312 <- p312 + labs(y = "Cell 312 [tn/ac]")
    p312 <- p312 + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    )
    p312 <- p312 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 522
    selected.year <- filter(cellData, cellData$Cell == 522)
    p522 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p522 <- p522 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
    p522 <- p522 + scale_colour_manual(values=values)
    p522 <- p522 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p522 <- p522 + labs(y = "Cell 522 [tn/ac]")
    p522 <- p522 + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    )
    p522 <- p522 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 552
    selected.year <- filter(cellData, cellData$Cell == 552)
    p552 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p552 <- p552 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
    p552 <- p552 + scale_colour_manual(values=values)
    p552 <- p552 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p552 <- p552 + labs(y = "Cell 552 [tn/ac]")
    p552 <- p552 + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    )
    p552 <- p552 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 1091
    selected.year <- filter(cellData, cellData$Cell == 1091)
    p1091 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p1091 <- p1091 + geom_point(aes(shape=Scenario)) + geom_line() + theme_few()
    p1091 <- p1091 + scale_colour_manual(values=values)
    p1091 <- p1091 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p1091 <- p1091 + labs(x = "Year", y = "Cell 1091 [tn/ac]")
    p1091 <- p1091 + theme(
      legend.position = "none",
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
    p1091 <- p1091 + scale_y_continuous(labels=scaleFUN)
    
    if (identical(cellData,annual)){
      title_txt <- "Annual Average Erosion"
    } else if (identical(cellData,quarterly)){
      title_txt <- "Quarterly Average Erosion"
    }
    
    title <- textGrob(title_txt, gp=gpar(fontsize=20,fontface="bold"))
    
    mp1 <- grid.arrange(p83,p312,p522,p552,p1091, ncol=1, top=title)
  }
  
  if (identical(cellData,monthly)| identical(cellData,weekly)){
    
    # y-axis digit transformation function - 2 decimal places
    scaleFUN <- function(x) sprintf("%.2f", x)
    
    # Cell 83
    selected.year <- filter(cellData, cellData$Cell == 83)
    p83 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p83 <- p83 + geom_line(size=0.1) + theme_few()
    p83 <- p83 + scale_colour_manual(values=values)
    p83 <- p83 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p83 <- p83 + labs(y = "Cell 83 [tn/ac]")
    p83 <- p83 + theme(
      legend.position = c(0.9, 0.7),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    ) + guides(color=guide_legend(ncol=2))
    p83 <- p83 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 312
    selected.year <- filter(cellData, cellData$Cell == 312)
    p312 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p312 <- p312 + geom_line() + theme_few()
    p312 <- p312 + scale_colour_manual(values=values)
    p312 <- p312 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p312 <- p312 + labs(y = "Cell 312 [tn/ac]")
    p312 <- p312 + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    )
    p312 <- p312 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 522
    selected.year <- filter(cellData, cellData$Cell == 522)
    p522 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p522 <- p522 + geom_line() + theme_few()
    p522 <- p522 + scale_colour_manual(values=values)
    p522 <- p522 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p522 <- p522 + labs(y = "Cell 522 [tn/ac]")
    p522 <- p522 + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    )
    p522 <- p522 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 552
    selected.year <- filter(cellData, cellData$Cell == 552)
    p552 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p552 <- p552 + geom_line() + theme_few()
    p552 <- p552 + scale_colour_manual(values=values)
    p552 <- p552 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p552 <- p552 + labs(y = "Cell 552 [tn/ac]")
    p552 <- p552 + theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face = "bold")
    )
    p552 <- p552 + scale_y_continuous(labels=scaleFUN)
    
    # Cell 1091
    selected.year <- filter(cellData, cellData$Cell == 1091)
    p1091 <- ggplot(selected.year, aes(Date,Average, color = Scenario, group = Scenario)) 
    p1091 <- p1091 + geom_line() + theme_few()
    p1091 <- p1091 + scale_colour_manual(values=values)
    p1091 <- p1091 + scale_x_date(
      breaks = date_breaks('year'),
      labels = date_format('%Y')
    )
    p1091 <- p1091 + labs(x = "Year", y = "Cell 1091 [tn/ac]")
    p1091 <- p1091 + theme(
      legend.position = "none",
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
    p1091 <- p1091 + scale_y_continuous(labels=scaleFUN)
  
    if (identical(cellData,monthly)){
      title_txt <- "Monthly Average Erosion"
    }else if (identical(cellData,weekly)){
      title_txt <- "Weekly Average Erosion"
    }
    
    title <- textGrob(title_txt, gp=gpar(fontsize=20,fontface="bold"))
    
    mp1 <- grid.arrange(p83,p312,p522,p552,p1091, ncol=1, top=title)
  }
}

erosionPlotAvg(annual)

# STATISTICS ---------------------------------

results <- data.frame(
  Cell = character(),
  Scenario = character(),
  Correl = character(),
  Coeff_Deter = character(),
  Nash_Sutcliffe = character(),
  PBIAS = character(),
  RSR = character(),
  stringsAsFactors = FALSE
) 

cells <- c(83,312,522,552,1091)


# Correlation coefficients
count <- 1
for (i in 1:5){
  cell <- filter(annual, annual$Cell == cells[i])
  for (j in 1:5){
    cellScn <- filter(cell, cell$Scenario == j)
    cellScn6 <- filter(cell, cell$Scenario == 6)
    results[count,1] <- cells[i]
    results[count,2] <- j
    results[count,3] <- cor(cellScn6[[4]],cellScn[[4]])
    results[count,4] <- (cor(cellScn6[[4]],cellScn[[4]]))^2
    results[count,5] <- NSE(cellScn[[4]],cellScn6[[4]])
    results[count,6] <- sum(cellScn6[[4]]-cellScn[[4]])*100/sum(cellScn6[[4]])
    results[count,7] <- sqrt(sum((cellScn6[[4]]-cellScn[[4]])^2))/sqrt(sum((cellScn6[[4]]-mean(cellScn[[4]]))^2))
    count = count +1
    }
}

# Write results to a csv file. 
write.csv(results,"statistics.csv",row.names = F,col.names = T)

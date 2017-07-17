# WES PORTER
# 7/12/2017
# RUSLE2 PROJECT - Plotting Erosion & Precip

library(dplyr)
library(ggplot2)
library(ggthemes)

setwd("E:/Wes/Work/Rusle2/R_input/July_12_2017")

precip_ero <- read.table("precip_ero.csv", header=F, 
                        sep =",")

colnames(precip_ero) <- c('Date_Frac','Precip','Erosion')

par(mar = c(5,5,2,5))
with(precip_ero, plot(Date_Frac,Erosion, type="l", col="red",
                      ylab="Erosion (M g)"))
# WES PORTER
# 7/24/2017
# USDA PROJECT - Discharge unit comparison.

# Aggregating USGS stream gage data and simulation data by month. 

library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(lubridate)
library(hydroGOF)

# Workspace
ws = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Units_Investigation_JULY_24_2017/R_input"

# Read in discharge data from USGS
obs <- read.csv(paste(ws,"/observed_discharge.csv",sep=''))
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

sim_ni <- read.csv(paste(ws,"/Sim_VY_NI_SI.csv",sep=''))
sim_ni$Date <- as.Date(sim_ni$Date, '%m/%d/%Y')
sim_vi <- read.csv(paste(ws,"/Sim_VY_vI_SI.csv",sep=''))
sim_vi$Date <- as.Date(sim_vi$Date, '%m/%d/%Y')

# Take a date-time object and round it down to the nearest integer value of the specified time unit - month.
# Sum discharge by month-year

obs_month <- obs
obs_month$Date <- floor_date(obs_month$Date, "month")
obs_bymonth <- aggregate(Discharge~Date, data=obs_month, FUN=sum)

sim_ni_month <- sim_ni
sim_ni_month$Date <- floor_date(sim_ni_month$Date, "month")
sim_ni_bymonth <- aggregate(Discharge~Date, data=sim_ni_month, FUN=sum)

sim_vi_month <- sim_vi
sim_vi_month$Date <- floor_date(sim_vi_month$Date, "month")
sim_vi_bymonth <- aggregate(Discharge~Date, data=sim_vi_month, FUN=sum)

# Write results to a csv file.
output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Units_Investigation_JULY_24_2017/R_output/"

write.csv(obs_bymonth,paste(output_loc,"obs_bymonth.csv"),row.names = F,col.names = T)
write.csv(sim_ni_bymonth,paste(output_loc,"sim_ni_bymonth.csv"),row.names = F,col.names = T)
write.csv(sim_vi_bymonth,paste(output_loc,"sim_vi_bymonth.csv"),row.names = F,col.names = T)

# Plot results
obs_bymonth[[3]] <- 'obs'
colnames(obs_bymonth)[[3]] <- 'ID'
sim_ni_bymonth[[3]] <- 'sim_ni'
colnames(sim_ni_bymonth)[[3]] <- 'ID'
sim_vi_bymonth[[3]] <- 'sim_vi'
colnames(sim_vi_bymonth)[[3]] <- 'ID'

results <- bind_rows(obs_bymonth,sim_ni_bymonth,sim_vi_bymonth)

p <- ggplot(results, aes(Date,Discharge, color=ID))
p <- p + geom_line(size=1) + theme_few()
p <- p + scale_x_date(
  breaks = date_breaks('year'),
  labels = date_format('%Y')
)
p

# STATS # ----------------------

nashSut_simNI <- NSE(obs_bymonth[[2]], sim_ni_bymonth[[2]])
nashSut_simVI <- NSE(obs_bymonth[[2]], sim_vi_bymonth[[2]])

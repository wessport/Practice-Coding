# WES PORTER
# 8/07/2017
# USDA PROJECT - Discharge-analysis

# Script Summary: Working with dygraphs to produce interactive javascript discharge plots.

library(dygraphs)
library(dplyr)
library(htmltools)
library(lubridate)
library(RColorBrewer)
library(scales)
library(xts)
library(zoo)

# Workspace
ws = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Units_Investigation_JULY_24_2017/R_input"

# Read in discharge data 
obs <- read.csv(paste(ws,"/observed_discharge.csv",sep=''),stringsAsFactors = FALSE)
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

sim_ni <- read.csv(paste(ws,"/Sim_VY_NI_SI.csv",sep=''))
sim_ni$Date <- as.Date(sim_ni$Date, '%m/%d/%Y')
sim_vi <- read.csv(paste(ws,"/Sim_VY_vI_SI.csv",sep=''))
sim_vi$Date <- as.Date(sim_vi$Date, '%m/%d/%Y')

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

# Create combined dataframe
discharge <- cbind(obs,select(sim_ni,Discharge),select(sim_vi,Discharge))
colnames(discharge) <- c('Date','obsDis','simviDis','simniDis')

# Convert to extensible time series object
discharge_xts <- xts(select(discharge,obsDis,simniDis,simviDis),order.by=discharge$Date)

# Create dygraph of Obs Vs Sim NI and Sim VI
d <- dygraph(discharge_xts, main = 'Observed Vs Simulated Discharge for Mississippi Watershed') %>% # Create a dygraph plot
        dyRangeSelector() %>% # Add a range selector
        dySeries("obsDis", label = "Observed") %>%
        dySeries("simniDis", label = "Sim No Irrigation") %>%
        dySeries("simviDis", label = "Sim Variable Yield Irrigation") %>%
        dyAxis('y', label = ' Discharge (m^3)') %>%
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)
d

# Multiple dygraphs in same viewer window

dis_simni <- xts(select(discharge,obsDis,simniDis),order.by=discharge$Date)

dis_simvi <- xts(select(discharge,obsDis,simviDis),order.by=discharge$Date)


dy_simni <- dygraph(dis_simni, main = 'Simulated No Irrigation', group='ensync',height = 450, width = "100%") %>%
              dyRangeSelector() %>%
              dySeries("obsDis", label = "Observed") %>%  
              dySeries("simniDis", label = "Sim No Irrigation") %>%
              dyAxis('y', label = ' Discharge (m^3)') %>%
              dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)  
  
dy_simvi <- dygraph(dis_simvi, main = 'Simulated Variable Yield Irrigation', group='ensync',height = 450, width = "100%") %>%
              dyRangeSelector() %>%
              dySeries("obsDis", label = "Observed") %>%  
              dySeries("simviDis", label = "Sim Variable Yield Irrigation") %>%
              dyAxis('y', label = ' Discharge (m^3)') %>%
              dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

multi_dy <- dy_simni %>% tagList(dy_simvi) %>% browsable()
multi_dy


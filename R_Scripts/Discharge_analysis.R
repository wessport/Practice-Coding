# WES PORTER
# 8/07/2017
# USDA PROJECT - Discharge-analysis

# Script Summary: Working with dygraphs to produce interactive javascript discharge plots.

library(dygraphs)
library(dplyr)
library(htmltools)
library(hydroGOF)
library(lubridate)
library(RColorBrewer)
library(scales)
library(xts)
library(zoo)

# Workspace
ws = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_input"

# Read in USGS observed total streamflow
obs <- read.csv(paste(ws,"/observed_discharge.csv",sep=''),stringsAsFactors = FALSE)
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

# Read in current simulated total streamflow for our 3 scenarios

sim_cy_ni <- read.csv(paste(ws,"/Sim_CY_NI.csv",sep='')) 
sim_cy_ni$Date <- as.Date(sim_cy_ni$Date, '%m/%d/%Y')

sim_vy_ni <- read.csv(paste(ws,"/Sim_VY_NI_SI.csv",sep=''))
sim_vy_ni$Date <- as.Date(sim_vy_ni$Date, '%m/%d/%Y')

sim_vy_vi <- read.csv(paste(ws,"/Sim_VY_vI_SI.csv",sep=''))
sim_vy_vi$Date <- as.Date(sim_vy_vi$Date, '%m/%d/%Y')

# Read in prior simulated discharge data 07/22/2017

old_sim_vy_ni <- read.csv(paste(ws,"/Sim_VY_NI_SI_old.csv",sep=''))
old_sim_vy_ni$Date <- as.Date(old_sim_vy_ni$Date, '%m/%d/%Y')

old_sim_vy_vi <- read.csv(paste(ws,"/Sim_VY_vI_SI_old.csv",sep=''))
old_sim_vy_vi$Date <- as.Date(old_sim_vy_vi$Date, '%m/%d/%Y')

# Sum discharge by month-year ---------------------------------------------------------------------
obs_bymonth <- obs
obs_bymonth$Date <- floor_date(obs_bymonth$Date, "month")
obs_bymonth <- aggregate(Discharge~Date, data=obs_bymonth, FUN=sum)

sim_cy_ni_bymonth <- sim_cy_ni
sim_cy_ni_bymonth$Date <- floor_date(sim_cy_ni_bymonth$Date, "month")
sim_cy_ni_bymonth <- aggregate(Discharge~Date, data=sim_cy_ni_bymonth, FUN=sum)

sim_vy_ni_bymonth <- sim_vy_ni
sim_vy_ni_bymonth$Date <- floor_date(sim_vy_ni_bymonth$Date, "month")
sim_vy_ni_bymonth <- aggregate(Discharge~Date, data=sim_vy_ni_bymonth, FUN=sum)

sim_vy_vi_bymonth <- sim_vy_vi
sim_vy_vi_bymonth$Date <- floor_date(sim_vy_vi_bymonth$Date, "month")
sim_vy_vi_bymonth <- aggregate(Discharge~Date, data=sim_vy_vi_bymonth, FUN=sum)

old_sim_vy_ni_bymonth <- old_sim_vy_ni
old_sim_vy_ni_bymonth$Date <- floor_date(old_sim_vy_ni_bymonth$Date, "month")
old_sim_vy_ni_bymonth <- aggregate(Discharge~Date, data=old_sim_vy_ni_bymonth, FUN=sum)

old_sim_vy_vi_bymonth <- old_sim_vy_vi
old_sim_vy_vi_bymonth$Date <- floor_date(old_sim_vy_vi_bymonth$Date, "month")
old_sim_vy_vi_bymonth <- aggregate(Discharge~Date, data=old_sim_vy_vi_bymonth, FUN=sum)

output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_output/"

write.csv(obs_bymonth,paste(output_loc,"obs_bymonth.csv"),row.names = F,col.names = T)
write.csv(sim_vy_ni_bymonth,paste(output_loc,"sim_vy_ni_bymonth.csv"),row.names = F,col.names = T)
write.csv(sim_vy_vi_bymonth,paste(output_loc,"sim_vy_vi_bymonth.csv"),row.names = F,col.names = T)
write.csv(old_sim_vy_ni_bymonth,paste(output_loc,"old_sim_vy_ni_bymonth.csv"),row.names = F,col.names = T)
write.csv(old_sim_vy_vi_bymonth,paste(output_loc,"old_sim_vy_vi_bymonth.csv"),row.names = F,col.names = T)

# Sum discharge annually
obs_byyear <- obs
obs_byyear$Date <- floor_date(obs_byyear$Date, "year")
obs_byyear <- aggregate(Discharge~Date, data=obs_byyear, FUN=sum)

sim_cy_ni_byyear <- sim_cy_ni
sim_cy_ni_byyear$Date <- floor_date(sim_cy_ni_byyear$Date, "year")
sim_cy_ni_byyear <- aggregate(Discharge~Date, data=sim_cy_ni_byyear, FUN=sum)

sim_vy_ni_byyear <- sim_vy_ni
sim_vy_ni_byyear$Date <- floor_date(sim_vy_ni_byyear$Date, "year")
sim_vy_ni_byyear <- aggregate(Discharge~Date, data=sim_vy_ni_byyear, FUN=sum)

sim_vy_vi_byyear <- sim_vy_vi
sim_vy_vi_byyear$Date <- floor_date(sim_vy_vi_byyear$Date, "year")
sim_vy_vi_byyear <- aggregate(Discharge~Date, data=sim_vy_vi_byyear, FUN=sum)

old_sim_vy_ni_byyear <- old_sim_vy_ni
old_sim_vy_ni_byyear$Date <- floor_date(old_sim_vy_ni_byyear$Date, "year")
old_sim_vy_ni_byyear <- aggregate(Discharge~Date, data=old_sim_vy_ni_byyear, FUN=sum)

old_sim_vy_vi_byyear <- old_sim_vy_vi
old_sim_vy_vi_byyear$Date <- floor_date(old_sim_vy_vi_byyear$Date, "year")
old_sim_vy_vi_byyear <- aggregate(Discharge~Date, data=old_sim_vy_vi_byyear, FUN=sum)

# Create combined dataframe of 8/07/2017 streamflow data
discharge_daily_new <- cbind(obs,select(sim_cy_ni,Discharge),select(sim_vy_ni,Discharge),select(sim_vy_vi,Discharge))
colnames(discharge_daily_new) <- c('Date','obsDis','simcyniDis','simvyniDis','simvyviDis')

discharge_daily_old <- cbind(obs,select(old_sim_vy_ni,Discharge),select(old_sim_vy_vi,Discharge))
colnames(discharge_daily_old) <- c('Date','obsDis','simvyniDis','simvyviDis')

# Multiple dygraphs in same viewer window ----------------------------------------------------------------------

dis_daily_new <- xts(select(discharge_daily_new,obsDis,simcyniDis,simvyniDis,simvyviDis),order.by=discharge_daily_new$Date)

dis_daily_old <- xts(select(discharge_daily_old,obsDis,simvyniDis,simvyviDis),order.by=discharge_daily_old$Date)


dy_dis_daily_new <- dygraph(dis_daily_new, main = 'Observed and Simulated Daily Total Streamflow - August 8 2017', group='ensync',height = 450, width = "100%") %>%
              dyRangeSelector() %>%
              dySeries("obsDis", label = "Observed") %>%  
              dySeries("simcyniDis", label = "Sim Constant Yield No Irrigation") %>%
              dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
              dySeries("simvyviDis", label = "Sim Variable Yield Varaible Irrigation") %>%
              dyAxis('y', label = ' Discharge (m^3)') %>%
              dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)  
  
dy_dis_daily_old <- dygraph(dis_daily_old, main = 'Observed and Simulated Daily Total Streamflow - July 22 2017', group='ensync',height = 450, width = "100%") %>%
              dyRangeSelector() %>%
              dySeries("obsDis", label = "Observed") %>%  
              dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
              dySeries("simvyviDis", label = "Sim Variable Yield Varaible Irrigation") %>%
              dyAxis('y', label = ' Discharge (m^3)') %>%
              dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

multi_dy_daily <- dy_dis_daily_new %>% tagList(dy_dis_daily_old) %>% browsable()
multi_dy_daily


# Monthly Dygraph - Sim Constant Yield No Irrigation
discharge_mo_new <- cbind(obs_bymonth,select(sim_cy_ni_bymonth,Discharge),select(sim_vy_ni_bymonth,Discharge),select(sim_vy_vi_bymonth,Discharge))
colnames(discharge_mo_new) <- c('Date','obsDis','simcyniDis','simvyniDis','simvyviDis')

discharge_mo_old <- cbind(obs_bymonth,select(old_sim_vy_ni_bymonth,Discharge),select(old_sim_vy_vi_bymonth,Discharge))
colnames(discharge_mo_old) <- c('Date','obsDis','simvyniDis','simvyviDis')

dis_mo_new <- xts(select(discharge_mo_new,obsDis,simvyniDis),order.by=discharge_mo_new$Date)

dis_mo_old <- xts(select(discharge_mo_old,obsDis,simvyniDis),order.by=discharge_mo_old$Date)

dy_dis_mo_new <- dygraph(dis_mo_new, main = 'Observed and Simulated Monthly Total Streamflow - August 8 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

dy_dis_mo_old <- dygraph(dis_mo_old, main = 'Observed and Simulated Monthly Total Streamflow - July 22 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

multi_dy_monthly <- dy_dis_mo_new %>% tagList(dy_dis_mo_old) %>% browsable()
multi_dy_monthly

# STATS ------------------------------------------------------------------------------------------------------

# Daily
ns_d_simCYNI <- NSE(obs[[2]], sim_cy_ni[[2]])
ns_d_simVYNI <- NSE(obs[[2]], sim_vy_ni[[2]])
ns_d_simVYVI <- NSE(obs[[2]], sim_vy_vi[[2]])
ns_d_simVYNI_old <- NSE(obs[[2]], old_sim_vy_ni[[2]])
ns_d_simVYVI_old <- NSE(obs[[2]], old_sim_vy_vi[[2]])

daily_stats <- rbind(ns_d_simCYNI,ns_d_simVYNI,ns_d_simVYVI,ns_d_simVYNI_old,ns_d_simVYVI_old)

# Monthly
ns_mo_simCYNI <- NSE(obs_bymonth[[2]], sim_cy_ni_bymonth[[2]])
ns_mo_simVYNI <- NSE(obs_bymonth[[2]], sim_vy_ni_bymonth[[2]])
ns_mo_simVYVI <- NSE(obs_bymonth[[2]], sim_vy_vi_bymonth[[2]])
ns_mo_simVYNI_old <- NSE(obs_bymonth[[2]], old_sim_vy_ni_bymonth[[2]])
ns_mo_simVYVI_old <- NSE(obs_bymonth[[2]], old_sim_vy_vi_bymonth[[2]])

monthly_stats <- rbind(ns_mo_simCYNI,ns_mo_simVYNI,ns_mo_simVYVI,ns_mo_simVYNI_old,ns_mo_simVYVI_old)

# Annually
ns_an_simCYNI <- NSE(obs_byyear[[2]], sim_cy_ni_byyear[[2]])
ns_an_simVYNI <- NSE(obs_byyear[[2]], sim_vy_ni_byyear[[2]])
ns_an_simVYVI <- NSE(obs_byyear[[2]], sim_vy_vi_byyear[[2]])
ns_an_simVYNI_old <- NSE(obs_byyear[[2]], old_sim_vy_ni_byyear[[2]])
ns_an_simVYVI_old <- NSE(obs_byyear[[2]], old_sim_vy_vi_byyear[[2]])

annual_stats <- rbind(ns_an_simCYNI,ns_an_simVYNI,ns_an_simVYVI,ns_an_simVYNI_old,ns_an_simVYVI_old)

# Combine stats into table

stats <- data.frame(matrix(ncol = 4, nrow=15))
colnames(stats) <- c('Run_Date','Breakdown','Simulation_Type','Nash_Sutcliffe')

stats[1:3,1] <- '08-07-2017'
stats[4:5,1] <- '07-22-2017'
stats[6:10,1] <- stats[1:5,1]
stats[11:15,1] <- stats[1:5,1]

stats[1:5,2] <- 'Daily'
stats[6:10,2] <- 'Monthly'
stats[11:15,2] <- 'Annual'

stats[1,3] <- 'Constant Yield No Irrigation'
stats[2,3] <- 'Variable Yield No Irrigation'
stats[3,3] <- 'Variable Yield Variable Irrigation'
stats[4,3] <- 'Variable Yield No Irrigation'
stats[5,3] <- 'Variable Yield Variable Irrigation'
stats[6:10,3] <- stats[1:5,3]
stats[11:15,3] <- stats[1:5,3]

stats[1:5,4] <- daily_stats
stats[6:10,4] <- monthly_stats
stats[11:15,4] <- annual_stats

# write results to csv
output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_output/"

write.csv(stats,paste(output_loc,"MS_Simulation_Statistics.csv"),row.names = F,col.names = T)



# WES PORTER
# 8/07/2017
# USDA PROJECT - Reach -analysis

# Script Summary: Analyzing reach #967 which passes through USGS stream gage station in Clarksdale, MS.
# Investigating if simulated streamflow is closer to observed at top of watershed vs lower portion. 

library(dygraphs)
library(dplyr)
library(htmltools)
library(hydrostats)
library(hydroGOF)
library(lubridate)
library(RColorBrewer)
library(scales)
library(xts)
library(zoo)

# Workspace
ws = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Reach967_Investigation_AUGUST_10_2017/R_input"

# Read in USGS observed total streamflow
obs <- read.csv(paste(ws,"/Obs_Dis_Clarksdale.csv",sep=''),stringsAsFactors = FALSE)
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

# Remove baseflow
obs_bf_removed <- select(obs,Date,Discharge)
colnames(obs_bf_removed) <- c('Date','Q')
obs_bf_removed <- baseflows(obs_bf_removed, ts='daily')
obs_bf_removed[[5]] <- obs_bf_removed[[2]] - obs_bf_removed[[3]]
colnames(obs_bf_removed)[5] <- 'Q_bf_removed'
obs_bf_removed <- select(obs_bf_removed,Date,Q_bf_removed)
colnames(obs_bf_removed) <- c('Date','Discharge')

# Read in reach data

sim_vy_ni_967Down <- read.csv(paste(ws,"/sim_vy_ni_967_Down.csv",sep='')) 
sim_vy_ni_967Down$Date <- as.Date(sim_vy_ni_967Down$Date, '%m/%d/%Y')

sim_vy_ni_967Up <- read.csv(paste(ws,"/sim_vy_ni_967_Up.csv",sep='')) 
sim_vy_ni_967Up$Date <- as.Date(sim_vy_ni_967Up$Date, '%m/%d/%Y')

sim_vy_i5_CN20_967Down <- read.csv(paste(ws,"/Sim_VY_I5_CN20_967_Down.csv",sep='')) 
sim_vy_i5_CN20_967Down$Date <- as.Date(sim_vy_i5_CN20_967Down$Date, '%m/%d/%Y')

# Sum discharge by month-year ---------------------------------------------------------------------

obs_bf_removed_bymonth <- obs_bf_removed
obs_bf_removed_bymonth$Date <- floor_date(obs_bf_removed_bymonth$Date, "month")
obs_bf_removed_bymonth <- aggregate(Discharge~Date, data=obs_bf_removed_bymonth, FUN=sum)

sim_vy_ni_967Down_bymonth <- sim_vy_ni_967Down
sim_vy_ni_967Down_bymonth$Date <- floor_date(sim_vy_ni_967Down_bymonth$Date, "month")
sim_vy_ni_967Down_bymonth <- aggregate(Discharge~Date, data=sim_vy_ni_967Down_bymonth, FUN=sum)

sim_vy_ni_967Up_bymonth <- sim_vy_ni_967Up
sim_vy_ni_967Up_bymonth$Date <- floor_date(sim_vy_ni_967Up_bymonth$Date, "month")
sim_vy_ni_967Up_bymonth <- aggregate(Discharge~Date, data=sim_vy_ni_967Up_bymonth, FUN=sum)

sim_vy_i5_CN20_967Down_bymonth <- sim_vy_i5_CN20_967Down
sim_vy_i5_CN20_967Down_bymonth$Date <- floor_date(sim_vy_i5_CN20_967Down_bymonth$Date, "month")
sim_vy_i5_CN20_967Down_bymonth <- aggregate(Discharge~Date, data=sim_vy_i5_CN20_967Down_bymonth, FUN=sum)

# Daily dygraphs ----------------------------------------------------------------------------------

discharge_daily_r967 <- cbind(obs_bf_removed,select(sim_vy_ni_967Down,Discharge),select(sim_vy_ni_967Up,Discharge),
                              select(sim_vy_i5_CN20_967Down,Discharge))
colnames(discharge_daily_r967) <- c('Date','obsDis','simvyni967D_Dis','simvyni967U_Dis','simvyi5cn20967D_Dis')

dis_daily_r967 <- xts(select(discharge_daily_r967,obsDis,simvyni967D_Dis,simvyni967U_Dis,simvyi5cn20967D_Dis),order.by=discharge_daily_r967$Date)

dy_dis_daily_r967 <- dygraph(dis_daily_r967, main = 'Observed (Baseflow Removed) and Simulated Daily Reach 967 Streamflow - August 10 2017', group='ensync',height = 450, width = "100%") %>%
                        dyRangeSelector() %>%
                        dySeries("obsDis", label = "Observed") %>%  
                        dySeries("simvyni967D_Dis", label = "Sim VYNI Reach967 Down") %>%
                        dySeries("simvyni967U_Dis", label = "Sim VYNI Reach967 Up") %>%
                        dySeries("simvyi5cn20967D_Dis", label = "Sim VY I50% CN20 Reach967 Down") %>%
                        dyAxis('y', label = ' Discharge (m^3)') %>%
                        dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)
dy_dis_daily_r967

# Daily Stats -------------------------------------------------------------------------------------

# Daily

ns_d_simVYNI967Down <- NSE(obs_bf_removed[[2]], sim_vy_ni_967Down[[2]])
ns_d_simVYNI967Up <- NSE(obs_bf_removed[[2]], sim_vy_ni_967Up[[2]])
ns_d_simVYI5CN20967Down <- NSE(obs_bf_removed[[2]], sim_vy_i5_CN20_967Down[[2]])

# Combine stats into table

stats <- data.frame(matrix(ncol = 5, nrow=3))
colnames(stats) <- c('Run_Date','Breakdown','Simulation_Type','Sim_Code','Nash_Sutcliffe')

stats[1:2,1] <- '08-07-2017'
stats[3,1] <- '08-10-2017'
stats[1:3,2] <- 'Daily'
stats[1,3] <- 'Sim Variable Yield No Irrigation Reach967 Down'
stats[2,3] <- 'Sim Variable Yield No Irrigation Reach967 Up'
stats[3,3] <- 'Sim Variable Yield Irrigation 50% Curve Numbers 20% Lower Reach967 Down'
stats[1,4] <- 'VYNI 967 Down'
stats[2,4] <- 'VYNI 967 Up'
stats[3,4] <- 'VYI5CN20 967 Down'
stats[1,5] <- ns_d_simVYNI967Down
stats[2,5] <- ns_d_simVYNI967Up
stats[3,5] <- ns_d_simVYI5CN20967Down

# write results to csv
output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Reach967_Investigation_AUGUST_10_2017/R_output/"

write.csv(stats,paste(output_loc,"MS_Simulation_Reach967_Statistics_BF_Removed_Aug9.csv"),row.names = F,col.names = T)

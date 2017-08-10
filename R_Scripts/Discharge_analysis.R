# WES PORTER
# 8/07/2017
# USDA PROJECT - Discharge-analysis

# Script Summary: Working with dygraphs to produce interactive javascript discharge plots.

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
ws = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_input"

# Read in USGS observed total streamflow
obs <- read.csv(paste(ws,"/observed_discharge.csv",sep=''),stringsAsFactors = FALSE)
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

# Read in August 8 2017 scenarios with adjusted irrigation and curve numbers

sim_vy_i5 <- read.csv(paste(ws,"/Sim_VY_I5.csv",sep='')) 
sim_vy_i5$Date <- as.Date(sim_vy_i5$Date, '%m/%d/%Y')

sim_vy_i5_cn20 <- read.csv(paste(ws,"/Sim_VY_I5_CN20.csv",sep='')) 
sim_vy_i5_cn20$Date <- as.Date(sim_vy_i5_cn20$Date, '%m/%d/%Y')

sim_vy_ni_cn20 <- read.csv(paste(ws,"/Sim_VY_NI_CN20.csv",sep='')) 
sim_vy_ni_cn20$Date <- as.Date(sim_vy_ni_cn20$Date, '%m/%d/%Y')

# Read in simulated total streamflow for our 3 scenarios August 3 2017

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

# Remove baseflow from Observed

obs_bf_removed <- select(obs,Date,Discharge)
colnames(obs_bf_removed) <- c('Date','Q')
obs_bf_removed <- baseflows(obs_bf_removed, ts='daily')
obs_bf_removed[[5]] <- obs_bf_removed[[2]] - obs_bf_removed[[3]]
colnames(obs_bf_removed)[5] <- 'Q_bf_removed'
obs_bf_removed <- select(obs_bf_removed,Date,Q_bf_removed)
colnames(obs_bf_removed) <- c('Date','Discharge')

# Sum discharge by month-year ---------------------------------------------------------------------
obs_bymonth <- obs
obs_bymonth$Date <- floor_date(obs_bymonth$Date, "month")
obs_bymonth <- aggregate(Discharge~Date, data=obs_bymonth, FUN=sum)

obs_bf_removed_bymonth <- obs_bf_removed
obs_bf_removed_bymonth$Date <- floor_date(obs_bf_removed_bymonth$Date, "month")
obs_bf_removed_bymonth <- aggregate(Discharge~Date, data=obs_bf_removed_bymonth, FUN=sum)

sim_vy_i5_bymonth <- sim_vy_i5
sim_vy_i5_bymonth$Date <- floor_date(sim_vy_i5_bymonth$Date, "month")
sim_vy_i5_bymonth <- aggregate(Discharge~Date, data=sim_vy_i5_bymonth, FUN=sum)

sim_vy_i5_cn20_bymonth <- sim_vy_i5_cn20
sim_vy_i5_cn20_bymonth$Date <- floor_date(sim_vy_i5_cn20_bymonth$Date, "month")
sim_vy_i5_cn20_bymonth <- aggregate(Discharge~Date, data=sim_vy_i5_cn20_bymonth, FUN=sum)

sim_vy_ni_cn20_bymonth <- sim_vy_ni_cn20
sim_vy_ni_cn20_bymonth$Date <- floor_date(sim_vy_ni_cn20_bymonth$Date, "month")
sim_vy_ni_cn20_bymonth <- aggregate(Discharge~Date, data=sim_vy_ni_cn20_bymonth, FUN=sum)

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

obs_bf_removed_byyear <- obs_bf_removed
obs_bf_removed_byyear$Date <- floor_date(obs_bf_removed_byyear$Date, "year")
obs_bf_removed_byyear <- aggregate(Discharge~Date, data=obs_bf_removed_byyear, FUN=sum)

sim_vy_i5_byyear <- sim_vy_i5
sim_vy_i5_byyear$Date <- floor_date(sim_vy_i5_byyear$Date, "year")
sim_vy_i5_byyear <- aggregate(Discharge~Date, data=sim_vy_i5_byyear, FUN=sum)

sim_vy_i5_cn20_byyear <- sim_vy_i5_cn20
sim_vy_i5_cn20_byyear$Date <- floor_date(sim_vy_i5_cn20_byyear$Date, "year")
sim_vy_i5_cn20_byyear <- aggregate(Discharge~Date, data=sim_vy_i5_cn20_byyear, FUN=sum)

sim_vy_ni_cn20_byyear <- sim_vy_ni_cn20
sim_vy_ni_cn20_byyear$Date <- floor_date(sim_vy_ni_cn20_byyear$Date, "year")
sim_vy_ni_cn20_byyear <- aggregate(Discharge~Date, data=sim_vy_ni_cn20_byyear, FUN=sum)

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

# Multiple dygraphs in same viewer window ----------------------------------------------------------------------

# Create combined dataframe of streamflow data
# discharge_daily_new <- cbind(obs,select(sim_cy_ni,Discharge),select(sim_vy_ni,Discharge),select(sim_vy_vi,Discharge))
# colnames(discharge_daily_new) <- c('Date','obsDis','simcyniDis','simvyniDis','simvyviDis')
# 
# discharge_daily_old <- cbind(obs,select(old_sim_vy_ni,Discharge),select(old_sim_vy_vi,Discharge))
# colnames(discharge_daily_old) <- c('Date','obsDis','simvyniDis','simvyviDis')
# 
# dis_daily_new <- xts(select(discharge_daily_new,obsDis,simcyniDis,simvyniDis,simvyviDis),order.by=discharge_daily_new$Date)
# 
# dis_daily_old <- xts(select(discharge_daily_old,obsDis,simvyniDis,simvyviDis),order.by=discharge_daily_old$Date)
# 
# 
# dy_dis_daily_new <- dygraph(dis_daily_new, main = 'Observed and Simulated Daily Total Streamflow - August 8 2017', group='ensync',height = 450, width = "100%") %>%
#               dyRangeSelector() %>%
#               dySeries("obsDis", label = "Observed") %>%  
#               dySeries("simcyniDis", label = "Sim Constant Yield No Irrigation") %>%
#               dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
#               dySeries("simvyviDis", label = "Sim Variable Yield Varaible Irrigation") %>%
#               dyAxis('y', label = ' Discharge (m^3)') %>%
#               dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)  
#   
# dy_dis_daily_old <- dygraph(dis_daily_old, main = 'Observed and Simulated Daily Total Streamflow - July 22 2017', group='ensync',height = 450, width = "100%") %>%
#               dyRangeSelector() %>%
#               dySeries("obsDis", label = "Observed") %>%  
#               dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
#               dySeries("simvyviDis", label = "Sim Variable Yield Varaible Irrigation") %>%
#               dyAxis('y', label = ' Discharge (m^3)') %>%
#               dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)
# 
# multi_dy_daily <- dy_dis_daily_new %>% tagList(dy_dis_daily_old) %>% browsable()
# multi_dy_daily

# BASFELOW REMOVED
discharge_daily_new <- cbind(obs_bf_removed,select(sim_cy_ni,Discharge),select(sim_vy_ni,Discharge),select(sim_vy_vi,Discharge))
colnames(discharge_daily_new) <- c('Date','obsDis','simcyniDis','simvyniDis','simvyviDis')

discharge_daily_old <- cbind(obs_bf_removed,select(old_sim_vy_ni,Discharge),select(old_sim_vy_vi,Discharge))
colnames(discharge_daily_old) <- c('Date','obsDis','simvyniDis','simvyviDis')

dis_daily_new <- xts(select(discharge_daily_new,obsDis,simcyniDis,simvyniDis,simvyviDis),order.by=discharge_daily_new$Date)

dis_daily_old <- xts(select(discharge_daily_old,obsDis,simvyniDis,simvyviDis),order.by=discharge_daily_old$Date)


dy_dis_daily_new <- dygraph(dis_daily_new, main = 'Observed (Baseflow Removed) and Simulated Daily Total Streamflow - August 8 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simcyniDis", label = "Sim Constant Yield No Irrigation") %>%
            dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
            dySeries("simvyviDis", label = "Sim Variable Yield Varaible Irrigation") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)  
 
dy_dis_daily_old <- dygraph(dis_daily_old, main = 'Observed (Baseflow Removed) and Simulated Daily Total Streamflow - July 22 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
            dySeries("simvyviDis", label = "Sim Variable Yield Varaible Irrigation") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

multi_dy_daily_bf_removed <- dy_dis_daily_new %>% tagList(dy_dis_daily_old) %>% browsable()
multi_dy_daily_bf_removed

# Monthly Dygraph - Sim Constant Yield No Irrigation

# discharge_mo_new <- cbind(select(obs_bymonth,Date,Discharge),select(sim_cy_ni_bymonth,Discharge),
#                           select(sim_vy_ni_bymonth,Discharge),select(sim_vy_vi_bymonth,Discharge))
# colnames(discharge_mo_new) <- c('Date','obsDis','simcyniDis','simvyniDis','simvyviDis')
# 
# discharge_mo_old <- cbind(select(obs_bymonth,Date,Discharge),select(old_sim_vy_ni_bymonth,Discharge),
#                           select(old_sim_vy_vi_bymonth,Discharge))
# colnames(discharge_mo_old) <- c('Date','obsDis','simvyniDis','simvyviDis')
# 
# dis_mo_new <- xts(select(discharge_mo_new,obsDis,simvyniDis),order.by=discharge_mo_new$Date)
# 
# dis_mo_old <- xts(select(discharge_mo_old,obsDis,simvyniDis),order.by=discharge_mo_old$Date)
# 
# 
# 
# dy_dis_mo_new <- dygraph(dis_mo_new, main = 'Observed and Simulated Monthly Total Streamflow - August 8 2017', group='ensync',height = 450, width = "100%") %>%
#             dyRangeSelector() %>%
#             dySeries("obsDis", label = "Observed") %>%  
#             dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
#             dyAxis('y', label = ' Discharge (m^3)') %>%
#             dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)
# 
# dy_dis_mo_old <- dygraph(dis_mo_old, main = 'Observed and Simulated Monthly Total Streamflow - July 22 2017', group='ensync',height = 450, width = "100%") %>%
#             dyRangeSelector() %>%
#             dySeries("obsDis", label = "Observed") %>%  
#             dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
#             dyAxis('y', label = ' Discharge (m^3)') %>%
#             dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)
# 
# multi_dy_monthly_aug8 <- dy_dis_mo_new %>% tagList(dy_dis_mo_old) %>% browsable()
# multi_dy_monthly_aug8

# BASEFLOW REMOVED - Monthly

discharge_mo_aug9_bf_removed <- cbind(select(obs_bf_removed_bymonth,Date,Discharge),select(sim_vy_i5_bymonth,Discharge),
                           select(sim_vy_i5_cn20_bymonth,Discharge),select(sim_vy_ni_cn20_bymonth,Discharge))
colnames(discharge_mo_aug9_bf_removed) <- c('Date','obsDis','simvyi5Dis','simvyi5cn20Dis','simvynicn20Dis')

discharge_mo_new_bf_removed <- cbind(select(obs_bf_removed_bymonth,Date,Discharge),select(sim_cy_ni_bymonth,Discharge),
                          select(sim_vy_ni_bymonth,Discharge),select(sim_vy_vi_bymonth,Discharge))
colnames(discharge_mo_new_bf_removed) <- c('Date','obsDis','simcyniDis','simvyniDis','simvyviDis')

discharge_mo_old_bf_removed <- cbind(select(obs_bf_removed_bymonth,Date,Discharge),select(old_sim_vy_ni_bymonth,Discharge),
                          select(old_sim_vy_vi_bymonth,Discharge))
colnames(discharge_mo_old_bf_removed) <- c('Date','obsDis','simvyniDis','simvyviDis')

dis_mo_aug9 <- xts(select(discharge_mo_aug9_bf_removed,obsDis,simvyi5Dis,simvyi5cn20Dis,simvynicn20Dis),order.by=discharge_mo_aug9_bf_removed$Date)

dis_mo_new <- xts(select(discharge_mo_new_bf_removed,obsDis,simvyniDis),order.by=discharge_mo_new$Date)

dis_mo_old <- xts(select(discharge_mo_old_bf_removed,obsDis,simvyniDis),order.by=discharge_mo_old$Date)

dy_dis_mo_aug9 <- dygraph(dis_mo_aug9, main = 'Observed and Simulated Monthly Total Streamflow - August 9 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simvyi5Dis", label = "Sim VY Irrigation 50%") %>%
            dySeries("simvyi5cn20Dis", label = "Sim VY IR50% CN Lower 20%") %>%
            dySeries("simvynicn20Dis", label = "Sim VY NI CN Lower 20%") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)

dy_dis_mo_new <- dygraph(dis_mo_new, main = 'Observed (Baseflow Removed) and Simulated Monthly Total Streamflow - August 8 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

dy_dis_mo_old <- dygraph(dis_mo_old, main = 'Observed (Baseflow Removed) and Simulated Monthly Total Streamflow - July 22 2017', group='ensync',height = 450, width = "100%") %>%
            dyRangeSelector() %>%
            dySeries("obsDis", label = "Observed") %>%  
            dySeries("simvyniDis", label = "Sim Variable Yield No Irrigation") %>%
            dyAxis('y', label = ' Discharge (m^3)') %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

multi_dy_monthly_bf_removed_aug8 <- dy_dis_mo_new %>% tagList(dy_dis_mo_old) %>% browsable()
multi_dy_monthly_bf_removed_aug8


multi_dy_monthly_bf_removed_aug9 <- dy_dis_mo_aug9 %>% tagList(dy_dis_mo_new) %>% browsable()
multi_dy_monthly_bf_removed_aug9

# STATS ------------------------------------------------------------------------------------------------------

# # Daily
# ns_d_simCYNI <- NSE(obs[[2]], sim_cy_ni[[2]])
# ns_d_simVYNI <- NSE(obs[[2]], sim_vy_ni[[2]])
# ns_d_simVYVI <- NSE(obs[[2]], sim_vy_vi[[2]])
# ns_d_simVYNI_old <- NSE(obs[[2]], old_sim_vy_ni[[2]])
# ns_d_simVYVI_old <- NSE(obs[[2]], old_sim_vy_vi[[2]])
# 
# daily_stats <- rbind(ns_d_simCYNI,ns_d_simVYNI,ns_d_simVYVI,ns_d_simVYNI_old,ns_d_simVYVI_old)
# 
# # Monthly
# ns_mo_simCYNI <- NSE(obs_bymonth[[2]], sim_cy_ni_bymonth[[2]])
# ns_mo_simVYNI <- NSE(obs_bymonth[[2]], sim_vy_ni_bymonth[[2]])
# ns_mo_simVYVI <- NSE(obs_bymonth[[2]], sim_vy_vi_bymonth[[2]])
# ns_mo_simVYNI_old <- NSE(obs_bymonth[[2]], old_sim_vy_ni_bymonth[[2]])
# ns_mo_simVYVI_old <- NSE(obs_bymonth[[2]], old_sim_vy_vi_bymonth[[2]])
# 
# monthly_stats <- rbind(ns_mo_simCYNI,ns_mo_simVYNI,ns_mo_simVYVI,ns_mo_simVYNI_old,ns_mo_simVYVI_old)
# 
# # Annually
# ns_an_simCYNI <- NSE(obs_byyear[[2]], sim_cy_ni_byyear[[2]])
# ns_an_simVYNI <- NSE(obs_byyear[[2]], sim_vy_ni_byyear[[2]])
# ns_an_simVYVI <- NSE(obs_byyear[[2]], sim_vy_vi_byyear[[2]])
# ns_an_simVYNI_old <- NSE(obs_byyear[[2]], old_sim_vy_ni_byyear[[2]])
# ns_an_simVYVI_old <- NSE(obs_byyear[[2]], old_sim_vy_vi_byyear[[2]])
# 
# annual_stats <- rbind(ns_an_simCYNI,ns_an_simVYNI,ns_an_simVYVI,ns_an_simVYNI_old,ns_an_simVYVI_old)
# 
# # Combine stats into table
# 
# stats <- data.frame(matrix(ncol = 5, nrow=15))
# colnames(stats) <- c('Run_Date','Breakdown','Simulation_Type','Sim_Code','Nash_Sutcliffe')
# 
# stats[1:3,1] <- '08-07-2017'
# stats[4:5,1] <- '07-22-2017'
# stats[6:10,1] <- stats[1:5,1]
# stats[11:15,1] <- stats[1:5,1]
# 
# stats[1:5,2] <- 'Daily'
# stats[6:10,2] <- 'Monthly'
# stats[11:15,2] <- 'Annual'
# 
# stats[1,3] <- 'Constant Yield No Irrigation'
# stats[2,3] <- 'Variable Yield No Irrigation'
# stats[3,3] <- 'Variable Yield Variable Irrigation'
# stats[4,3] <- 'Variable Yield No Irrigation'
# stats[5,3] <- 'Variable Yield Variable Irrigation'
# stats[6:10,3] <- stats[1:5,3]
# stats[11:15,3] <- stats[1:5,3]
# 
# stats[1,4] <- 'CYNI'
# stats[2,4] <- 'VYNI'
# stats[3,4] <- 'VYVI'
# stats[4,4] <- 'VYNI'
# stats[5,4] <- 'VYVI'
# stats[6:10,4] <- stats[1:5,4]
# stats[11:15,4] <- stats[1:5,4]
# 
# 
# stats[1:5,5] <- daily_stats
# stats[6:10,5] <- monthly_stats
# stats[11:15,5] <- annual_stats
# 
# # write results to csv
# output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_output/"
# 
# write.csv(stats,paste(output_loc,"MS_Simulation_Statistics.csv"),row.names = F,col.names = T)

#BASEFLOW REMOVED stat calculations

# Daily
ns_d_simVYI5 <- NSE(obs_bf_removed[[2]], sim_vy_i5[[2]])
ns_d_simVYI5CN20 <- NSE(obs_bf_removed[[2]], sim_vy_i5_cn20[[2]])
ns_d_simVYNICN20 <- NSE(obs_bf_removed[[2]], sim_vy_ni_cn20[[2]])
ns_d_simCYNI <- NSE(obs_bf_removed[[2]], sim_cy_ni[[2]])
ns_d_simVYNI <- NSE(obs_bf_removed[[2]], sim_vy_ni[[2]])
ns_d_simVYVI <- NSE(obs_bf_removed[[2]], sim_vy_vi[[2]])
ns_d_simVYNI_old <- NSE(obs_bf_removed[[2]], old_sim_vy_ni[[2]])
ns_d_simVYVI_old <- NSE(obs_bf_removed[[2]], old_sim_vy_vi[[2]])

daily_stats_bf_removed <- rbind(ns_d_simVYI5,ns_d_simVYI5CN20,ns_d_simVYNICN20,ns_d_simCYNI,
                                ns_d_simVYNI,ns_d_simVYVI,ns_d_simVYNI_old,ns_d_simVYVI_old)

# Monthly
ns_mo_simVYI5 <- NSE(obs_bf_removed_bymonth[[2]], sim_vy_i5_bymonth[[2]])
ns_mo_simVYI5CN20 <- NSE(obs_bf_removed_bymonth[[2]], sim_vy_i5_cn20_bymonth[[2]])
ns_mo_simVYNICN20 <- NSE(obs_bf_removed_bymonth[[2]], sim_vy_ni_cn20_bymonth[[2]])
ns_mo_simCYNI <- NSE(obs_bf_removed_bymonth[[2]], sim_cy_ni_bymonth[[2]])
ns_mo_simVYNI <- NSE(obs_bf_removed_bymonth[[2]], sim_vy_ni_bymonth[[2]])
ns_mo_simVYVI <- NSE(obs_bf_removed_bymonth[[2]], sim_vy_vi_bymonth[[2]])
ns_mo_simVYNI_old <- NSE(obs_bf_removed_bymonth[[2]], old_sim_vy_ni_bymonth[[2]])
ns_mo_simVYVI_old <- NSE(obs_bf_removed_bymonth[[2]], old_sim_vy_vi_bymonth[[2]])

monthly_stats_bf_removed <- rbind(ns_mo_simVYI5,ns_mo_simVYI5CN20,ns_mo_simVYNICN20,ns_mo_simCYNI,
                                  ns_mo_simVYNI,ns_mo_simVYVI,ns_mo_simVYNI_old,ns_mo_simVYVI_old)

# Annually
ns_an_simVYI5 <- NSE(obs_bf_removed_byyear[[2]], sim_vy_i5_byyear[[2]])
ns_an_simVYI5CN20 <- NSE(obs_bf_removed_byyear[[2]], sim_vy_i5_cn20_byyear[[2]])
ns_an_simVYNICN20 <- NSE(obs_bf_removed_byyear[[2]], sim_vy_ni_cn20_byyear[[2]])
ns_an_simCYNI <- NSE(obs_bf_removed_byyear[[2]], sim_cy_ni_byyear[[2]])
ns_an_simVYNI <- NSE(obs_bf_removed_byyear[[2]], sim_vy_ni_byyear[[2]])
ns_an_simVYVI <- NSE(obs_bf_removed_byyear[[2]], sim_vy_vi_byyear[[2]])
ns_an_simVYNI_old <- NSE(obs_bf_removed_byyear[[2]], old_sim_vy_ni_byyear[[2]])
ns_an_simVYVI_old <- NSE(obs_bf_removed_byyear[[2]], old_sim_vy_vi_byyear[[2]])

annual_stats_bf_removed <- rbind(ns_an_simVYI5,ns_an_simVYI5CN20,ns_an_simVYNICN20,ns_an_simCYNI,
                                 ns_an_simVYNI,ns_an_simVYVI,ns_an_simVYNI_old,ns_an_simVYVI_old)

# Combine stats into table

stats <- data.frame(matrix(ncol = 5, nrow=24))
colnames(stats) <- c('Run_Date','Breakdown','Simulation_Type','Sim_Code','Nash_Sutcliffe')

stats[1:3,1] <- '08-09-2017'
stats[4:6,1] <- '08-07-2017'
stats[7:8,1] <- '07-22-2017'
stats[9:16,1] <- stats[1:8,1]
stats[17:24,1] <- stats[1:8,1]

stats[1:8,2] <- 'Daily'
stats[9:16,2] <- 'Monthly'
stats[17:24,2] <- 'Annual'

stats[1,3] <- 'Variable Yield Irrigation 50%'
stats[2,3] <- 'Variable Yield Irrigation 50% Curve Numbers Lowered 20%'
stats[3,3] <- 'Variable Yield No Irrigation Curve Numbers Lowered 20%'
stats[4,3] <- 'Constant Yield No Irrigation'
stats[5,3] <- 'Variable Yield No Irrigation'
stats[6,3] <- 'Variable Yield Variable Irrigation'
stats[7,3] <- 'Variable Yield No Irrigation'
stats[8,3] <- 'Variable Yield Variable Irrigation'

stats[9:16,3] <- stats[1:8,3]
stats[17:24,3] <- stats[1:8,3]

stats[1,4] <- 'VYI5'
stats[2,4] <- 'VYI5CN20'
stats[3,4] <- 'VYNICN20'
stats[4,4] <- 'CYNI'
stats[5,4] <- 'VYNI'
stats[6,4] <- 'VYVI'
stats[7,4] <- 'VYNI'
stats[8,4] <- 'VYVI'
stats[9:16,4] <- stats[1:8,4]
stats[17:24,4] <- stats[1:8,4]

stats[1:8,5] <- daily_stats_bf_removed
stats[9:16,5] <- monthly_stats_bf_removed
stats[17:24,5] <- annual_stats_bf_removed



# write results to csv
output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_output/"
write.csv(stats,paste(output_loc,"MS_Simulation_Statistics_BF_Removed_Aug9.csv"),row.names = F,col.names = T)


# SEASONAL STATISTICS with monthly data -----------------------------------------------------------------------

winter <- c(12,1,2)
spring <- c(3,4,5)
summer <- c(6,7,8)
fall <- c(9,10,11)

season_assign <- function(data_input){
  
  var_name <- deparse(substitute(data_input))
 
   for (i in 1:nrow(data_input)){
    
    if (month(data_input[i,1]) %in% winter){
      
      data_input[i,3] <- 'winter'
      
    }
    
    if (month(data_input[i,1]) %in% spring){
      
      data_input[i,3] <- 'spring'
      
    }
    
    if (month(data_input[i,1]) %in% summer){
      
      data_input[i,3] <- 'summer'
      
    }
    
    if (month(data_input[i,1]) %in% fall){
      
      data_input[i,3] <- 'fall'
      
    }
    
  }
  
  colnames(data_input)[3] <- 'Season'
  assign(var_name,data_input,envir=.GlobalEnv)
}

season_assign(obs_bymonth)
season_assign(obs_bf_removed_bymonth)
season_assign(sim_cy_ni_bymonth)
season_assign(sim_vy_ni_bymonth)
season_assign(sim_vy_vi_bymonth)
season_assign(old_sim_vy_ni_bymonth)
season_assign(old_sim_vy_vi_bymonth)

# Winter
winter_stats_bymonth <- data.frame(matrix(ncol=7,nrow=5))
colnames(winter_stats_bymonth) <- c('Run_Date','Breakdown','Season','Simulation_Type','Sim_Code','Nash_Sutcliffe','Nash_Sutcliffe_BF_Removed')

winter_obs_bymonth <- filter(obs_bymonth,Season == 'winter')
winter_sim_cy_ni_bymonth <- filter(sim_cy_ni_bymonth,Season == 'winter')
winter_sim_vy_ni_bymonth <- filter(sim_vy_ni_bymonth,Season == 'winter')
winter_sim_vy_vi_bymonth <- filter(sim_vy_vi_bymonth,Season == 'winter')
winter_old_sim_vy_ni_bymonth <- filter(old_sim_vy_ni_bymonth,Season == 'winter')
winter_old_sim_vy_vi_bymonth <- filter(old_sim_vy_vi_bymonth,Season == 'winter')

winter_stats_bymonth[1:3,1] <- '08-07-2017'
winter_stats_bymonth[4:5,1] <- '07-22-2017'
winter_stats_bymonth[1:5,2] <- 'Monthly'
winter_stats_bymonth[1:5,3] <- 'winter'
winter_stats_bymonth[1,4] <- 'Constant Yield No Irrigation'
winter_stats_bymonth[2,4] <- 'Variable Yield No Irrigation'
winter_stats_bymonth[3,4] <- 'Variable Yield Variable Irrigation'
winter_stats_bymonth[4,4] <- 'Variable Yield No Irrigation'
winter_stats_bymonth[5,4] <- 'Variable Yield Variable Irrigation'
winter_stats_bymonth[1,5] <- 'CYNI'
winter_stats_bymonth[2,5] <- 'VYNI'
winter_stats_bymonth[3,5] <- 'VYVI'
winter_stats_bymonth[4,5] <- 'VYNI'
winter_stats_bymonth[5,5] <- 'VYVI'

winter_stats_bymonth[1,6] <- NSE(winter_obs_bymonth[[2]],winter_sim_cy_ni_bymonth[[2]])
winter_stats_bymonth[2,6] <- NSE(winter_obs_bymonth[[2]],winter_sim_vy_ni_bymonth[[2]])
winter_stats_bymonth[3,6] <- NSE(winter_obs_bymonth[[2]],winter_sim_vy_vi_bymonth[[2]])
winter_stats_bymonth[4,6] <- NSE(winter_obs_bymonth[[2]],winter_old_sim_vy_ni_bymonth[[2]])
winter_stats_bymonth[5,6] <- NSE(winter_obs_bymonth[[2]],winter_old_sim_vy_vi_bymonth[[2]])

# Spring
spring_stats_bymonth <- data.frame(matrix(ncol=6,nrow=5))
colnames(spring_stats_bymonth) <- c('Run_Date','Breakdown','Season','Simulation_Type','Sim_Code','Nash_Sutcliffe')

spring_obs_bymonth <- filter(obs_bymonth,Season == 'spring')
spring_sim_cy_ni_bymonth <- filter(sim_cy_ni_bymonth,Season == 'spring')
spring_sim_vy_ni_bymonth <- filter(sim_vy_ni_bymonth,Season == 'spring')
spring_sim_vy_vi_bymonth <- filter(sim_vy_vi_bymonth,Season == 'spring')
spring_old_sim_vy_ni_bymonth <- filter(old_sim_vy_ni_bymonth,Season == 'spring')
spring_old_sim_vy_vi_bymonth <- filter(old_sim_vy_vi_bymonth,Season == 'spring')

spring_stats_bymonth[[1]] <- winter_stats_bymonth[[1]]
spring_stats_bymonth[[2]] <- winter_stats_bymonth[[2]]
spring_stats_bymonth[1:5,3] <- 'spring'
spring_stats_bymonth[[4]] <- winter_stats_bymonth[[4]]
spring_stats_bymonth[[5]] <- winter_stats_bymonth[[5]]

spring_stats_bymonth[1,6] <- NSE(spring_obs_bymonth[[2]],spring_sim_cy_ni_bymonth[[2]])
spring_stats_bymonth[2,6] <- NSE(spring_obs_bymonth[[2]],spring_sim_vy_ni_bymonth[[2]])
spring_stats_bymonth[3,6] <- NSE(spring_obs_bymonth[[2]],spring_sim_vy_vi_bymonth[[2]])
spring_stats_bymonth[4,6] <- NSE(spring_obs_bymonth[[2]],spring_old_sim_vy_ni_bymonth[[2]])
spring_stats_bymonth[5,6] <- NSE(spring_obs_bymonth[[2]],spring_old_sim_vy_vi_bymonth[[2]])

# Summer
summer_stats_bymonth <- data.frame(matrix(ncol=6,nrow=5))
colnames(summer_stats_bymonth) <- c('Run_Date','Breakdown','Season','Simulation_Type','Sim_Code','Nash_Sutcliffe')

summer_obs_bymonth <- filter(obs_bymonth,Season == 'summer')
summer_sim_cy_ni_bymonth <- filter(sim_cy_ni_bymonth,Season == 'summer')
summer_sim_vy_ni_bymonth <- filter(sim_vy_ni_bymonth,Season == 'summer')
summer_sim_vy_vi_bymonth <- filter(sim_vy_vi_bymonth,Season == 'summer')
summer_old_sim_vy_ni_bymonth <- filter(old_sim_vy_ni_bymonth,Season == 'summer')
summer_old_sim_vy_vi_bymonth <- filter(old_sim_vy_vi_bymonth,Season == 'summer')

summer_stats_bymonth[[1]] <- winter_stats_bymonth[[1]]
summer_stats_bymonth[[2]] <- winter_stats_bymonth[[2]]
summer_stats_bymonth[1:5,3] <- 'summer'
summer_stats_bymonth[[4]] <- winter_stats_bymonth[[4]]
summer_stats_bymonth[[5]] <- winter_stats_bymonth[[5]]

summer_stats_bymonth[1,6] <- NSE(summer_obs_bymonth[[2]],summer_sim_cy_ni_bymonth[[2]])
summer_stats_bymonth[2,6] <- NSE(summer_obs_bymonth[[2]],summer_sim_vy_ni_bymonth[[2]])
summer_stats_bymonth[3,6] <- NSE(summer_obs_bymonth[[2]],summer_sim_vy_vi_bymonth[[2]])
summer_stats_bymonth[4,6] <- NSE(summer_obs_bymonth[[2]],summer_old_sim_vy_ni_bymonth[[2]])
summer_stats_bymonth[5,6] <- NSE(summer_obs_bymonth[[2]],summer_old_sim_vy_vi_bymonth[[2]])

# Fall
fall_stats_bymonth <- data.frame(matrix(ncol=6,nrow=5))
colnames(fall_stats_bymonth) <- c('Run_Date','Breakdown','Season','Simulation_Type','Sim_Code','Nash_Sutcliffe')

fall_obs_bymonth <- filter(obs_bymonth,Season == 'fall')
fall_sim_cy_ni_bymonth <- filter(sim_cy_ni_bymonth,Season == 'fall')
fall_sim_vy_ni_bymonth <- filter(sim_vy_ni_bymonth,Season == 'fall')
fall_sim_vy_vi_bymonth <- filter(sim_vy_vi_bymonth,Season == 'fall')
fall_old_sim_vy_ni_bymonth <- filter(old_sim_vy_ni_bymonth,Season == 'fall')
fall_old_sim_vy_vi_bymonth <- filter(old_sim_vy_vi_bymonth,Season == 'fall')

fall_stats_bymonth[[1]] <- winter_stats_bymonth[[1]]
fall_stats_bymonth[[2]] <- winter_stats_bymonth[[2]]
fall_stats_bymonth[1:5,3] <- 'fall'
fall_stats_bymonth[[4]] <- winter_stats_bymonth[[4]]
fall_stats_bymonth[[5]] <- winter_stats_bymonth[[5]]

fall_stats_bymonth[1,6] <- NSE(fall_obs_bymonth[[2]],fall_sim_cy_ni_bymonth[[2]])
fall_stats_bymonth[2,6] <- NSE(fall_obs_bymonth[[2]],fall_sim_vy_ni_bymonth[[2]])
fall_stats_bymonth[3,6] <- NSE(fall_obs_bymonth[[2]],fall_sim_vy_vi_bymonth[[2]])
fall_stats_bymonth[4,6] <- NSE(fall_obs_bymonth[[2]],fall_old_sim_vy_ni_bymonth[[2]])
fall_stats_bymonth[5,6] <- NSE(fall_obs_bymonth[[2]],fall_old_sim_vy_vi_bymonth[[2]])


seasonal_stats_monthly <- rbind(winter_stats_bymonth,spring_stats_bymonth,summer_stats_bymonth,fall_stats_bymonth)

write.csv(seasonal_stats_monthly,paste(output_loc,"Seasonal_Monthly_Statistics.csv"),row.names = F,col.names = T)

#BASEFLOW REMOVED stat calculations

# Winter

winter_obs_bf_removed_bymonth <- filter(obs_bf_removed_bymonth,Season == 'winter')

seasonal_stats_monthly[1,7] <- NSE(winter_obs_bf_removed_bymonth[[2]],winter_sim_cy_ni_bymonth[[2]])
seasonal_stats_monthly[2,7] <- NSE(winter_obs_bf_removed_bymonth[[2]],winter_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[3,7] <- NSE(winter_obs_bf_removed_bymonth[[2]],winter_sim_vy_vi_bymonth[[2]])
seasonal_stats_monthly[4,7] <- NSE(winter_obs_bf_removed_bymonth[[2]],winter_old_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[5,7] <- NSE(winter_obs_bf_removed_bymonth[[2]],winter_old_sim_vy_vi_bymonth[[2]])

# Spring

spring_obs_bf_removed_bymonth <- filter(obs_bf_removed_bymonth,Season == 'spring')

seasonal_stats_monthly[6,7] <- NSE(spring_obs_bf_removed_bymonth[[2]],spring_sim_cy_ni_bymonth[[2]])
seasonal_stats_monthly[7,7] <- NSE(spring_obs_bf_removed_bymonth[[2]],spring_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[8,7] <- NSE(spring_obs_bf_removed_bymonth[[2]],spring_sim_vy_vi_bymonth[[2]])
seasonal_stats_monthly[9,7] <- NSE(spring_obs_bf_removed_bymonth[[2]],spring_old_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[10,7] <- NSE(spring_obs_bf_removed_bymonth[[2]],spring_old_sim_vy_vi_bymonth[[2]])

# Summer

summer_obs_bf_removed_bymonth <- filter(obs_bf_removed_bymonth,Season == 'summer')

seasonal_stats_monthly[11,7] <- NSE(summer_obs_bf_removed_bymonth[[2]],summer_sim_cy_ni_bymonth[[2]])
seasonal_stats_monthly[12,7] <- NSE(summer_obs_bf_removed_bymonth[[2]],summer_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[13,7] <- NSE(summer_obs_bf_removed_bymonth[[2]],summer_sim_vy_vi_bymonth[[2]])
seasonal_stats_monthly[14,7] <- NSE(summer_obs_bf_removed_bymonth[[2]],summer_old_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[15,7] <- NSE(summer_obs_bf_removed_bymonth[[2]],summer_old_sim_vy_vi_bymonth[[2]])

# Fall

fall_obs_bf_removed_bymonth <- filter(obs_bf_removed_bymonth,Season == 'fall')

seasonal_stats_monthly[16,7] <- NSE(fall_obs_bf_removed_bymonth[[2]],fall_sim_cy_ni_bymonth[[2]])
seasonal_stats_monthly[17,7] <- NSE(fall_obs_bf_removed_bymonth[[2]],fall_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[18,7] <- NSE(fall_obs_bf_removed_bymonth[[2]],fall_sim_vy_vi_bymonth[[2]])
seasonal_stats_monthly[19,7] <- NSE(fall_obs_bf_removed_bymonth[[2]],fall_old_sim_vy_ni_bymonth[[2]])
seasonal_stats_monthly[20,7] <- NSE(fall_obs_bf_removed_bymonth[[2]],fall_old_sim_vy_vi_bymonth[[2]])


colnames(seasonal_stats_monthly)[7] <- 'Nash_Sutcliffe_BF_Removed'

write.csv(seasonal_stats_monthly,paste(output_loc,"Seasonal_Monthly_Statistics_BF_Removed.csv"),row.names = F,col.names = T)

# SEASONAL STATISTICS with daily data -----------------------------------------------------------------------
season_assign(obs)
season_assign(sim_cy_ni)
season_assign(sim_vy_ni)
season_assign(sim_vy_vi)
season_assign(old_sim_vy_ni)
season_assign(old_sim_vy_vi)

# Precipitation - Residuals dygraph -------------------------------------------------------------------------

precip <- read.csv(paste(ws,"/precipitation.csv",sep=''),stringsAsFactors = FALSE)
precip$Date <- as.Date(precip$Date, '%m/%d/%Y')

# Calculate observed -simulation residuals

residuals <- data.frame(matrix(ncol = 6, nrow=nrow(obs_bf_removed)))
residuals[[1]] <- obs_bf_removed[[1]]
colnames(residuals) <- c('Date','CYNI','VYNI','VYVI','VYNI_old','VYVI_old')

# Absolute value of residuals

residuals[[2]] <- abs(sim_cy_ni[[2]] - obs_bf_removed[[2]])
residuals[[3]] <- abs(sim_vy_ni[[2]] - obs_bf_removed[[2]])  
residuals[[4]] <- abs(sim_vy_vi[[2]] - obs_bf_removed[[2]])
residuals[[5]] <- abs(old_sim_vy_ni[[2]] - obs_bf_removed[[2]])
residuals[[6]] <- abs(old_sim_vy_vi[[2]] - obs_bf_removed[[2]])

residuals[[2]] <- sim_cy_ni[[2]] - obs_bf_removed[[2]]
residuals[[3]] <- sim_vy_ni[[2]] - obs_bf_removed[[2]]
residuals[[4]] <- sim_vy_vi[[2]] - obs_bf_removed[[2]]
residuals[[5]] <- old_sim_vy_ni[[2]] - obs_bf_removed[[2]]
residuals[[6]] <- old_sim_vy_vi[[2]] - obs_bf_removed[[2]]

prcp_res_combined <- cbind(precip,select(residuals,CYNI,VYNI,VYVI,VYNI_old,VYVI_old))

prcp_res <- xts(select(prcp_res_combined,Rain,VYNI),order.by=prcp_res_combined$Date)

dy_prcp_res <- dygraph(prcp_res, main = 'Residuals and Rainfall - August 8 2017 ', height = 450, width = "100%") %>%
                  dyRangeSelector() %>%
                  dySeries("VYNI", label = "Residuals VYNI") %>%  
                  dySeries("Rain", label = "Rainfall", axis = 'y2') %>%
                  dyAxis('y', label = 'Absolute Residual') %>%
                  dyAxis('y2', label = 'Rainfall (mm)', independentTicks = TRUE, drawGrid = FALSE) %>%
                  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),axisLineWidth = 1.5)

dy_prcp_res

# QQplot of the residuals
qqnorm(residuals$VYNI, main = 'VYNI  - Normal Q-Q Plot')
qqline(residuals$VYNI)

mean(residuals$VYNI)

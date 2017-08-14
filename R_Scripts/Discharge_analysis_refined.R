# WES PORTER
# 8/11/2017
# USDA PROJECT - Discharge-analysis-refined

# Script Summary: Create a refined process that removes baseflow, aggregates data, and produces statistics. 

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
setwd(ws)

# Read in USGS observed total streamflow
obs <- read.csv(paste(ws,"/observed_discharge.csv",sep=''),stringsAsFactors = FALSE)
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

obs_bf_removed <- select(obs,Date,Discharge) # Remove baseflow from Observed
colnames(obs_bf_removed) <- c('Date','Q')
obs_bf_removed <- baseflows(obs_bf_removed, ts='daily')
obs_bf_removed[[5]] <- obs_bf_removed[[2]] - obs_bf_removed[[3]]
colnames(obs_bf_removed)[5] <- 'Q_bf_removed'
obs_bf_removed <- select(obs_bf_removed,Date,Q_bf_removed)
colnames(obs_bf_removed) <- c('Date','Discharge')

obs_bf_removed_bymonth <- obs_bf_removed
obs_bf_removed_bymonth$Date <- floor_date(obs_bf_removed_bymonth$Date, "month")
obs_bf_removed_bymonth <- aggregate(Discharge~Date, data=obs_bf_removed_bymonth, FUN=sum)

obs_bf_removed_byyear <- obs_bf_removed
obs_bf_removed_byyear$Date <- floor_date(obs_bf_removed_byyear$Date, "year")
obs_bf_removed_byyear <- aggregate(Discharge~Date, data=obs_bf_removed_byyear, FUN=sum)

# Read in Simulations

files <- list.files(ws,pattern = glob2rx("S*.csv")) # Grab a list of the file names in ws

daily_stats <- data.frame(matrix(ncol=6,nrow=length(files)))
monthly_stats <- data.frame(matrix(ncol=6,nrow=length(files)))
yearly_stats <- data.frame(matrix(ncol=6,nrow=length(files)))
cal_val_stats <- data.frame(matrix(ncol=9,nrow=length(files)))

a <- 1
for (i in files){
  
  df <- read.csv(i) # temp dataframe
  df$Date <- as.Date(df$Date, '%m/%d/%Y')
  name <- substr(i,1,nchar(i)-4) # subtract .csv
  assign(name, as.data.frame(df)) # create a dataframe with appropriate name
  run_date <- substr(file.info(i)$mtime,1,10)
  
  df_bymonth <- df # aggregate bymonth
  df_bymonth$Date <- floor_date(df_bymonth$Date, "month")
  df_bymonth <- aggregate(Discharge~Date, data=df_bymonth, FUN=sum)
  assign(paste(name,'_bymonth',sep=''), as.data.frame(df_bymonth))
  
  df_byyear <- df # aggregate byyear
  df_byyear$Date <- floor_date(df_byyear$Date, "year")
  df_byyear <- aggregate(Discharge~Date, data=df_byyear, FUN=sum)
  assign(paste(name,'_byyear',sep=''), as.data.frame(df_byyear))
  
  ns_d_df <- NSE(df[[2]],obs_bf_removed[[2]]) # calculate stats
  pbias_d_df <- pbias(df[[2]],obs_bf_removed[[2]])
  cod_d_df <- (cor(df[[2]],obs_bf_removed[[2]]))^2
  ns_mo_df <- NSE(df_bymonth[[2]],obs_bf_removed_bymonth[[2]])
  pbias_mo_df <- pbias(df_bymonth[[2]],obs_bf_removed_bymonth[[2]])
  cod_mo_df <- (cor(df_bymonth[[2]],obs_bf_removed_bymonth[[2]]))^2
  ns_an_df <- NSE(df_byyear[[2]],obs_bf_removed_byyear[[2]])
  pbias_an_df <- pbias(df_byyear[[2]],obs_bf_removed_byyear[[2]])
  cod_an_df <- (cor(df_byyear[[2]],obs_bf_removed_byyear[[2]]))^2

  daily_stats[a,1] <- run_date
  daily_stats[a,2] <- 'Daily'
  daily_stats[a,3] <- name
  daily_stats[a,4] <- ns_d_df
  daily_stats[a,5] <- pbias_d_df
  daily_stats[a,6] <- cod_d_df 
  daily_stats <- arrange(daily_stats, desc(daily_stats[[1]]))
  monthly_stats[a,1] <- run_date
  monthly_stats[a,2] <- 'Monthly'
  monthly_stats[a,3] <- name
  monthly_stats[a,4] <- ns_mo_df
  monthly_stats[a,5] <- pbias_mo_df
  monthly_stats[a,6] <- cod_mo_df 
  monthly_stats <- arrange(monthly_stats, desc(monthly_stats[[1]]))
  yearly_stats[a,1] <- run_date
  yearly_stats[a,2] <- 'Annually'
  yearly_stats[a,3] <- name
  yearly_stats[a,4] <- ns_an_df
  yearly_stats[a,5] <- pbias_an_df
  yearly_stats[a,6] <- cod_an_df 
  yearly_stats <- arrange(yearly_stats, desc(yearly_stats[[1]]))
  
  #Calibration Validation
  obs_cal <- filter(obs_bf_removed_bymonth,Date <= '2008-12-01')
  cal <- filter(df_bymonth,Date <= '2008-12-01')
  ns_mo_cal_df <- NSE(cal[[2]],obs_cal[[2]])
  pbias_mo_cal_df <- pbias(cal[[2]],obs_cal[[2]])
  cod_mo_cal_df <- (cor(cal[[2]],obs_cal[[2]]))^2
  
  obs_val <- filter(obs_bf_removed_bymonth,Date >= '2009-01-01')
  val <- filter(df_bymonth,Date >= '2009-01-01')
  ns_mo_val_df <- NSE(val[[2]],obs_val[[2]])
  pbias_mo_val_df <- pbias(val[[2]],obs_val[[2]])
  cod_mo_val_df <- (cor(val[[2]],obs_val[[2]]))^2
  
  cal_val_stats[a,1:3] <- monthly_stats[a,1:3]
  cal_val_stats[a,4] <- ns_mo_cal_df
  cal_val_stats[a,5] <- ns_mo_val_df
  cal_val_stats[a,6] <- pbias_mo_cal_df
  cal_val_stats[a,7] <- cod_mo_cal_df
  cal_val_stats[a,8] <- pbias_mo_val_df
  cal_val_stats[a,9] <- cod_mo_val_df
  
  a <- a+1
    
  rm(df) # remove temp variables
  rm(df_bymonth)
  rm(df_byyear)
}
rm(a)

stats <- rbind(daily_stats,monthly_stats,yearly_stats)
colnames(stats) <- c('Run_Date','Breakdown','Simulation_Type','Nash_Sutcliffe','PBIAS','Rsqr')

colnames(cal_val_stats) <- c('Run_Date','Breakdown','Simulation_Type','Cal_NSE','Cal_PBIAS','Cal_Rsqr','Val_NSE','Val_PBIAS','Val_Rsqr')
cal_val_stats <- arrange(cal_val_stats,desc(Run_Date))

# write stats to csv
output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_output/"
write.csv(stats,paste(output_loc,"MS_Simulation_Statistics_BF_Removed_",Sys.Date(),".csv",sep=''),row.names = F,col.names = T)
write.csv(cal_val_stats,paste(output_loc,"MS_Cal_Val_Stats_BF_Removed_",Sys.Date(),".csv",sep=''),row.names = F,col.names = T)

test <- filter(Sim_CY_NI,year(Date)%%2 == 0)


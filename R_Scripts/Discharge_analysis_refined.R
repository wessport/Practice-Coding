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
in_loc = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_input"


# Read in USGS observed total streamflow
obs <- read.csv(paste(in_loc,"/observed_discharge.csv",sep=''),stringsAsFactors = FALSE)
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

files <- list.files(in_loc,pattern = glob2rx("S*.csv")) # Grab a list of the file names

# Initialize empty stat data frames
daily_stats <- data.frame(matrix(ncol=6,nrow=length(files)))
monthly_stats <- data.frame(matrix(ncol=6,nrow=length(files)))
yearly_stats <- data.frame(matrix(ncol=6,nrow=length(files)))
cal_val_stats <- data.frame(matrix(ncol=9,nrow=length(files)))
cal_val_m2_stats <- data.frame(matrix(ncol=9,nrow=length(files)))

winter <- c(12,1,2)
spring <- c(3,4,5)
summer <- c(6,7,8)
fall <- c(9,10,11)

winter_stats_bymonth <- data.frame(matrix(ncol=8,nrow=length(files)))
spring_stats_bymonth <- data.frame(matrix(ncol=8,nrow=length(files)))
summer_stats_bymonth <- data.frame(matrix(ncol=8,nrow=length(files)))
fall_stats_bymonth <- data.frame(matrix(ncol=8,nrow=length(files)))

# Assign season function
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
season_assign(obs_bf_removed_bymonth)

winter_obs_bymonth <- filter(obs_bf_removed_bymonth,Season == 'winter')
spring_obs_bymonth <- filter(obs_bf_removed_bymonth,Season == 'spring')
summer_obs_bymonth <- filter(obs_bf_removed_bymonth,Season == 'summer')
fall_obs_bymonth <- filter(obs_bf_removed_bymonth,Season == 'fall')

a <- 1
for (i in files){
  
  df <- read.csv(paste(in_loc,'/',i,sep='')) # temp dataframe
  df$Date <- as.Date(df$Date, '%m/%d/%Y')
  name <- substr(i,1,nchar(i)-4) # subtract .csv
  assign(name, as.data.frame(df)) # create a dataframe with appropriate name
  run_date <- substr(file.info(paste(in_loc,'/',i,sep=''))$mtime,1,10)
  
  df_bymonth <- df # aggregate bymonth
  df_bymonth$Date <- floor_date(df_bymonth$Date, "month")
  df_bymonth <- aggregate(Discharge~Date, data=df_bymonth, FUN=sum)
  season_assign(df_bymonth)
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
  monthly_stats[a,1] <- run_date
  monthly_stats[a,2] <- 'Monthly'
  monthly_stats[a,3] <- name
  monthly_stats[a,4] <- ns_mo_df
  monthly_stats[a,5] <- pbias_mo_df
  monthly_stats[a,6] <- cod_mo_df 
  yearly_stats[a,1] <- run_date
  yearly_stats[a,2] <- 'Annually'
  yearly_stats[a,3] <- name
  yearly_stats[a,4] <- ns_an_df
  yearly_stats[a,5] <- pbias_an_df
  yearly_stats[a,6] <- cod_an_df 
  
  winter_df_bymonth <- filter(df_bymonth,Season == 'winter')
  spring_df_bymonth <- filter(df_bymonth,Season == 'spring')
  summer_df_bymonth <- filter(df_bymonth,Season == 'summer')
  fall_df_bymonth <- filter(df_bymonth,Season == 'fall')
  
  winter_stats_bymonth[a,1:3] <- monthly_stats[a,1:3]
  winter_stats_bymonth[a,4] <- 'winter'
  winter_stats_bymonth[a,5] <-  NSE(winter_df_bymonth[[2]],winter_obs_bymonth[[2]])
  winter_stats_bymonth[a,6] <-  pbias(winter_df_bymonth[[2]],winter_obs_bymonth[[2]])
  winter_stats_bymonth[a,7] <-  (cor(winter_df_bymonth[[2]],winter_obs_bymonth[[2]]))^2
  winter_stats_bymonth[a,8] <-  rsr(winter_df_bymonth[[2]],winter_obs_bymonth[[2]])
  winter_stats_bymonth[a,9] <-  mean(winter_df_bymonth[[2]])
  spring_stats_bymonth[a,1:3] <- monthly_stats[a,1:3]
  spring_stats_bymonth[a,4] <- 'spring'
  spring_stats_bymonth[a,5] <-  NSE(spring_df_bymonth[[2]],spring_obs_bymonth[[2]])
  spring_stats_bymonth[a,6] <-  pbias(spring_df_bymonth[[2]],spring_obs_bymonth[[2]])
  spring_stats_bymonth[a,7] <-  (cor(spring_df_bymonth[[2]],spring_obs_bymonth[[2]]))^2
  spring_stats_bymonth[a,8] <-  rsr(spring_df_bymonth[[2]],spring_obs_bymonth[[2]])
  spring_stats_bymonth[a,9] <-  mean(spring_df_bymonth[[2]])
  summer_stats_bymonth[a,1:3] <- monthly_stats[a,1:3]
  summer_stats_bymonth[a,4] <- 'summer'
  summer_stats_bymonth[a,5] <-  NSE(summer_df_bymonth[[2]],summer_obs_bymonth[[2]])
  summer_stats_bymonth[a,6] <-  pbias(summer_df_bymonth[[2]],summer_obs_bymonth[[2]])
  summer_stats_bymonth[a,7] <-  (cor(summer_df_bymonth[[2]],summer_obs_bymonth[[2]]))^2
  summer_stats_bymonth[a,8] <-  rsr(summer_df_bymonth[[2]],summer_obs_bymonth[[2]])
  summer_stats_bymonth[a,9] <-  mean(summer_df_bymonth[[2]])
  fall_stats_bymonth[a,1:3] <- monthly_stats[a,1:3]
  fall_stats_bymonth[a,4] <- 'fall'
  fall_stats_bymonth[a,5] <-  NSE(fall_df_bymonth[[2]],fall_obs_bymonth[[2]])
  fall_stats_bymonth[a,6] <-  pbias(fall_df_bymonth[[2]],fall_obs_bymonth[[2]])
  fall_stats_bymonth[a,7] <-  (cor(fall_df_bymonth[[2]],fall_obs_bymonth[[2]]))^2
  fall_stats_bymonth[a,8] <-  rsr(fall_df_bymonth[[2]],fall_obs_bymonth[[2]])
  fall_stats_bymonth[a,9] <-  mean(fall_df_bymonth[[2]])
  
  #Calibration Validation Method 1 (m1)
  obs_cal <- filter(obs_bf_removed_bymonth,Date <= '2008-12-01')
  cal <- filter(df_bymonth,Date <= '2008-12-01')
  ns_mo_cal_df <- NSE(cal[[2]],obs_cal[[2]])
  pbias_mo_cal_df <- pbias(cal[[2]],obs_cal[[2]])
  cod_mo_cal_df <- (cor(cal[[2]],obs_cal[[2]]))^2
  rsr_mo_cal_df <- rsr(cal[[2]],obs_cal[[2]])
  
  obs_val <- filter(obs_bf_removed_bymonth,Date >= '2009-01-01')
  val <- filter(df_bymonth,Date >= '2009-01-01')
  ns_mo_val_df <- NSE(val[[2]],obs_val[[2]])
  pbias_mo_val_df <- pbias(val[[2]],obs_val[[2]])
  cod_mo_val_df <- (cor(val[[2]],obs_val[[2]]))^2
  rsr_mo_val_df <- rsr(val[[2]],obs_val[[2]])
  
  
  cal_val_stats[a,1:3] <- monthly_stats[a,1:3]
  cal_val_stats[a,4] <- ns_mo_cal_df
  cal_val_stats[a,5] <- ns_mo_val_df 
  cal_val_stats[a,6] <- pbias_mo_cal_df 
  cal_val_stats[a,7] <- pbias_mo_val_df
  cal_val_stats[a,8] <- cod_mo_cal_df
  cal_val_stats[a,9] <- cod_mo_val_df
  cal_val_stats[a,10] <- rsr_mo_cal_df
  cal_val_stats[a,11] <- rsr_mo_val_df
  
  #Calibration Validation Method 2 (m2)
  obs_cal_m2 <- filter(obs_bf_removed_bymonth,year(Date)%%2 == 0)
  cal_m2 <- filter(df_bymonth,year(Date)%%2 == 0)
  ns_mo_cal_m2_df <- NSE(cal_m2[[2]],obs_cal_m2[[2]])
  pbias_mo_cal_m2_df <- pbias(cal_m2[[2]],obs_cal_m2[[2]])
  cod_mo_cal_m2_df <- (cor(cal_m2[[2]],obs_cal_m2[[2]]))^2
  rsr_mo_cal_m2_df <- rsr(cal_m2[[2]],obs_cal_m2[[2]])
  
  obs_val_m2 <- filter(obs_bf_removed_bymonth,year(Date)%%2 == 1)
  val_m2 <- filter(df_bymonth,year(Date)%%2 == 1)
  ns_mo_val_m2_df <- NSE(val_m2[[2]],obs_val_m2[[2]])
  pbias_mo_val_m2_df <- pbias(val_m2[[2]],obs_val_m2[[2]])
  cod_mo_val_m2_df <- (cor(val_m2[[2]],obs_val_m2[[2]]))^2
  rsr_mo_val_m2_df <- rsr(val_m2[[2]],obs_val_m2[[2]])
  
  cal_val_m2_stats[a,1:3] <- monthly_stats[a,1:3]
  cal_val_m2_stats[a,4] <- ns_mo_cal_m2_df
  cal_val_m2_stats[a,5] <- ns_mo_val_m2_df
  cal_val_m2_stats[a,6] <- pbias_mo_cal_m2_df 
  cal_val_m2_stats[a,7] <- pbias_mo_val_m2_df  
  cal_val_m2_stats[a,8] <- cod_mo_cal_m2_df
  cal_val_m2_stats[a,9] <- cod_mo_val_m2_df
  cal_val_m2_stats[a,10] <- rsr_mo_cal_m2_df
  cal_val_m2_stats[a,11] <- rsr_mo_val_m2_df
  
  a <- a+1
    
  rm(df) # remove temp variables
  rm(df_bymonth)
  rm(df_byyear)
}
rm(a)

daily_stats <- arrange(daily_stats, desc(daily_stats[[1]]))
monthly_stats <- arrange(monthly_stats, desc(monthly_stats[[1]]))
yearly_stats <- arrange(yearly_stats, desc(yearly_stats[[1]]))

stats <- rbind(daily_stats,monthly_stats,yearly_stats)
colnames(stats) <- c('Run_Date','Breakdown','Simulation_Type','Nash_Sutcliffe','PBIAS','Rsqr')

winter_stats_bymonth <- arrange(winter_stats_bymonth, desc(winter_stats_bymonth[[1]]))
spring_stats_bymonth <- arrange(spring_stats_bymonth, desc(spring_stats_bymonth[[1]]))
summer_stats_bymonth <- arrange(summer_stats_bymonth, desc(summer_stats_bymonth[[1]]))
fall_stats_bymonth <- arrange(fall_stats_bymonth, desc(fall_stats_bymonth[[1]]))

seasonal_stats_bymonth <- rbind(winter_stats_bymonth,spring_stats_bymonth,summer_stats_bymonth,fall_stats_bymonth)
colnames(seasonal_stats_bymonth) <- c('Run_Date','Breakdown','Simulation_Type','Season','Nash_Sutcliffe','PBIAS',
                                      'Rsqr','RSR','Simulated_Mean')

colnames(cal_val_stats) <- c('Run_Date','Breakdown','Simulation_Type','Cal_NSE','Val_NSE','Cal_PBIAS','Val_PBIAS',
                             'Cal_Rsqr','Val_Rsqr','Cal_RSR','Val_RSR')
cal_val_stats <- arrange(cal_val_stats,desc(Run_Date))

colnames(cal_val_m2_stats) <- c('Run_Date','Breakdown','Simulation_Type','Cal_NSE', 'Val_NSE', 'Cal_PBIAS','Val_PBIAS',
                                'Cal_Rsqr','Val_Rsqr','Cal_RSR','Val_RSR')
cal_val_m2_stats <- arrange(cal_val_m2_stats,desc(Run_Date))

# write stats to csv
output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/Discharge_Analysis_AUGUST_8_2017/R_output/"
write.table(stats,paste(output_loc,"MS_Simulation_Statistics_BF_Removed_",Sys.Date(),".csv",sep=''),row.names = F,col.names = T, sep=',')
write.table(seasonal_stats_bymonth,paste(output_loc,"MS_Sim_Seasonal_Stats_BF_Removed_",Sys.Date(),".csv",sep=''),row.names = F,col.names = T, sep=',')
write.table(cal_val_stats,paste(output_loc,"MS_Cal_Val_M1_Stats_BF_Removed_",Sys.Date(),".csv",sep=''),row.names = F,col.names = T, sep=',')
write.table(cal_val_m2_stats,paste(output_loc,"MS_Cal_Val_M2_Stats_BF_Removed_",Sys.Date(),".csv",sep=''),row.names = F,col.names = T, sep=',')




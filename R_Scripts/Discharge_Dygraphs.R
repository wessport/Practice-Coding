# WES PORTER
# 8/23/2017
# USDA PROJECT - Discharge_Dygraphs

# Script Summary: Readily produce dygraphs.

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


# In files location
in_loc <- file.path("E:","Wes","Work","USDA","raw","Mississippi","MS_BaseflowRemoval","Discharge_Analysis_AUGUST_8_2017","R_input", "Cal_Val_Inputs")

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

source("season_assign.R")
season_assign(obs_bf_removed_bymonth)

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
  
  winter_df_bymonth <- filter(df_bymonth,Season == 'winter')
  spring_df_bymonth <- filter(df_bymonth,Season == 'spring')
  summer_df_bymonth <- filter(df_bymonth,Season == 'summer')
  fall_df_bymonth <- filter(df_bymonth,Season == 'fall')
  
  rm(df) # remove temp variables
  rm(df_bymonth)
  rm(df_byyear)
}

# Dygraphs ----------------------------------------------------------------

dis <- cbind(obs_bf_removed_bymonth,select(Sim_VY_VI_SI_old_bymonth,Discharge))
colnames(dis) <- c('Date','obsDis','Season','Sim_VY_VI_SI_old_bymonth')

winter_months <- which(dis$Season == "winter") 
spring_months <- which(dis$Season == "spring")
summer_months <- which(dis$Season == "summer") 
fall_months <- which(dis$Season == "fall") 
ribbon_data <- rep(0, nrow(dis))
ribbon_data[winter_months] <-  0.0
ribbon_data[spring_months] <-  0.25
ribbon_data[summer_months] <-  0.5
ribbon_data[fall_months] <-  0.75

dis_xts <- xts(select(dis,obsDis,Sim_VY_VI_SI_old_bymonth),order.by=dis$Date)

dy_dis <- dygraph(dis_xts, main = 'Simulated Discharge - Before Calibration Validation', group='ensync',height = 450, width = "100%") %>%
  #dyRangeSelector() %>%
  dySeries("obsDis", label = "Observed") %>%  
  #dySeries("Sim_VY_I5_SCN", label = "Sim_VY_I5_SCN") %>%
  dySeries("Sim_VY_VI_SI_old_bymonth", label = "Sim_VY_VI") %>%
  dyAxis('y', label = ' Discharge (m^3)') %>%
  #dyRibbon(data = ribbon_data, top = 0.15, bottom = 0.0, palette = c("#33A5FF","#A2FF33","#FF3633","#FFC133")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)  

dy_dis


# Sim results for adjusted sec climate runs

dis_opup <- cbind(obs_bf_removed_bymonth, select(Sim_VY_I5_CN16_Int5_OPUP_bymonth, Discharge))
colnames(dis_opup) <- c('Date','obsDis','Season','Sim_VY_I5_CN16_Int5_OPUP_bymonth') 

dis_opup_xts <- xts(select(dis_opup, obsDis, Sim_VY_I5_CN16_Int5_OPUP_bymonth), order.by = dis_opup$Date)

dy_dis_opup <- dygraph(dis_opup_xts, main = "Simulated Discharge - After Calibration Validation", group = 'ensync', height = 450, width = "100%") %>%
  #dyRangeSelector() %>%
  dySeries("obsDis", label = "Observed") %>%  
  dySeries("Sim_VY_I5_CN16_Int5_OPUP_bymonth", label = "Sim_VY_I5_CN16_Int5_OPUP") %>%
  #dySeries("Sim_VY_I5_SCN_Int5_OPUP", label = "Sim_VY_I5_SCN_Int5_OPUP") %>%
  dyAxis('y', label = ' Discharge (m^3)',valueRange = c(0, 500100000)) %>%
  #dyRibbon(data = ribbon_data, top = 0.15, bottom = 0.0, palette = c("#33A5FF","#A2FF33","#FF3633","#FFC133")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"),axisLineWidth = 1.5)

multi_dy <- dy_dis %>% tagList(dy_dis_opup) %>% browsable()
multi_dy


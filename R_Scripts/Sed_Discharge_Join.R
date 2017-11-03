# WES PORTER
# 9/15/2017
# USDA PROJECT - Join daily discharge and discrete sediment tables for Harris Bayou.
# Harris Bayou is missing many dailyflow records making conversion to Mg impossible.

library(dplyr)
library(lubridate)


# Observed sediment location
obs_loc <- file.path("E:","Wes","Work","USDA","raw","Mississippi","MS_BaseflowRemoval","Discharge_Analysis_AUGUST_8_2017","Sediment")

obs_harris <- read.csv(file.path(obs_loc,"harris_bayou_discrete_data.csv"))

obs_harris_dis <- read.csv(file.path(obs_loc, "Harris_Bayou_discharge.csv"))

# Clean up the data structure of discharge data
harris_discharge <- as.data.frame(obs_harris_dis[30:nrow(obs_harris_dis),2:5])
colnames(harris_discharge) <- as.character(unlist(obs_harris_dis[28,2:5])) # Key is to unlist the row first
harris_discharge$datetime <- as.Date(harris_discharge$datetime, '%m/%d/%Y')

obs_harris$DATETIME <- as.Date(obs_harris$DATETIME, '%m/%d/%Y')

# Perform left out join by date
harris_join <- left_join(obs_harris, harris_discharge, by = c('DATETIME' = 'datetime'))

harris_complete <- obs_harris
harris_complete$DAILYFLOW <- harris_join$`79103_00060_00003`

out_loc <- file.path("E:","Wes","Work","USDA","raw","Mississippi","MS_BaseflowRemoval","Discharge_Analysis_AUGUST_8_2017","Sediment")

write.table(harris_complete, file.path(out_loc, "harris_with_discharge.csv"), sep=',', col.names = T, row.names = F, na="")

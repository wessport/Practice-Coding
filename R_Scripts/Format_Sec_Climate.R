# WES PORTER
# 8/3/2017
# USDA PROJECT - Post Processing Secondary climate files.

# Extract transform load output from Matlab script.

library(readr)
library(dplyr)

# Set working directory
setwd("E:/Wes/Work/USDA/raw/Mississippi/MS_Climate_Data/MS_AnnAGNPS_Secondary_Climate/July_28_2017/R_input")

# Read in data
files <- list.files(pattern = '*.csv') # Grab a list of the file names
for (i in files){
  df <- read_csv(i) # temp dataframe
  
  name <- substr(i,1,nchar(i)-4) # subtract .csv
  assign(name, as.data.frame(df)) # create a dataframe with appropriate name
  rm(df) # remove temp variable
}

# Loop through records

output <- select(Output_Clarksdale,DATE,TMAX,TMIN,PRCP)

a<- 1
b<- 1
for (i in 1:nrow(Output_Clarksdale)){
  
  if(Output_Clarksdale[i,5] == -9999.0){ # Check to see if Max Temp iS missing
    
    if(Output_Clarksdale[i-a,5] != -9999.0 & Output_Clarksdale[i+b,5] != -9999.0){
      output[i,2] <- mean(c(Output_Clarksdale[i-a,5],Output_Clarksdale[i+b,5]))
    }
    
    if (Output_Clarksdale[i-a,5] == -9999.0){ # Check to see if the previous record has no data.
      
      while ((i-a)>1 & Output_Clarksdale[i-a,5] == -9999.0){a <- a +1}
    }
    if (Output_Clarksdale[i+b,5] == -9999.0){ # Check to see if the subsequent record has no data.
      
      while ((i+b)<nrow(Output_Clarksdale) & Output_Clarksdale[i+b,5] == -9999.0){b <- b +1}
    }
    
    output[i,2] <- mean(c(Output_Clarksdale[i-a,5],Output_Clarksdale[i+b,5]))
    
  } else {
    a<- 1
    b<- 1}
}



a<- 1
b<- 1
for (i in 1:nrow(Output_Clarksdale)){
  
  if(Output_Clarksdale[i,5] == -9999.0){ # Check to see if Max Temp iS missing
    
    if(Output_Clarksdale[i-a,5] != -9999.0 & Output_Clarksdale[i+b,5] != -9999.0){
      output[i,2] <- mean(c(Output_Clarksdale[i-a,5],Output_Clarksdale[i+b,5]))
    } else {
      while ((i-a)>1 & Output_Clarksdale[i-a,5] == -9999.0){a <- a +1}
      while ((i+b)<nrow(Output_Clarksdale) & Output_Clarksdale[i+b,5] == -9999.0){b <- b +1}
      output[i,2] <- mean(c(Output_Clarksdale[i-a,5],Output_Clarksdale[i+b,5]))
    }
    a<- 1
    b<- 1
  } 
}
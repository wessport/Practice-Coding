# WES PORTER
# 8/3/2017
# USDA PROJECT - Post Processing Secondary climate files.

# Extract transform load output from Matlab script.

library(readr)
library(dplyr)

# Set working directory
setwd("E:\Wes\Work\USDA\raw\Mississippi\MS_Climate_Data\MS_AnnAGNPS_Secondary_Climate\July_28_2017\R_input")

# Read in data
files <- list.files(pattern = '*.csv') # Grab a list of the file names
for (i in files){
  df <- read_csv(i) # temp dataframe
  
  name <- substr(i,1,nchar(i)-4) # subtract .csv
  assign(name, as.data.frame(df)) # create a dataframe with appropriate name
  rm(df) # remove temp variable
}

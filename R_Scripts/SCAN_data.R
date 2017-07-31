# WES PORTER
# 7/28/2017
# USDA PROJECT - SCAN site precipitation data aggregation.

# Working with R to clean up the SCAN Site precipitation data.

library(readr)
library(dplyr)

# Set working directory
setwd("E:/Wes/Work/USDA/raw/Mississippi/MS_Climate_Data/MS_NRCS_SCAN_Stations/R_input")

# Read in precipitation data

files <- list.files(pattern = '*Combined.csv') # Grab a list of the file names
for (i in files){
  df <- read_csv(i) # temp dataframe
  df$Date <- as.Date(df$Date, '%m/%d/%Y') # convert to date format
  df$Precip_2 <- 0 # initialize new column to hold daily precip values
  colnames(df) <- c('Site_ID','Date','Precip','Precip_2')
  name <- substr(i,1,nchar(i)-13) # subtract _Combined.csv
  assign(name, as.data.frame(df)) # create a dataframe with appropriate name
  rm(df) # remove temp variable
}

SCAN_cleanData <- function(SCAN_site){
  
  # Has to come first or else it fails.
  site_name <<- deparse(substitute(SCAN_site))

  # Loop through precipitation records
  a <- 1
  for (i in 1:nrow(SCAN_site)){
    
    if(i == 1){
     
       SCAN_site[i,4] <- SCAN_site[i,3] # Copy over the first record.
       
    } else if (SCAN_site[i,3] == -99.9){ # If no data was reported, report no data value -9999
  
      SCAN_site[i,4] <- -9999
  
    } else if (SCAN_site[i-a,3] == -99.9){ # Check to see if the previous record was no data.
      
      while (SCAN_site[i-a,3] == -99.9){a <- a +1} 
      
      # Continue to look further back until no longer encountering no data.
      # Otherwise will get erroneously high daily precip value. 
      
      SCAN_site[i,4] <- SCAN_site[i,3] - SCAN_site[i-a,3]
      a <- 1
  
    } else if ((SCAN_site[i,3] - SCAN_site[i-1,3])<0){ 
      # At the end of the month will be an accumulated precip value.
      # Beginning of next month will likely be zero. 
      # In these cases, will always get a negative value when comparing end of month and beginning of next.
      # Cannot have negative precip values.
      # Set precip value for beginning of month to zero in such cases.
      
      SCAN_site[i,4] <- 0 
      
    } else {
  
      SCAN_site[i,4] <- SCAN_site[i,3] - SCAN_site[i-1,3]
  
    }
  }
  
  results <<- select(SCAN_site,Site_ID,Date,Precip_2)
  colnames(results) <<- c('Site_ID','Date','Precip')
  
  # Write results to a csv file. 
  write.csv(results,paste(site_name,'_daily.csv', sep=''),row.names = F,col.names = T)

}

SCAN_cleanData(Beasley)
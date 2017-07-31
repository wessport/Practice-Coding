# WES PORTER
# 7/28/2017
# USDA PROJECT - SCAN site precipitation data aggregation.

# Working with R to clean up the SCAN Site precipitation data.

library(readr)
library(dplyr)

# Set working directory
setwd("E:/Wes/Work/USDA/raw/Mississippi/MS_Climate_Data/MS_NRCS_SCAN_Stations/R_input")

# Read in precipitation data
blc <- read_csv("Beasley_Lake_Combined.csv")
blc$Date <- as.Date(blc$Date, '%m/%d/%Y', origin="9-21-1999")
blc$Precip_2 <- 0
colnames(blc) <- c('Site_ID','Date','Precip','Precip_2')

# Loop through precipitation records
a <- 1
for (i in 1:nrow(blc)){
  
  if(i == 1){
   
     blc[i,4] <- blc[i,3] # Copy over the first record.
     
  } else if (blc[i,3] == -99.9){ # If no data was reported, report no data value -9999

    blc[i,4] <- -9999

  } else if (blc[i-a,3] == -99.9){ # Check to see if the previous record was no data.
    
    while (blc[i-a,3] == -99.9){a <- a +1} 
    
    # Continue to look further back until no longer encountering no data.
    # Otherwise will get erroneously high daily precip value. 
    
    blc[i,4] <- blc[i,3] - blc[i-a,3]
    a <- 1

  } else if ((blc[i,3] - blc[i-1,3])<0){ 
    # At the end of the month will be an accumulated precip value.
    # Beginning of next month will likely be zero. 
    # In these cases, will always get a negative value when comparing end of month and beginning of next.
    # Cannot have negative precip values.
    # Set precip value for beginning of month to zero in such cases.
    
    blc[i,4] <- 0 
    
  } else {

    blc[i,4] <- blc[i,3] - blc[i-1,3]

  }
}

results <- select(blc,Site_ID,Date,Precip_2)
colnames(results) <- c('Site_ID','Date','Precip')

# Write results to a csv file. 
write.csv(results,"blc.csv",row.names = F,col.names = T)
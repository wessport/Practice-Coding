# WES PORTER
# 9/14/2017
# USDA PROJECT - Create climate station record without date gaps.

library(dplyr)
library(magrittr)

# Create complete date sequence
date_seq <- as.data.frame(seq(as.Date("1970/1/1"), as.Date("2015/12/31"), "days"))
colnames(date_seq) <- "Date"

# Read in raw climate station files with gaps that haven't been filled.
input_loc <- file.path("E:","Wes","Work","USDA","raw","Scripts","nd_climate_secondary","Sept_7_2017","climate_w_gaps")

# Define data output location
out_loc <- file.path("E:","Wes","Work","USDA","raw","Scripts","nd_climate_secondary","Sept_7_2017","R_output")

# Grab a list of the file names in input folder
files <- list.files(input_loc,pattern = glob2rx("*.csv")) 

#Read in climate station files with gaps
for (i in files){
  
  file_loc <- file.path(input_loc,i)
  
  # Create temp dataframe
  df <- read.csv(file_loc) 
  
  # Convert Month-Day-Year into Date
  d <- as.character(paste(df$Year, df$Month, df$Day, sep="/"))
  d <- as.Date(d)
  
  # Add new attribute column corresponding to Date
  df <-  df %>% as_tibble() %>% mutate(Date = d)
  
  # Perform left out join. Left outer join returns all rows from date_seq,
  # and all columns from date_seq and df. Rows in date_seq with no match 
  # in df will have NA values in the new columns.
  
  df <- left_join(date_seq, df, by = "Date")
  
  # Scrub name from input file
  name <- substr(i,1,nchar(i)-4) # subtract '.csv'
  
  # Create a dataframe with appropriate name
  assign(name, as.data.frame(df)) 
  
  # Write left outer join result to file
  write.table(df, paste(out_loc,"/",name, ".csv", sep=""), sep=",", col.names = T, row.names = F)
  
  # Remove temp variable
  rm(df)
}




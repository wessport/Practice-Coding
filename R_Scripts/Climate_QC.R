# WES PORTER
# 9/11/2017
# USDA PROJECT - Post Processing Secondary climate files. 
# Looks for common errors in the climate files and attempts to fix them.

library(dplyr)

input_loc <- file.path("E:","Wes","Work","USDA","raw","Scripts","nd_climate_secondary","Sept_7_2017","climate_w_gaps")

test_data <- read.csv(file.path(input_loc,"test_data.csv"))

test <-  test_data

climate_qc <-  function(climate_data){
  
  var_name <- deparse(substitute(climate_data))
  
  # Handle no dat for TMIN -------------------------------
  
  tmin_noDat <- which(climate_data$TMIN == -9999)
  
  a <- 1
  b <- 1
  for (i in tmin_noDat){
    
    # Handling noData when encountering as first and last values in vector
    while ((i-a)>1 && climate_data$TMIN[i-a] == -9999.0){a <- a +1}
    while ((i+b)<nrow(climate_data) && climate_data$TMIN[i+b] == -9999.0){b <- b +1}
    
    if (i == 1){
      climate_data$TMIN[i] <- mean(c(climate_data$TMIN[i+b], climate_data$TMIN[i+b+1]))
    }
    
    if (i == nrow(climate_data)){
      climate_data$TMIN[i] <- mean(c(climate_data$TMIN[i-a],climate_data$TMIN[i-(a+1)])) 
    }
    
    if( i != 1 && i != nrow(climate_data)) {
  
    # Replace noData with average TMIN of neighboring records
    climate_data$TMIN[i] <- mean(c(climate_data$TMIN[i-a], climate_data$TMIN[i+b]))
  
    }
    
    a <- 1
    b <- 1
    
  }

  # Handle no dat for TMAX ---------------------------

  tmax_noDat <- which(climate_data$TMAX == -9999)

  a <- 1
  b <- 1
  for (i in tmax_noDat){
    
    # Handling noData when encountering as first and last values in vector
    while ((i-a)>1 && climate_data$TMAX[i-a] == -9999.0){a <- a +1}
    while ((i+b)<nrow(climate_data) && climate_data$TMAX[i+b] == -9999.0){b <- b +1}
    
    if (i == 1){
      climate_data$TMAX[i] <- mean(c(climate_data$TMAX[i+b],climate_data$TMAX[i+b+1]))
    }
    
    if (i == nrow(climate_data)){
      climate_data$TMAX[i] <- mean(c(climate_data$TMAX[i-a],climate_data$TMAX[i-(a+1)]))
    }
    
    if( i != 1 && i != nrow(climate_data)) {
      
      # Replace noData with average TMAX of neighboring records
      climate_data$TMAX[i] <- mean(c(climate_data$TMAX[i-a], climate_data$TMAX[i+b]))
      
    }
    
    a <- 1
    b <- 1
    
  }
  
  # Handle TMIN errors --------------------------
  tmin_errors <- which(climate_data$TMIN > climate_data$TMAX)
  tmin_errors
  
  a <- 1
  b <- 1
  for( i in tmin_errors){
  
    # Handling errors when encountering as first and last values in vector
    if (i == 1){
      
      while(climate_data$TMIN[i] > mean(c(climate_data$TMAX[i+b], climate_data$TMAX[i+b+1]))){b <- b + 1}
      
      climate_data$TMAX[i] <-  mean(c(climate_data$TMAX[i+b], climate_data$TMAX[i+b+1]))
      
    }
    if (i == nrow(climate_data)){
      while(climate_data$TMIN[i] > mean(c(climate_data$TMAX[i-a], climate_data$TMAX[i-(a+1)]))){a <- a + 1}
      
      climate_data$TMAX[i] <-  mean(c(climate_data$TMAX[i-a], climate_data$TMAX[i-(a+1)]))
    }
    
    if( i != 1 && i != nrow(climate_data)) {
    climate_data$TMAX[i] <- mean(c(climate_data$TMAX[i-a], climate_data$TMAX[i+b])) # Replace with average from neighbors
    climate_data$TMIN[i] <- mean(c(climate_data$TMIN[i-a], climate_data$TMIN[i+b])) # Replace with average from neighbors
    
    }
  
    a <- 1
    b <- 1
  }
  
  corrected_records <- length(tmin_errors)
  
  print(paste("Number of corrected TMIN records: ", corrected_records, sep=""))
  
  temp_errors <- which(climate_data$TMIN > climate_data$TMAX)
  
  if(length(temp_errors) < 1){
    print("TEMP QC - PASS")
  } else {
    print("TEMP QC - FAIL")
    temp_errors
  }
  
  assign(paste(var_name, "_QC", sep=''), climate_data, envir=.GlobalEnv)
  
}

climate_qc(test)

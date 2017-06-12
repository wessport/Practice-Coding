# WES PORTER
# 6/12/2017
# USDA PROJECT - Discharge

# Formatting USGS Big Sunflower Stream Disharge data by
# inserting '0' into missing collection dates.

# Workspace
ws = "E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/"

# Read in discharge data from USGS
BS_discharge <- read.csv("E:/Wes/Work/USDA/raw/Mississippi/Ms_BaseflowRemoval/BS_discharge.csv")

View(BS_discharge)

# Read in a vector of all dates between 2000-2015
Dates <- read.csv(paste(ws,"Dates.csv", sep=""),header=FALSE)

View(Dates)

count = 1
index = 1
# Create empty vector to hold results
dis_out <- data.frame(Dates,numeric(1:length(Dates)))

# Loop through dates and check for missing data
for (i in 1:nrow(Dates)){
  
  if (count < nrow(BS_discharge) + 1){
  
    if (Dates[i,1] == BS_discharge[count,1]){
    dis_out[index,2] <- BS_discharge[count,2]
    count = count + 1
    index = index + 1
    
    } else {
      
    dis_out[index,2] <- 0
    index = index + 1 }
    
  } else{
    
  dis_out[index,2] <- 0
  index = index + 1}
  
}

View(dis_out)

# Write results to a csv file. 
write.csv(dis_out,paste(ws,"dis_out.csv"),na="0",
          row.names = F,col.names = F)


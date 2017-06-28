# WES PORTER
# 6/26/2017
# RUSLE2 PROJECT

# Script Summary: Relates Rusle2 slope soil loss rate 
# W/ AnnAGNPS sheet and hill erosion based on date.

library(readr)
library(stringr)

ws <- "E:/Wes/Work/Rusle2/tmp/"

files <- read.table("E:/Wes/Work/Rusle2/tmp/files.txt", header=FALSE, sep=" ", stringsAsFactors=FALSE)

files <- data.frame(files)

dateList <- read.table("E:/Wes/Work/Rusle2/tmp/dates.txt", header=FALSE, sep=" ", stringsAsFactors=FALSE)

dateList <- data.frame(dateList)

processRusle2 <- function(dates, results){
  AGNPS_dates <- read_csv(paste("E:/Wes/Work/Rusle2/tmp/",dates, sep=""),
                          col_names = FALSE)
  #View(AGNPS_dates)
  
  rusle2_raw <- read_csv(paste("E:/Wes/Work/Rusle2/tmp/", results, sep=""), 
                        col_names = FALSE)
  
  rusle <- data.frame(rusle2_raw)
  r <- rusle[,1]
  
  count = 1
  index = 1
  # Create empty vector to hold results
  dis_out <- data.frame(AGNPS_dates, NA)
  
  # Loop through dates 
  for (i in 1:nrow(rusle)){
    if (count < nrow(AGNPS_dates) + 1){
      
      if (rusle[i,1] == AGNPS_dates[count,1]){
        dis_out[index,2] <- rusle[i,2]
        count = count + 1
        index = index + 1
      }
    }
  }
  
  #View(dis_out)
  
  # Write results to a csv file. 
  write.table(dis_out,paste(ws,substr(results, 1, nchar(results)-4),"_proc.csv",sep=""), sep=",", na="0",
            row.names = F,col.names = c("Date","Slope soil loss rate [t/ac]"))
}

# Run preprocess function
# processRusle2("dates_1091.csv", "1091_scn5.csv")

for (i in 1:nrow(files)){
  processRusle2(dateList[i,1],files[i,1])
  #print(files[i,1])
}

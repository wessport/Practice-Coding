library(readr)

ws <- "E:/Wes/Work/Rusle2/tmp/"

AGNPS_dates <- read_csv("E:/Wes/Work/Rusle2/tmp/AGNPS_dates.csv",
                        col_names = FALSE)
View(AGNPS_dates)

rusle2_83_scn5 <- read_csv("E:/Wes/Work/Rusle2/tmp/rusle2_83_scn5.csv", 
                           col_names = FALSE)

rusle <- data.frame(rusle2_83_scn5)
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

View(dis_out)

# Write results to a csv file. 
write.csv(dis_out,paste(ws,"dis_out.csv"),na="0",
          row.names = F,col.names = F)
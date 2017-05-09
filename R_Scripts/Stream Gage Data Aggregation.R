# ##########################################
#   Stream Gage Data Aggregation Script    #
#     Last Edited: September 26 2016       #
#              Wes Porter                  #
############################################

#Packages

library(dplyr)

# Reading the data

filename <- file.choose()
testData <- read.csv(file= filename, header=TRUE, sep=",")

#Removing row 2

end <- nrow(testData)

testData <- dplyr::slice(testData, 2:end)

#Creating a duplicate

testData2 <- testData

#Converting from factor class to date class

testData2$datetime <- as.Date(testData$datetime, format = "%m/%d/%Y")

# Removing Data-value qualification codes

testData2 <- testData2[, -grep("cd$", colnames(testData2))]


#Converting from factor class to numerical class

endCol <- ncol(testData2)

initCol <- which(colnames(testData2)=="datetime") + 1


testData2[,initCol:endCol] <- lapply(testData2[,3:endCol, drop=FALSE], as.character)

testData2[,initCol:endCol] <- lapply(testData2[,3:endCol, drop=FALSE], as.numeric)


# Handling missing data or NAs - NOAA uses "9999" to denote missing data, same approach used here

convert <- function (x) {
  x[is.na(x)] <- 9999
  return(x)
}

testData2 <- convert(testData2)  

# Creating mean function

stat <- function(x) (mean = signif(mean(x), digits= 5 ))

# Aggregating 15min interval measurements into daily average



# agg <- function (x) {aggResults <- aggregate(x ~ testData2$datetime, testData2, stat)
#                         return(aggResults)}
# 
# test <- agg(testData2[,3])
# 
# 
# test <- testData2[,initCol:endCol]
# 
# test <- cbind(testData2[1:2], apply(testData2[3:4],2, agg))




aggResults <-aggregate(testData2$Gage.height..feet ~ testData2$datetime, testData2, stat)

newCol <- aggregate(testData2$Discharge..cubic.feet.per.second ~ testData2$datetime, testData2, stat)

colnames(newCol) <- c("Date", "Discharge Cubic Ft per Sec")

newCol <- newCol$`Discharge Cubic Ft per Sec`

aggResults$newCol <- newCol

colnames(aggResults) <- c("Date","Gage Height ft","Discharge Cubic Ft per Sec")

write.table(aggResults, "C:\\Users\\wsp2sgis\\Desktop\\aggResults.txt", sep="\t")




# WES PORTER
# 23-FEB-2018
# USDA PROJECT - phenology curves

library(dplyr)
library(magrittr)

# Set working directory
setwd("E:/Wes/Work/USDA/raw/Mississippi/MS_NDVI/Field_raster")

output_loc <- "E:/Wes/Work/USDA/raw/Mississippi/MS_NDVI/Field_raster/R_output"

# Read in all stats
ndvi_stats_2000 <- read.csv("E:/Wes/Work/USDA/raw/Mississippi/MS_NDVI/Field_raster/ndvi_stats_2000.csv", stringsAsFactors = FALSE)

# Filter by crop type
corn <- filter(ndvi_stats_2000, ndvi_stats_2000$cdl == 1)
cotton <- filter(ndvi_stats_2000, ndvi_stats_2000$cdl == 2)
rice <- filter(ndvi_stats_2000, ndvi_stats_2000$cdl == 3)
soybeans <- filter(ndvi_stats_2000, ndvi_stats_2000$cdl == 5)
fallow <- filter(ndvi_stats_2000, ndvi_stats_2000$cdl == 61)

# Filter out no data
corn <- filter(corn, corn$avg > 0)
cotton <- filter(cotton, cotton$avg > 0)
rice <- filter(rice, rice$avg > 0)
soybeans <- filter(soybeans, soybeans$avg > 0)
fallow <- filter(fallow, fallow$avg > 0)

# Calculate stats on cleaned data
corn %>%
  group_by(doy) %>%
    summarise(average = mean(avg), maximum = max(avg), minimum = min(avg), stdev = sd(avg) ) -> corn_stats

cotton %>%
  group_by(doy) %>%
  summarise(average = mean(avg), maximum = max(avg), minimum = min(avg), stdev = sd(avg) ) -> cotton_stats

rice %>%
  group_by(doy) %>%
  summarise(average = mean(avg), maximum = max(avg), minimum = min(avg), stdev = sd(avg) ) -> rice_stats

soybeans %>%
  group_by(doy) %>%
  summarise(average = mean(avg), maximum = max(avg), minimum = min(avg), stdev = sd(avg) ) -> soybeans_stats

fallow %>%
  group_by(doy) %>%
  summarise(average = mean(avg), maximum = max(avg), minimum = min(avg), stdev = sd(avg) ) -> fallow_stats

# Gather stats
crops <- list(corn_stats, cotton_stats, rice_stats, soybeans_stats, fallow_stats)
crop_names <- c('Corn','Cotton','Rice','Soybean','Fallow')
par(mfrow=c(2,3)) # all plots on one page 

c <- 2 
for(i in 1:length(crops)){ 

  crop <- crops[[i]]
  
  heading = paste("Crop_ID = ", crop_names[i]) 
  plot(x=crop$doy, y=crop$average, ylim=c(1000, 8500), type="l", main=heading, col = c, xlab = 'd.o.y.', ylab = 'avg ndvi') 
  lines(x=crop$doy, y=crop$average, type="l", col = c)
  c <- c + 1
}
par(mfrow=c(1,1)) # Reset plot parameters back to single plot single page

# Plot our phenology curves all together
plot(rice_stats$doy, rice_stats$average, type='l', col = 4, main = 'Phenology Curves', xlab = 'Day of Year', ylab = 'Avg NDVI')   
lines(x=cotton_stats$doy, y=cotton_stats$average, type="l", col = 3)
lines(x=corn_stats$doy, y=corn_stats$average, type="l", col = 2)
lines(x=soybeans_stats$doy, y=soybeans_stats$average, type="l", col = 5)
lines(x=fallow_stats$doy, y=fallow_stats$average, type="l", col = 6)

legend('topleft',
  legend = c("Corn", "Cotton", "Rice", "Soybean", "Fallow"),
  col = c(2,3,4,5,6),
  lty=c(1,1)
)


# Join cdl information to smoothed ndvi values
smoothed_ndvi_cdl <- right_join(smoothed_ndvi, ndvi_stats_2000, by = c("field_ID" = "field_id"))
smoothed_ndvi_cdl <- select(test, field_ID, DOY, mean_ndvi, cdl)






  
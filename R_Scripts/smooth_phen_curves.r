# WES PORTER
# 12-MAR-2018
# USDA PROJECT - smoothed phenology curves

library(dplyr)
library(magrittr)
library(tidyr)

setwd("E:/Wes/Work/USDA/raw/NDVI_Smoothing")

# Read in all stats
ndvi_stats_2000 <- read.csv("E:/Wes/Work/USDA/raw/Mississippi/MS_NDVI/Field_raster/ndvi_stats_2000.csv", stringsAsFactors = FALSE)

nd_ndvi_stats_2000 <- read.csv("E:/Wes/Work/USDA/raw/North_Dakota/ND_NDVI/Field_raster/ndvi_stats_2000.csv", stringsAsFactors = FALSE)

# nd_cdl_2000_maj <- read.csv("E:/Wes/Work/USDA/raw/North_Dakota/ND_NDVI/Field_raster/ND_agg_fields_2000cdl_maj.csv", stringsAsFactors = FALSE)
# 
# ndvi_stats_2000_cor <- left_join(nd_ndvi_stats_2000, nd_cdl_2000_maj, by = c("field_id" = "FID"))

# Read in smoothed ndvi
smoothed_ndvi <- read.csv("E:/Wes/Work/USDA/raw/NDVI_Smoothing/smoothed_ndvi.csv", stringsAsFactors = FALSE)

nd_smoothed_ndvi_2000 <- read.csv("E:/Wes/Work/USDA/raw/NDVI_Smoothing/nd_smoothed_ndvi_2000.csv", stringsAsFactors = FALSE)

# Join cdl information to smoothed ndvi values
smoothed_ndvi_cdl <- left_join(smoothed_ndvi, ndvi_stats_2000, by = c("field_ID" = "field_id", "DOY" = "doy"))
smoothed_ndvi_cdl <- select(smoothed_ndvi_cdl, field_ID, DOY, mean_ndvi, cdl)

nd_smoothed_ndvi_cdl_2000 <- left_join(nd_smoothed_ndvi_2000, nd_ndvi_stats_2000, by = c("field_ID" = "field_id", "DOY" = "doy"))
nd_smoothed_ndvi_cdl_2000 <- select(nd_smoothed_ndvi_cdl_2000, field_ID, DOY, mean_ndvi, cdl)

# write.table(nd_smoothed_ndvi_cdl_2000,"nd_smoothed_ndvi_cdl_2000.csv", row.names = F, col.names = T, sep=',')

# Weighted_ndvi_stats
weighted_ndvi_stats <- read.csv("E:/Wes/Work/USDA/raw/NDVI_Smoothing/weighted_ndvi_stats.csv", stringsAsFactors = FALSE)

# Filter by crop type
corn <- filter(smoothed_ndvi_cdl, smoothed_ndvi_cdl$cdl == 1)
cotton <- filter(smoothed_ndvi_cdl, smoothed_ndvi_cdl$cdl == 2)
rice <- filter(smoothed_ndvi_cdl, smoothed_ndvi_cdl$cdl == 3)
soybeans <- filter(smoothed_ndvi_cdl, smoothed_ndvi_cdl$cdl == 5)
fallow <- filter(smoothed_ndvi_cdl, smoothed_ndvi_cdl$cdl == 61)

nd_corn <- filter(nd_smoothed_ndvi_cdl_2000, nd_smoothed_ndvi_cdl_2000$cdl == 1)
nd_spring_wheat <- filter(nd_smoothed_ndvi_cdl_2000, nd_smoothed_ndvi_cdl_2000$cdl == 23)
nd_sunflower <- filter(nd_smoothed_ndvi_cdl_2000, nd_smoothed_ndvi_cdl_2000$cdl == 6)
nd_soybeans <- filter(nd_smoothed_ndvi_cdl_2000, nd_smoothed_ndvi_cdl_2000$cdl == 5)
nd_fallow <- filter(nd_smoothed_ndvi_cdl_2000, nd_smoothed_ndvi_cdl_2000$cdl == 61)

# Calculate stats on cleaned data
corn %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> corn_stats

cotton %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> cotton_stats

rice %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> rice_stats

soybeans %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> soybeans_stats

fallow %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> fallow_stats

# write.table(corn_stats,"ms_corn_ref.csv", row.names = F, col.names = T, sep=',')
# write.table(cotton_stats,"ms_cotton_ref.csv", row.names = F, col.names = T, sep=',')
# write.table(rice_stats,"ms_rice_ref.csv", row.names = F, col.names = T, sep=',')
# write.table(soybeans_stats,"ms_soybeans_ref.csv", row.names = F, col.names = T, sep=',')
# write.table(fallow_stats,"ms_fallow_ref.csv", row.names = F, col.names = T, sep=',')

nd_corn %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> nd_corn_stats

nd_spring_wheat %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> nd_spring_wheat_stats

nd_sunflower %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> nd_sunflower_stats

nd_soybeans %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> nd_soybeans_stats

nd_fallow %>%
  group_by(DOY) %>%
  summarise(average = mean(mean_ndvi), maximum = max(mean_ndvi), minimum = min(mean_ndvi), stdev = sd(mean_ndvi) ) -> nd_fallow_stats

write.table(nd_corn_stats,"nd_corn_ref.csv", row.names = F, col.names = T, sep=',')
write.table(nd_spring_wheat_stats,"nd_spring_wheat_ref.csv", row.names = F, col.names = T, sep=',')
write.table(nd_sunflower_stats,"nd_sunflower_ref.csv", row.names = F, col.names = T, sep=',')
write.table(nd_soybeans_stats,"nd_soybeans_ref.csv", row.names = F, col.names = T, sep=',')
write.table(nd_fallow_stats,"nd_fallow_ref.csv", row.names = F, col.names = T, sep=',')

# Gather stats
crops <- list(corn_stats, cotton_stats, rice_stats, soybeans_stats, fallow_stats)
crop_names <- c('Corn','Cotton','Rice','Soybean','Fallow')
par(mfrow=c(2,3)) # all plots on one page 

c <- 2 
for(i in 1:length(crops)){ 
  
  crop <- crops[[i]]
  
  heading = paste("Crop_ID = ", crop_names[i]) 
  plot(x=crop$DOY, y=crop$average, ylim=c(1000, 8500), type="l", main=heading, col = c, xlab = 'd.o.y.', ylab = 'avg ndvi') 
  lines(x=crop$DOY, y=crop$average, type="l", col = c)
  c <- c + 1
}
par(mfrow=c(1,1)) # Reset plot parameters back to single plot single page

# Plot our phenology curves all together
plot(rice_stats$DOY, rice_stats$average, type='l', col = 4, main = 'Phenology Curves', xlab = 'Day of Year', ylab = 'Avg NDVI')   
lines(x=cotton_stats$DOY, y=cotton_stats$average, type="l", col = 3)
lines(x=corn_stats$DOY, y=corn_stats$average, type="l", col = 2)
lines(x=soybeans_stats$DOY, y=soybeans_stats$average, type="l", col = 5)
lines(x=fallow_stats$DOY, y=fallow_stats$average, type="l", col = 6)

legend('topleft',
       legend = c("Corn", "Cotton", "Rice", "Soybean", "Fallow"),
       col = c(2,3,4,5,6),
       lty=c(1,1)
)

# ND
plot(nd_corn_stats$DOY, nd_corn_stats$average, type='l', col = 2, main = 'Phenology Curves', xlab = 'Day of Year', ylab = 'Avg NDVI')   
lines(x=nd_soybeans_stats$DOY, y=nd_soybeans_stats$average, type="l", col = 5)
lines(x=nd_fallow_stats$DOY, y=nd_fallow_stats$average, type="l", col = 6)
lines(x=nd_spring_wheat_stats$DOY, y=nd_spring_wheat_stats$average, type="l", col = 3)
lines(x=nd_sunflower_stats$DOY, y=nd_sunflower_stats$average, type="l", col = 4)

legend('topleft',
       legend = c("Corn", "Soybean", "Fallow", "Spring Wheat", "Sunflower"),
       col = c(2,5,6,3,4),
       lty=c(1,1)
)

##### Classification
weighted_ndvi_stats %>%
  distinct(field_id) -> fields

f <- fields[[1]] 

# for (i in f){
#   smoothed_ndvi %>%
#     filter(field_ID == i) %>%
#       mutate(corn_stats, mean_ndvi - average) -> temp
#   
#   if (i==4){break}
#   
# }

# Determine lowest sum of residuals
for (i in f){
  
  if (i == 0){
    
  smoothed_ndvi %>%
    filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats$average,
             soybeans = mean_ndvi - soybeans_stats$average,
             cotton = mean_ndvi - cotton_stats$average,
             rice = mean_ndvi - rice_stats$average,
             fallow = mean_ndvi - fallow_stats$average) %>%
      summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      gather(crop, residual) %>%
      mutate(field_ID = i) %>%
      slice(which.min(residual)) -> temp
    
  } else {
    
    smoothed_ndvi %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats$average,
             soybeans = mean_ndvi - soybeans_stats$average,
             cotton = mean_ndvi - cotton_stats$average,
             rice = mean_ndvi - rice_stats$average,
             fallow = mean_ndvi - fallow_stats$average) %>%
      summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      gather(crop, residual) %>%
      mutate(field_ID = i) %>%
      slice(which.min(residual)) %>%
      bind_rows(temp) -> temp
  }


  #if (i==4){break}
}


# Assign CDL value

for (i in 1:nrow(temp)) {
  if (temp$crop[i] == 'corn') {
    temp$p_cdl[i] = 1
  } else if (temp$crop[i] == 'soybeans') {
    temp$p_cdl[i] = 5
  } else if (temp$crop[i] == 'cotton') {
    temp$p_cdl[i] = 2
  } else if (temp$crop[i] == 'rice') {
    temp$p_cdl[i] = 3
  } else if (temp$crop[i] == 'fallow') {
    temp$p_cdl[i] = 61
  }
}

classified_fields <- temp
  
##### Compare classification

# Create df of fields and corresponding CDL ID
smoothed_ndvi_cdl %>% distinct(field_ID, .keep_all=TRUE) %>%
  select(field_ID, cdl) -> cdl_fields

# Remove fields that do not fall into one of the five crop types of interest 
cdl_fields %>% filter(cdl == 1 | cdl == 5 | cdl == 2 | cdl == 3 | cdl == 61) -> cdl_fields

# Join classified and cdl tables together for comparison

cdl_fields %>% left_join(classified_fields, by = c('field_ID'='field_ID')) -> results

results %>% filter(cdl == 1) -> c_results
results %>% filter(cdl == 5) -> s_results
results %>% filter(cdl == 2) -> co_results
results %>% filter(cdl == 3) -> r_results
results %>% filter(cdl == 61) -> fa_results

test %>% filter(cdl == 1) -> c_results
test %>% filter(cdl == 7) -> sc_results
test %>% filter(cdl == 3) -> r_results
test %>% filter(cdl == 61) -> fa_results

results %>% 
  mutate(p_cdl=replace(p_cdl, p_cdl == 5 | p_cdl == 2, 7)) %>%
  mutate(cdl=replace(cdl, cdl == 5 | cdl == 2, 7))-> test

# Count number of correctly classified fields

count <-  0
for (i in 1:nrow(test)) {

  if(test$cdl[i] == test$p_cdl[i]){ count <- count + 1}
  
}

percent_correct <- count/nrow(test)*100

count <-  0
for (i in 1:nrow(r_results)) {
  
  if(r_results$cdl[i] == r_results$p_cdl[i]){ count <- count + 1}
  #if(r_results$cdl[i] != r_results$p_cdl[i] & r_results$p_cdl[i] == 1){ count <- count + 1}
  
}

# total 35.8 - not great

# Investigation of results

results %>% filter(cdl != p_cdl) -> miss_class

# What if we only looked at growing season doy 84+

smoothed_ndvi %>% filter(DOY > 83) -> smoothed_ndvi_gs
corn_stats %>% filter(DOY > 83) -> corn_stats_gs
soybeans_stats %>% filter(DOY > 83) -> soybeans_stats_gs
cotton_stats %>% filter(DOY > 83) -> cotton_stats_gs
rice_stats %>% filter(DOY > 83) -> rice_stats_gs
fallow_stats %>% filter(DOY > 83) -> fallow_stats_gs

for (i in f){
  
  if (i == 0){
    
    smoothed_ndvi_gs %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats_gs$average,
             soybeans = mean_ndvi - soybeans_stats_gs$average,
             cotton = mean_ndvi - cotton_stats_gs$average,
             rice = mean_ndvi - rice_stats_gs$average,
             fallow = mean_ndvi - fallow_stats_gs$average) %>%
      summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      gather(crop, residual) %>%
      mutate(field_ID = i) %>%
      slice(which.min(residual)) -> temp
    
  } else {
    
    smoothed_ndvi_gs %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats_gs$average,
             soybeans = mean_ndvi - soybeans_stats_gs$average,
             cotton = mean_ndvi - cotton_stats_gs$average,
             rice = mean_ndvi - rice_stats_gs$average,
             fallow = mean_ndvi - fallow_stats_gs$average) %>%
      summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      gather(crop, residual) %>%
      mutate(field_ID = i) %>%
      slice(which.min(residual)) %>%
      bind_rows(temp) -> temp
  }
  
  
  #if (i==4){break}
}

for (i in 1:nrow(temp)) {
  if (temp$crop[i] == 'corn') {
    temp$p_cdl[i] = 1
  } else if (temp$crop[i] == 'soybeans') {
    temp$p_cdl[i] = 5
  } else if (temp$crop[i] == 'cotton') {
    temp$p_cdl[i] = 2
  } else if (temp$crop[i] == 'rice') {
    temp$p_cdl[i] = 3
  } else if (temp$crop[i] == 'fallow') {
    temp$p_cdl[i] = 61
  }
}

cdl_fields %>% left_join(temp, by = c('field_ID'='field_ID')) -> results_gs

# Count number of correctly classified fields

count <-  0
for (i in 1:nrow(results_gs)) {
  
  if(results_gs$cdl[i] == results_gs$p_cdl[i]){ count <- count + 1}
  
}

percent_correct <- count/nrow(results_gs)*100

# 33% using only the growing season

##### Classification in steps

# git rid of 


# Create results table
cdl_fields %>%
  select(field_ID) %>%
  arrange(field_ID) %>%
  mutate(p_cdl = NA) -> results

r <-  results[[1]]

# Classify Fallow
for (i in r){

  # Only interested in growing season
  field <- filter(smoothed_ndvi, field_ID == i & DOY > 83)
    
  if(max(field$mean_ndvi) < 4000){
    
    results[results$field_id == i,2] <- 61
    
  } 
}

# Classify Rice --------------- stopped here ------------------
results %>%
  filter(is.na(p_cdl)) -> unclassified

r <-  unclassified[[1]]

for (i in r){
  
  if (i == 0){
    
    smoothed_ndvi %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats$average,
             soybeans = mean_ndvi - soybeans_stats$average,
             cotton = mean_ndvi - cotton_stats$average,
             rice = mean_ndvi - rice_stats$average) %>%
      summarise_at(vars(corn,soybeans,cotton,rice), funs(sum(abs(.)))) %>%
      gather(crop, residual) %>%
      mutate(field_ID = i) %>%
      slice(which.min(residual)) -> temp
    
  } else {
    
    smoothed_ndvi %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats$average,
             soybeans = mean_ndvi - soybeans_stats$average,
             cotton = mean_ndvi - cotton_stats$average,
             rice = mean_ndvi - rice_stats$average) %>%
      summarise_at(vars(corn,soybeans,cotton,rice), funs(sum(abs(.)))) %>%
      gather(crop, residual) %>%
      mutate(field_ID = i) %>%
      slice(which.min(residual)) %>%
      bind_rows(temp) -> temp
  }
  
  
  #if (i==4){break}
}


##### Classifying each DOY

for (i in f){
  
  if (i == 0){
    
    smoothed_ndvi %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats$average,
             soybeans = mean_ndvi - soybeans_stats$average,
             cotton = mean_ndvi - cotton_stats$average,
             rice = mean_ndvi - rice_stats$average,
             fallow = mean_ndvi - fallow_stats$average) -> test
      # summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      # gather(crop, residual) %>%
      # mutate(field_ID = i) %>%
      # slice(which.min(residual)) -> temp
    
  } else {
    
    smoothed_ndvi %>%
      filter(field_ID == i) %>%
      mutate(corn = mean_ndvi - corn_stats$average,
             soybeans = mean_ndvi - soybeans_stats$average,
             cotton = mean_ndvi - cotton_stats$average,
             rice = mean_ndvi - rice_stats$average,
             fallow = mean_ndvi - fallow_stats$average) %>%
      bind_rows(test) -> test
      # summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      # gather(crop, residual) %>%
      # mutate(field_ID = i) %>%
      # slice(which.min(residual)) %>%
      # bind_rows(temp) -> temp
  }
  
  
  #if (i==4){break}
}

test %>% mutate(p_cdl = NA) -> test

for (i in 1:nrow(test)) {
  
  c <- abs(test$corn[i])
  s <- abs(test$soybeans[i])
  r <- abs(test$rice[i])
  co <- abs(test$cotton[i])
  fa <- abs(test$fallow[i])
  
  minimum <- min(c,s,r,co,fa)
  
  if (c == minimum) {
    test$p_cdl[i] <-  1
    
    
  } else if (s == minimum) {
    test$p_cdl[i] <-  5
    
  } else if (co == minimum) {
    test$p_cdl[i] <-  2
    
  } else if (r == minimum) {
    test$p_cdl[i] <-  3
    
  } else if (fa == minimum) {
    test$p_cdl[i] <-  61
  }
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

test %>%
  group_by(field_ID) %>%
    summarise(mode_cdl = Mode(p_cdl)) -> test2

cdl_fields %>% left_join(test2, by = c('field_ID'='field_ID')) -> results_mode

count <-  0
for (i in 1:nrow(results_mode)) {
  
  if(results_mode$cdl[i] == results_mode$mode_cdl[i]){ count <- count + 1}
  
}

percent_correct <- count/nrow(results_mode)*100

# 21%

##### Rudimentary Classification Tree


# Determine lowest sum of residuals
for (i in f){
  
  if (i == 0){
    
    smoothed_ndvi %>%
      filter(field_ID == i & DOY > 83) %>%
      summarise(avg = mean(mean_ndvi)) -> temp
      
      # mutate(corn = mean_ndvi - corn_stats$average,
      #        soybeans = mean_ndvi - soybeans_stats$average,
      #        cotton = mean_ndvi - cotton_stats$average,
      #        rice = mean_ndvi - rice_stats$average,
      #        fallow = mean_ndvi - fallow_stats$average) %>%
      # summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      # gather(crop, residual) %>%
      # mutate(field_ID = i) %>%
      # slice(which.min(residual)) -> temp
    
  } else {
    
    smoothed_ndvi %>%
      filter(field_ID == i) %>%
      
      # mutate(corn = mean_ndvi - corn_stats$average,
      #        soybeans = mean_ndvi - soybeans_stats$average,
      #        cotton = mean_ndvi - cotton_stats$average,
      #        rice = mean_ndvi - rice_stats$average,
      #        fallow = mean_ndvi - fallow_stats$average) %>%
      # summarise_at(vars(corn,soybeans,cotton,rice,fallow), funs(sum(abs(.)))) %>%
      # gather(crop, residual) %>%
      # mutate(field_ID = i) %>%
      # slice(which.min(residual)) %>%
      # bind_rows(temp) -> temp
  }
  
  
  if (i==4){break}
}


# Assign CDL value

for (i in 1:nrow(temp)) {
  if (temp$crop[i] == 'corn') {
    temp$p_cdl[i] = 1
  } else if (temp$crop[i] == 'soybeans') {
    temp$p_cdl[i] = 5
  } else if (temp$crop[i] == 'cotton') {
    temp$p_cdl[i] = 2
  } else if (temp$crop[i] == 'rice') {
    temp$p_cdl[i] = 3
  } else if (temp$crop[i] == 'fallow') {
    temp$p_cdl[i] = 61
  }
}

classified_fields <- temp


























# WES PORTER
# 8/31/2017
# USDA PROJECT - Baseflow trend

# Script Summary: Plotting baseflow and observed discharge.

library(dygraphs)
library(dplyr)
library(htmltools)
library(hydrostats)
library(hydroGOF)
library(lubridate)
library(RColorBrewer)
library(scales)
library(xts)
library(zoo)

in_loc <- file.path("E:","Wes","Work","USDA","raw","Mississippi","Ms_BaseflowRemoval","Discharge_Analysis_AUGUST_8_2017","R_input")

# Read in USGS observed total streamflow
obs <- read.csv(file.path(in_loc,"observed_discharge.csv"),stringsAsFactors = FALSE)
obs$Date <- as.Date(obs$Date, '%m/%d/%Y')

# Calculate baseflow from Observed
obs_bf <- select(obs,Date,Discharge) 
colnames(obs_bf) <- c('Date','Q')
obs_bf <- baseflows(obs_bf, ts='daily')

x_lab <- "Date"
y_lab <- "Baseflow [m^3]"

plot(obs_bf$Date, obs_bf$bf, main = "Baseflow", xlab = x_lab, ylab = y_lab, col = "blue", pch = 16)

lin_reg <- lm(obs_bf$bf ~ obs_bf$Date)

# Add line - Intercept of linear reg and slope
abline(lin_reg$coefficients[1],lin_reg$coefficients[2], col = "red")
summary(lin_reg)

# Baseflow Calibration period

bf_cal <- filter(obs_bf,Date <= '2008-12-01')

plot(bf_cal$Date, bf_cal$bf, main = "Baseflow - Calibration Period", xlab = x_lab, ylab = y_lab, col = "blue", pch = 16)

fit_cal <- lm(bf_cal$bf ~ bf_cal$Date)
abline(lm(bf_cal$bf ~ bf_cal$Date), col = "red")
summary(fit_cal)

# Baseflow Validation period

bf_val <- filter(obs_bf,Date >= '2008-12-01')

plot(bf_val$Date, bf_val$bf, main = "Baseflow - Validation Period", xlab = x_lab, ylab = y_lab, col = "blue", pch = 16 )

fit_val <- lm(bf_val$bf ~ bf_val$Date)
abline(lm(bf_val$bf ~ bf_val$Date), col = "red")
summary(fit_val)


# Observed (Baseflow removed)

obs_bf[[5]] <- obs_bf[[2]] - obs_bf[[3]]
colnames(obs_bf)[5] <- 'Q_bf_removed'
obs_bf_removed <- select(obs_bf,Date,Q_bf_removed)
colnames(obs_bf_removed) <- c('Date','Discharge')

y_lab2 <- "Discharge [m^3]"
plot(obs_bf_removed$Date, obs_bf_removed$Discharge, main = "Observed (Baseflow Removed)", xlab = x_lab, ylab = y_lab2, col = "blue", pch = 16)

obs_fit <- lm(obs_bf_removed$Discharge ~ obs_bf_removed$Date)
abline(lm(obs_bf_removed$Discharge ~ obs_bf_removed$Date), col = "red")
summary(obs_fit)


# Observed Calibration Period

obs_cal <- filter(obs_bf_removed,Date <= '2008-12-01')

plot(obs_cal$Date, obs_cal$Discharge, main = "Observed - Calibration Period", xlab = x_lab, ylab = y_lab2, col = "blue", pch = 16)

obs_cal_fit <- lm(obs_cal$Discharge ~ obs_cal$Date)
abline(lm(obs_cal$Discharge ~ obs_cal$Date), col = "red")
summary(obs_cal_fit)

# Observed Validation Period

obs_val <- filter(obs_bf_removed,Date >= '2008-12-01')

plot(obs_val$Date, obs_val$Discharge, main = "Observed - Validation Period", xlab = x_lab, ylab = y_lab2, col = "blue", pch = 16)

obs_val_fit <- lm(obs_val$Discharge ~ obs_val$Date)
abline(lm(obs_val$Discharge ~ obs_val$Date), col = "red")
summary(obs_val_fit)
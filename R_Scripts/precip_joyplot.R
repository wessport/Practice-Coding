# WES PORTER
# 8/30/2017
# USDA PROJECT - Precipitation joyplot.

# Script Summary: Learning to use ggjoy to produce precipitation joyplots 
# for the current conditions simulation period.

library(ggplot2)
library(ggjoy)
library(lubridate)
library(hrbrthemes)
library(viridis)

# Read in data
in_loc <- file.path("E:","Wes","Work","USDA","raw","Scripts","ms_climate_secondary","R_Sec_Climate","R_input")

weather <- read.csv(file.path(in_loc,"Coahoma_v1.csv"),stringsAsFactors = FALSE)
weather$DATE <- as.Date(as.character(weather$DATE),'%Y%m%d')
weather <- filter(weather, Station_ID == 2 & DATE >= "2002-10-02") # Clarksdale
weather[weather == -9999] <- NA

# scales
mins <- min(weather$TMIN_C, na.rm = T)
maxs <- max(weather$TMAX_C, na.rm = T)

# precip

weather$TMEAN_C <- rowMeans(subset(weather, select = c(TMAX_C, TMIN_C)), na.rm = T)
weather[is.na(weather)] <-  NA

weather$year <- format(as.Date(weather$DATE, format="%Y-%m-%Y"),"%Y")
b <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)

ggplot(weather, aes(x = TMEAN_C, y = year)) + 
  geom_joy_gradient(scale=3, gradient_lwd = 1.)+
  scale_x_continuous(limits = c(mins,maxs)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(, option = "C") +
  theme_ipsum(grid=F)+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1)) +
  labs(title='Temperatures in Clarksdale MS',
     subtitle='Mean temperatures (C) by  year \nData: NOAA Global Historical Climate Network Daily')

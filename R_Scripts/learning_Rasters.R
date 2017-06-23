# WES PORTER
# 6/23/2017
# Learning the rasters package in R

# Load the raster, sp, and rgdal packages
library(raster)
library(sp)
library(rgdal)

# Set working directory to data folder
setwd("C:/Users/wsp2sgis/Documents/R/R_gdal");

img <- raster("LT50230362005193.B3.tif");

img <- setMinMax(img);

img

# View coordinate reference system
img@crs

# The distribution of values in the raster
hist(img, main="Distribution of pixel values", 
     col= "purple", 
     maxpixels=22000000)
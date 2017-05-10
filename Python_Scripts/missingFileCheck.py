# Script Purpose: Check two sets of data and identify unique values
# Date: 5/10/2017
# Author: Wes Porter

# Python executible location
# C:\Python27\ArcGISx6410.3\python.exe missingFileCheck.py

# CSV containing SENSOR, PATH, ROW, YEAR, DOY of downloaded NDVI images
dl = "D:/Wes/Work/USDA/raw/NDVI_Inventory/ND/downloaded.csv"

# CSV containing SENSOR, PATH, ROW, YEAR, DOY of all possible NDVI images
ps = "D:/Wes/Work/USDA/raw/NDVI_Inventory/ND/possible.csv"

# Want to check and see what images we need to download

# Read in the data
inFile1 = open(dl,'r')
inFile2 = open(ps,'r')

dl_row = []
for i in inFile1:
    dl_row.append(i)
inFile1.close()

ps_row = []
for i in inFile2:
    ps_row.append(i)
inFile2.close()

# Initialize empty list to contain missing images
missing = []
for i in ps_row:
    if(i not in dl_row): # checks if 'i' is in our list of downloaded images
        missing.append(i)

# Name and location of CSV file to generate with our missing image names
ms = "D:/Wes/Work/USDA/raw/NDVI_Inventory/ND/missing.csv"

# Write out the data
outFile = open(ms,'w')

for i in missing:
    outFile.write(i)
outFile.close()

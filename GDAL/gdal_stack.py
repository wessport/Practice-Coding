# WES PORTER
# 7/20/2017
# USDA PROJECT - gdal_stack.py

# SCRIPT SUMMARY:
# Working with python to run GDAL to stack a list of NDVI images

import os
import glob
import subprocess

ws = "E:/Wes/Work/USDA/raw/Mississippi/MS_NDVI/MS_Cloudfree_Null"

years = ["1985","1990","1995"]
for year in years:
    test_vrt= ["gdalbuildvrt", "-separate", ws+"/NDVI_stack.vrt"]
    image_list =  glob.glob(ws+"/LT5023{}*_msc_null_warp.tif".format(year))

    for i in image_list:
        test_vrt.append(i)
        subprocess.call(test_vrt)

    img2tiff =["gdal_translate",ws+"/NDVI_stack.vrt", ws+"/NDVI_stack{}.tif".format(year)]
    subprocess.call(img2tiff)
    fixNoData = ["gdalwarp","-dstnodata","-9999",ws+"/NDVI_stack{}.tif".format(year),ws+"/NDVI_stack{}_warp.tif".format(year)]
    subprocess.call(fixNoData)
    os.remove(ws+"/NDVI_stack.vrt")

print("\n ~~~FINI~~~ \n")

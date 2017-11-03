:: WES PORTER
:: 10/13/2017
:: USDA PROJECT
:: Summary: Relocating ESPA images into new subdirectories.
echo off
title THE GREAT MIGRATION OF ESPA IMAGES
echo.
echo Move ndvi files
MOVE *sr_ndvi.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR_NDVI
echo Move top of atmosphere files
MOVE *toa_band1.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/TOA
MOVE *toa_band2.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/TOA
MOVE *toa_band3.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/TOA
MOVE *toa_band4.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/TOA
MOVE *toa_band5.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/TOA
MOVE *toa_band7.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/TOA
echo Move pixel quality assurance files
MOVE *VER.txt //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/PIXEL_QA
MOVE *VER.jpg //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/PIXEL_QA
MOVE *pixel_qa.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/PIXEL_QA
MOVE *radsat_qa.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/PIXEL_QA
MOVE *azimuth_band4.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/PIXEL_QA
MOVE *zenith_band4.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/PIXEL_QA
echo Move surface reflectance files
MOVE *ANG.txt //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *MTL.txt //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *GCP.txt //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *.xml //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *bqa.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b1.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b2.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b3.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b4.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b5.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b6.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
MOVE *b7.tif //161.45.156.61/Public/Joel/Landsat5_images/Mississippi/unzipped_folder/SR
echo FILE MIGRATION COMPLETE.
Pause

// Wes Porter
// 10/3/2017
// Using Alps NDVI example to better understand GEE NDVI
// Modified for MS

// Source: https://gis.stackexchange.com/questions/241822/mask-clouds-in-landsat-8-surface-refletance-image

// single image from 1995
var lt5 = ee.Image('LANDSAT/LT5_SR/LT50230371995246')

var lt5_ndvi = lt5
  .normalizedDifference(['B2', 'B3'])  // calculate NDVI
  .updateMask(lt5.select(['cfmask']).neq(4))  // mask everything labeled 'cloud'

// Map.centerObject(lt5, 8)
// Map.addLayer(lt5)
//Map.centerObject(lt5_ndvi, 8)
Map.addLayer(lt5_ndvi, {}, "LT50230371995246 NDVI cloud masked", 0)

// image collection
var lt5_ic = ee.ImageCollection('LANDSAT/LT5_SR')
  .filterBounds(aoi)  // filter to area-of-interest
  .filterDate(ee.Date("1995-01-01"),ee.Date("1995-12-31"))  // filter to 1995

var lt5_ndvi_ic = lt5_ic
  .map(function(img){  // add cloud free NDVI to all images
  return img.addBands(img.normalizedDifference(['B2', 'B3'])).updateMask(img.select(['cfmask']).neq(4))
  });

print(lt5_ndvi_ic)

// define testPoint for NDVI mean
//var testPoint = ee.Geometry.Point([-90.84749221801758, 33.71291698851023]);
var testPoint = geometry;

Map.centerObject(testPoint, 10)
var ndvi_viz = {min:-0.8, max:0.3, palette:'000000,00FF00'};
Map.addLayer(lt5_ndvi_ic.select('nd').mean(), ndvi_viz, "LT5 1995 NDVI mean")

// NDVI time-series chart
// Create and print the chart.
print(ui.Chart.image.series(lt5_ndvi_ic.select('nd'), testPoint, ee.Reducer.mean(), 30));

// Google Earth Engine Cloud Mask Example

// Load a cloudy Landsat scene and display it.
var cloudy_scene = ee.Image('LANDSAT/LC8_L1T_TOA/LC80440342014269LGN00');
Map.centerObject(cloudy_scene);
Map.addLayer(cloudy_scene, {bands: ['B4', 'B3', 'B2'], max: 0.4}, 'TOA', false);

// Add a cloud score band.  It is automatically called 'cloud'.
var scored = ee.Algorithms.Landsat.simpleCloudScore(cloudy_scene);

// Create a mask from the cloud score and combine it with the image mask.
var mask = scored.select(['cloud']).lte(20);

// Apply the mask to the image and display the result.
var masked = cloudy_scene.updateMask(mask);
Map.addLayer(masked, {bands: ['B4', 'B3', 'B2'], max: 0.4}, 'masked');


// EXAMPLE 2

// This is a test script for learning purposes

var intDate = new Date("06/01/2005");
var endDate = new Date("06/31/2005");

var MS =  [
                [-91.0382080078125, 33.84988869610126], // Test Coordinates Rosedale-Merigold-12-Avon MS
                [-90.72509765625, 33.8339199536547],
                [-90.72509765625, 33.23639027157906],
                [-91.05194091796875, 33.224903086263964]
              ];

var landsat = ee.ImageCollection('LANDSAT/LT5_SR').filterBounds(ee.Geometry.Polygon(MS));
var dates = landsat.filterDate(intDate, endDate);
var scene = ee.Image(dates.first());

// select cfmask band as mask
var msk = scene.select('cfmask');

//conditions which to mask out - no shadows, snow or clouds
msk = msk.neq(2).and(msk.neq(3)).and(msk.neq(4));

// apply mask
var masked = scene.mask(msk)

// add masked image to Layer
Map.addLayer(masked,{min:0,max:3000,bands:['B3','B2','B1']},'masked');

// var median = dates.median();

// Map.addLayer(median, {'bands': ['B4', 'B3', 'B2'], 'min': 5000, 'max': 18000});
// Map.addLayer(dates, {'bands': ['sr_cloud_qa']});
Map.addLayer(masked);
// Map.setCenter(-119.84, 37.83, 8);


//var geometry = /* color: #d63000 */ee.Feature(
//        ee.Geometry.Polygon(
//            [[[-121.2615966796875, 36.6992553955527],
//              [-119.937744140625, 36.69485094156225],
//              [-119.9212646484375, 37.61423141542417],
//              [-121.2835693359375, 37.63598495426961]]]),
//        {
//          "system:index": "0"
//        });

print(endDate)

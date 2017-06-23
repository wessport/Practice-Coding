// Evan's Google Earth Engine Script modified to remove clouds

// Map.setCenter(-91.0382, 33.8498, 8);

function getNDVIimages(area, collection_name, redBand, nirBand){
  var polygon = ee.Geometry.Polygon(area);
  Map.centerObject(polygon);
  function computeAndPrint (image) {
    var img = ee.Image.load(image.id, image.version);//.set('SENSOR_ID', 'TM');
    //The sensor IDs of Landsat 5, 7 and 8 are 'TM', 'ETM+' and 'OLI_TIRS', respectively.

    // select cfmask band as mask
    var msk = img.select('cfmask');

    //conditions which to mask out - no shadows, snow or clouds
    msk = msk.neq(2).and(msk.neq(3)).and(msk.neq(4));

    // apply mask
    var masked = img.mask(msk);

    // Calculate NDVI for masked image
    var ndvi = masked.expression(
      '(nir - red) / (nir + red)',
      {
          red: masked.select(redBand),
          nir: masked.select(nirBand)
      });
    print(ndvi.getDownloadURL({ name: image.properties["system:index"]}));
  }
  var collection = ee.ImageCollection(collection_name);

  var list = collection.filterDate(new Date('07/01/2005'), new Date('07/31/2005')).filterBounds(ee.Geometry.Polygon(area)).toList(collection.size());
  print(list)
  for (var i = 0; i < 5; i++) {

    list.get(i).evaluate(computeAndPrint)
  }
}

var region =  [
                [-91.0382080078125, 33.84988869610126], // Test Coordinates Rosedale-Merigold-12-Avon MS
                [-90.72509765625, 33.8339199536547],
                [-90.72509765625, 33.23639027157906],
                [-91.05194091796875, 33.224903086263964]
              ];

var regionND = []

var region2 =  [
                [-90.956962, 34.40944], //Coordinates of the study area in MS
                [-90.232902, 34.413464],
                [-90.26126, 33.40915],
                [-90.958858, 33.432743]
            ];
getNDVIimages(region, 'LANDSAT/LT5_SR', 'B2', 'B3' );

//LANDSAT/LC8_SR   Landsat 8                                B4, B5    Data availability (time)  Apr 11, 2013 - Nov 1, 2015
//LANDSAT/LE7_SR  Landsat 7    --LEDAPS/LE7_L1T_SR          B3, B4    Data availability (time)  Jan 1, 1999 - Mar 24, 2017
//LANDSAT/LT5_SR  Landsat 5    --LEDAPS/LT5_L1T_SR          B2, B3    Data availability (time)  Jan 1, 1984 - May 5, 2012
//LANDSAT/LT4_SR  Landsat 4                                 B2, B3    Data availability (time)  Aug 22, 1982 - Dec 14, 1993

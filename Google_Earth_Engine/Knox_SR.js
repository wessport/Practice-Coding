Map.setCenter(-83.93, 35.99, 9);

function getNDVIimages(area, collection_name){
  function computeAndPrint (image) {
    var img = ee.Image.load(image.id, image.version);

    print(img.getDownloadURL({ name: image.properties["system:index"]}));
  }
  var collection = ee.ImageCollection(collection_name);

  var list = collection.filterDate(new Date('06/01/2015'), new Date('06/30/2015')).filterBounds(ee.Geometry.Polygon(area)).toList(collection.size());
  print(list)
  for (var i = 0; i < 5; i++) {

    list.get(i).evaluate(computeAndPrint)
  }
}

var boundary = [
                  [-83.98704528808594,35.897367892271085], // Knoxville
                  [-83.78448486328125,35.894030442222785],
                  [-83.78517150878906,35.993814939482796],
                  [-83.99425506591797,35.992426048629845]
              ];

getNDVIimages(boundary, 'LANDSAT/LC8_SR');

//LANDSAT/LC8_SR   Landsat 8  Data availability (time)  Apr 11, 2013 - Nov 1, 2015

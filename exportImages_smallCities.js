//AUTHOR: Michelle Stuhlmacher

//GOAL: 
//1. Export the 75th percentile of NDVI for each CCA bounding circle
//2. Export the population image for each CCA bounding circle
//3. Export a table with the radii values for each city
//...for the cities that are too small to use the buffer loop. These cities are:
//NH: Belgaum (India), Holguin (Cuba), Kabul (Afghanistan), Myeik (Myanmar), Pokhara (Nepal), Gorgan (Iran), Sitapur (India)
//SH: Ilheus (Brazil), Beira (Mozambique)

//IMPORTS:
var GFC = ee.Image("UMD/hansen/global_forest_change_2020_v1_8"),
    cityCenters = ee.FeatureCollection("users/mfstuhlmacher/UrbanGreenspace/CityCenters"),
    popGHSL = ee.ImageCollection("JRC/GHSL/P2016/POP_GPW_GLOBE_V1"),
    GHSL = ee.Image("JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1"),
    Belgaum = 
    /* color: #d63000 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[74.4643060263641, 15.92494004286194],
          [74.4643060263641, 15.806217305958494],
          [74.56369796361996, 15.806217305958494],
          [74.56369796361996, 15.92494004286194]]], null, false),
    Holguin = 
    /* color: #98ff00 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-76.31116157372612, 20.917248255388124],
          [-76.31116157372612, 20.838175679994777],
          [-76.19666343529839, 20.838175679994777],
          [-76.19666343529839, 20.917248255388124]]], null, false),
    Beira = 
    /* color: #0b4a8b */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[34.8043368568273, -19.718120060116036],
          [34.8043368568273, -19.853802810184902],
          [34.92106659315542, -19.853802810184902],
          [34.92106659315542, -19.718120060116036]]], null, false),
    Ilheus = 
    /* color: #ffc82d */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-39.08517028537302, -14.732973484071415],
          [-39.08517028537302, -14.856787189807829],
          [-39.01976730075388, -14.856787189807829],
          [-39.01976730075388, -14.732973484071415]]], null, false),
    Kabul = 
    /* color: #00ffff */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[69.04782236703358, 34.5944221594483],
          [69.04782236703358, 34.44648026511144],
          [69.29879130013904, 34.44648026511144],
          [69.29879130013904, 34.5944221594483]]], null, false),
    Myeik = 
    /* color: #bf04c2 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[98.59257753977293, 12.467769586274375],
          [98.59257753977293, 12.412116040295171],
          [98.62416323313231, 12.412116040295171],
          [98.62416323313231, 12.467769586274375]]], null, false),
    Pokhara = 
    /* color: #ff0000 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[83.96292932716112, 28.25146058752407],
          [83.96292932716112, 28.187629681483628],
          [84.0168309995244, 28.187629681483628],
          [84.0168309995244, 28.25146058752407]]], null, false),
    Gorgan = 
    /* color: #00ff00 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[54.374686816958246, 36.8657979963775],
          [54.374686816958246, 36.80782002934877],
          [54.48798332574731, 36.80782002934877],
          [54.48798332574731, 36.8657979963775]]], null, false),
    Sitapur = 
    /* color: #0000ff */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[80.64688833826786, 27.588136033591844],
          [80.64688833826786, 27.54157027626473],
          [80.71108969324833, 27.54157027626473],
          [80.71108969324833, 27.588136033591844]]], null, false),
    L8 = ee.ImageCollection("LANDSAT/LC08/C02/T1");

//Functions for later in the code (remove geometry column for export)
var removeGeo = (function(feature){
  feature = feature.setGeometry(null);
return (feature)});

//Add 2015 built layer to visualization to hand-draw boundaries
var y2015 = GHSL.select('built').gte(3);
Map.addLayer(y2015.selfMask(),{},'built 2015',false);

//Hand-draw boundaries based on 2015 built visualization, combine into a feature collection
Map.addLayer(cityCenters,{},'city center', false);

var features = [
  ee.Feature(Beira, {TARGET_FID: 13, City: 'Beira'}),
  ee.Feature(Belgaum, {TARGET_FID: 14, City: 'Belgaum'}), 
  ee.Feature(Holguin, {TARGET_FID: 50, City: 'Holguin'}), 
  ee.Feature(Ilheus, {TARGET_FID: 55, City: 'Ilheus'}), 
  ee.Feature(Kabul, {TARGET_FID: 63, City: 'Kabul'}), 
  ee.Feature(Myeik, {TARGET_FID: 93, City: 'Myeik'}), //?
  ee.Feature(Pokhara, {TARGET_FID: 109, City: 'Pokhara'}), 
  ee.Feature(Gorgan, {TARGET_FID: 43, City: 'Gorgan'}),
  ee.Feature(Sitapur, {TARGET_FID: 130, City: 'Sitapur'})
  ];
var handExport = ee.FeatureCollection(features);
 
//--GOAL 1: NDVI--//
//Set up date filters for northern and southern hemisphere summers
var yearRange = ee.DateRange('2014-01-01','2016-12-31');
var filterNH = ee.Filter.dayOfYear(172,266);

//Dec 22 (winter solstice) and ends March 20 (spring equinox);
var filterSH = ee.Filter.dayOfYear(356,79); 

//Create a water mask
var water = GFC.select(['datamask']).eq(2);
var waterMask = water.not();

//--GOAL 2: Population--//
var pop2015 = popGHSL.filter(ee.Filter.date('2015-01-01', '2015-12-31'));
var populationCount = pop2015.select('population_count').mosaic();
var populationCountVis = {
  min: 0.0,
  max: 200.0,
  palette: ['060606', '337663', '337663', 'ffffff'],
};
Map.addLayer(populationCount, populationCountVis, 'Population Count',false);

// //--Loop for Export--//
// // INSTRUCTIONS: Toggle comments on or off based on whether you want to export northern or southern hemisphere cities.
// // Northern Hemisphere Loop (small cities)
// var cityID = ['Belgaum','Holguin','Kabul','Myeik','Pokhara','Gorgan','Sitapur'];

// for (var jj = 0; jj < cityID.length; ++jj) {
//   var i = cityID[jj];
//   var filterOneCity = ee.Filter.eq('City',i);
//   var cityBounds = handExport.filter(filterOneCity);
  
//   //--Set up imagery and calculate NDVI--//
//   //Make a cloudless composite of 2015 imagery
//   var L8_comp_NH = ee.Algorithms.Landsat.simpleComposite({
//     collection: L8.filterDate(yearRange).filter(filterNH).filterBounds(cityBounds),
//     cloudScoreRange: 3,
//     maxDepth: 100,
//     asFloat: true
//   });
  
//   //Mask out water
//   var L8_noWater = L8_comp_NH.mask(waterMask); 
//   Map.addLayer(L8_comp_NH,{bands: ['B4','B3','B2']},'L8 composite',false);

//   //Calculate NDVI
//   var ndvi = L8_noWater.normalizedDifference(['B5', 'B4']).rename('NDVI');
//   //Map.addLayer(ndvi,{},'NDVI contin',false);
  
//   //Determine 75th percentile threshold
//   var pctNDVI = ndvi.reduceRegion({
//     reducer: ee.Reducer.percentile([75]), 
//     geometry: cityBounds,                 
//     scale: 30,
//     maxPixels: 1e9
//   }).get('NDVI');
//   //print(ee.Number(pctNDVI));

//   //Threshold based on percentile value
//   var threshNDVI = ndvi.gte(ee.Number(pctNDVI));
//   //Map.addLayer(threshNDVI,{},'NDVI ' + i, false);
  
//   //Create export circle based on hand-drawn geometry
//   var filterOneCityCenter = cityCenters.filter(filterOneCity).geometry();
//   var perimeter = cityBounds.geometry().perimeter(0.5);
//   var radii = perimeter.divide(5);
//   var circleBuffer = filterOneCityCenter.buffer(radii);
//   Map.addLayer(circleBuffer,{},'buffer, ' + i);
  
//   //Export NDVI to Drive
//   Export.image.toDrive({
//     image: threshNDVI,
//     description: "CCA_NDVI_" + i + "_WGS84m_20230316",
//     folder: "CCAExport_GlobalGreenspace",
//     region: circleBuffer,
//     maxPixels: 10e9,
//     scale: 30,
//     //crs: "EPSG:4326"
//     crs: "EPSG:3395" //WGS 84 / World Mercator
// });

//   //Export GHSL Population to Drive
//   Export.image.toDrive({
//     image: populationCount,
//     description: "Pop_BoundingCircle_" + i + "_WGS84m_20230316",
//     folder: "CCAExport_GlobalGreenspace",
//     region: circleBuffer,
//     maxPixels: 10e9,
//     scale: 38,
//     //crs: "EPSG:4326"
//     crs: "EPSG:3395" //WGS 84 / World Mercator
// });

//   //--Add radius distance, pop sum, NDVI thresh, and green area sum to export table--//
//   //Pop sum
//   var clipPop = populationCount.clip(circleBuffer);
//   var popSum = clipPop.reduceRegion({
//     reducer: ee.Reducer.sum(),
//     scale: 250,
//     maxPixels: 1e9,
//     geometry: circleBuffer,
//     tileScale: 10
//   });
  
//   //Green area sum
//   var NDVI_mask = threshNDVI.selfMask();
//   var NDVI_area = NDVI_mask.multiply(ee.Image.pixelArea());
//   var NDVISum = NDVI_area.reduceRegion({
//     reducer:ee.Reducer.sum(),
//     geometry: circleBuffer,
//     scale: 30,
//     maxPixels: 1e9,
//     tileScale: 10
//   });
  
//   //Add properties
//   var properties = cityBounds.first()
//   .set({'radius': radii})
//   .set({'pop2015': popSum.get('population_count')})
//   .set({'NDVIthresh': ee.Number(pctNDVI)})
//   .set({'greenArea': NDVISum.get('NDVI')
//   });
//   //Conver to Feature Collection (required for export)
//   var row = ee.FeatureCollection([properties]);
  
//   //Map to remove geometry
//   var exportRow = row.map(removeGeo);
//   //print(exportRow);

//   //Export properties (will need to combine individual rows with R)
//   Export.table.toDrive({
//     collection: exportRow,
//     description: 'boundingCircleVaules_' + i + '20230316',
//     folder: "CCATableExport_GlobalGreenspace"
//   });
// }

//--Southern Hemisphere Loop--//
// //Full city 
var cityID = ['Ilheus','Beira'];

for (var jj = 0; jj < cityID.length; ++jj) {
  var i = cityID[jj];
  var filterOneCity = ee.Filter.eq('City',i);
  var cityBounds = handExport.filter(filterOneCity);
  
  //--Set up imagery and calculate NDVI--//
  //Make a cloudless composite of 2015 imagery
  var L8_comp_SH = ee.Algorithms.Landsat.simpleComposite({
    collection: L8.filterDate(yearRange).filter(filterSH),//.filterBounds(cityBounds),
    cloudScoreRange: 3,
    maxDepth: 100,
    asFloat: true
  });
  
  //Mask out water
  var L8_noWater = L8_comp_SH.mask(waterMask); 

  //Calculate NDVI
  var ndvi = L8_noWater.normalizedDifference(['B5', 'B4']).rename('NDVI');
  //Map.addLayer(ndvi,{},'NDVI contin',false);
  
  //Determine 75th percentile threshold
  var pctNDVI = ndvi.reduceRegion({
    reducer: ee.Reducer.percentile([75]), 
    geometry: cityBounds,                 
    scale: 30,
    maxPixels: 1e9
  }).get('NDVI');
  //print(ee.Number(pctNDVI));

  //Threshold based on percentile value
  var threshNDVI = ndvi.gte(ee.Number(pctNDVI));
  //Map.addLayer(threshNDVI,{},'NDVI ' + i, false);
  
  //Create export circle based on hand-drawn geometry
  var filterOneCityCenter = cityCenters.filter(filterOneCity).geometry();
  var perimeter = cityBounds.geometry().perimeter(0.5);
  var radii = perimeter.divide(5);
  var circleBuffer = filterOneCityCenter.buffer(radii);
  Map.addLayer(circleBuffer,{},'buffer, ' + i);
  
  //Export NDVI to Drive
  Export.image.toDrive({
    image: threshNDVI,
    description: "CCA_NDVI_" + i + "_WGS84m_20230316",
    folder: "CCAExport_GlobalGreenspace",
    region: circleBuffer,
    maxPixels: 10e9,
    scale: 30,
    //crs: "EPSG:4326"
    crs: "EPSG:3395" //WGS 84 / World Mercator
});

  //Export GHSL Population to Drive
  Export.image.toDrive({
    image: populationCount,
    description: "Pop_BoundingCircle_" + i + "_WGS84m_20230316",
    folder: "CCAExport_GlobalGreenspace",
    region: circleBuffer,
    maxPixels: 10e9,
    scale: 38,
    //crs: "EPSG:4326"
    crs: "EPSG:3395" //WGS 84 / World Mercator
});

  //--Add radius distance, pop sum, NDVI thresh, and green area sum to export table--//
  //Pop sum
  var clipPop = populationCount.clip(circleBuffer);
  var popSum = clipPop.reduceRegion({
    reducer: ee.Reducer.sum(),
    scale: 250,
    maxPixels: 1e9,
    geometry: circleBuffer,
    tileScale: 10
  });
  
  //Green area sum
  var NDVI_mask = threshNDVI.selfMask();
  var NDVI_area = NDVI_mask.multiply(ee.Image.pixelArea());
  var NDVISum = NDVI_area.reduceRegion({
    reducer:ee.Reducer.sum(),
    geometry: circleBuffer,
    scale: 30,
    maxPixels: 1e9,
    tileScale: 10
  });
  
  //Add properties
  var properties = cityBounds.first()
  .set({'radius': radii})
  .set({'pop2015': popSum.get('population_count')})
  .set({'NDVIthresh': ee.Number(pctNDVI)})
  .set({'greenArea': NDVISum.get('NDVI')
  });
  //Conver to Feature Collection (required for export)
  var row = ee.FeatureCollection([properties]);
  
  //Map to remove geometry
  var exportRow = row.map(removeGeo);
  //print(exportRow);

  //Export properties (will need to combine individual rows with R)
  Export.table.toDrive({
    collection: exportRow,
    description: 'boundingCircleVaules_' + i + '20230316',
    folder: "CCATableExport_GlobalGreenspace"
  });
}


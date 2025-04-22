//AUTHOR: Michelle Stuhlmacher

//GOAL: 
//1. Export the 75th percentile of NDVI for each CCA bounding circle
//2. Export the population image for each CCA bounding circle
//3. Export a table with the radii values for each city

//IMPORTS:
var GFC = ee.Image("UMD/hansen/global_forest_change_2020_v1_8"),
    cityCenters = ee.FeatureCollection("users/mfstuhlmacher/UrbanGreenspace/CityCenters"),
    cityImages = ee.ImageCollection("users/mfstuhlmacher/UrbanGreenspace/CCA/CCAImageCollection"),
    popGHSL = ee.ImageCollection("JRC/GHSL/P2016/POP_GPW_GLOBE_V1"),
    exportCCA = ee.FeatureCollection("users/mfstuhlmacher/UrbanGreenspace/CCABounds3"),
    L8 = ee.ImageCollection("LANDSAT/LC08/C02/T1");

//Functions for later in the code (remove geometry column for export)
var removeGeo = (function(feature){
  feature = feature.setGeometry(null);
return (feature)});
 
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

Map.addLayer(exportCCA,{},'CCA export bounds', false);

//--Loop for Export--//
// INSTRUCTIONS: Toggle comments on or off based on whether you want to export northern or southern hemisphere cities.
// Northern Hemisphere Loop
//All cities:
// var cityID = ['Accra','Ahmedabad','Ahvaz','Alexandria','Algiers','Astrakhan','Baghdad','Baku','Bamako','Belgrade',
// 'Berlin','Budapest','Busan','Cabimas','Cairo','Caracas','CebuCity','Cheonan','Cleveland','Coimbatore','Chicago',
// 'Culiacan','Dhaka','Fukuoka','Gainesville','Gombe','Gomel','Gorgan','Guadalajara','GuatemalaCity','Hindupur','Houston',
// 'Hyderabad','Ibadan','Istanbul','Jaipur','Jalna','Jinju','Kanpur','Karachi','Kaunas','Kayseri','Khartoum','Killeen',
// 'Kolkata','Kozhikode','Lagos','Lahore','LeMans','London','LosAngeles','Malatya','Malegaon','Manila','Marrakesh','Medan',
// 'MexicoCity','Milan','Minneapolis','Modesto','Montreal','Moscow','Mumbai','Nikolaev','Okayama','Oldenburg','Kyoto','Oyo',
// 'Palermo','Parbhani','Paris','Pematangtiantar','NewYork','Portland','Pyongyang','Raleigh','KualaLumpur','Reynosa','Riyadh',
// 'Rovno','Saidpur','SaintPetersburg','SanSalvador','Sana','Seoul','Sheffield','Shymkent','Sialkot','Singapore','Sitapur',
// 'Taipei','Tashkent','Tebessa','TelAviv','Tijuana','Tokyo','Toledo','Ulaanbaatar','Victoria','Vienna','Vijayawada','Warsaw',
// 'Yamaguchi','Zwolle','Anqing','Beijing','Changzhi','Changzhou','Guangzhou','Hangzhou','HongKong','Jinan','Kaiping',
// 'Pingxiang','Tangshan','Tianjin','Wuhan','Xingping','Yiyang','Zhuji'];

//Cities that run without special changes to code
// var cityID = ['Accra','Ahmedabad','Ahvaz','Alexandria','Algiers','Astrakhan','Baghdad','Baku','Bamako','Belgrade',
// 'Berlin','Budapest','Busan','Cabimas','Cairo','Caracas','CebuCity','Cheonan','Cleveland','Coimbatore','Chicago',
// 'Culiacan','Dhaka','Fukuoka','Gainesville','Gombe','Gomel','Gorgan','Guadalajara','GuatemalaCity','Hindupur','Houston',
// 'Hyderabad','Ibadan','Istanbul','Jaipur','Jalna','Jinju','Kanpur','Karachi','Kaunas','Kayseri','Khartoum','Killeen',
// 'Kolkata','Kozhikode','Lagos','Lahore','LeMans','London','LosAngeles','Malatya','Malegaon','Manila','Marrakesh','Medan',
// 'MexicoCity','Milan','Minneapolis','Modesto','Montreal','Moscow','Mumbai','Nikolaev','Okayama','Oldenburg','Kyoto','Oyo',
// 'Palermo','Parbhani','Paris','Pematangtiantar','Portland','Pyongyang','Raleigh','KualaLumpur','Reynosa','Riyadh',
// 'Rovno','Saidpur','SaintPetersburg','SanSalvador','Sana','Seoul','Sheffield','Shymkent','Sialkot','Singapore','Sitapur',
// 'Taipei','Tashkent','Tebessa','TelAviv','Tijuana','Toledo','Ulaanbaatar','Victoria','Vienna','Vijayawada','Warsaw',
// 'Yamaguchi','Zwolle','Anqing','Beijing','Changzhi','Changzhou','Guangzhou','Hangzhou','HongKong','Jinan','Kaiping',
// 'Pingxiang','Tangshan','Tianjin','Wuhan','Xingping','Yiyang','Zhuji'];

var cityID = ['Guangzhou']; 

//Remaining cities: New York, Tokyo (need to modify code slightly to get them to run)

for (var jj = 0; jj < cityID.length; ++jj) {
  var i = cityID[jj];
  var filterOneCity = ee.Filter.eq('City',i);
  var cityBounds = exportCCA.filter(filterOneCity);
  
  //--Set up imagery and calculate NDVI--//
  //Make a cloudless composite of 2015 imagery
  var L8_comp_NH = ee.Algorithms.Landsat.simpleComposite({
    collection: L8.filterDate(yearRange).filter(filterNH),//.filterBounds(cityBounds),
    cloudScoreRange: 3,
    maxDepth: 100,
    asFloat: true
  });
  
  
  //Mask out water
  var L8_noWater = L8_comp_NH.mask(waterMask); 
  Map.addLayer(L8_comp_NH,{},'L8 composite',false);

  //Calculate NDVI
  var ndvi = L8_noWater.normalizedDifference(['B5', 'B4']).rename('NDVI');
  //Map.addLayer(ndvi,{},'NDVI contin',false);
  
  //Import CCA
  var cityImagesM = cityImages.select('labels').mosaic().clip(cityBounds);
  
  //Create a vector from the CCA and use this to bound the NDVI percentile threshold
  Map.addLayer(cityImagesM.randomVisualizer(),{},'CCA image, ' + i,false);
  
  //Select the most common value
  var mode = cityImagesM.reduceRegion({
    reducer: ee.Reducer.mode(),
    geometry: cityBounds,
    scale: 2445.98
    //scale: cityImagesM.projection().nominalScale() //need to do this to get Tokyo to run
  });
  var thresh = ee.Number(mode.get('labels')).round();
  //var thresh = ee.Number(mode.get('labels')).round().add(1); //need to add 1 based on decimal rounding for NYC
  //print(thresh);

  //Select only the city pixels
  var cityPixels = cityImages.mosaic().select('labels').eq(ee.Number(thresh)).clip(cityBounds).selfMask();
  Map.addLayer(cityPixels,{},'city pixels, ' + i,false);

  //Covert CCA pixels to a vector boundary
  var cityVector = cityPixels.reduceToVectors({
    reducer: ee.Reducer.countEvery(),
    geometry: cityBounds,
    scale: 2445.98,
    geometryType: "polygon"
  });
  Map.addLayer(cityVector,{},'city vector',false);
  
  var vectorBounds = cityVector.geometry().bounds();
  //Map.addLayer(vectorBounds,{},'city bounds');
  
  //Determine 75th percentile threshold
  var pctNDVI = ndvi.reduceRegion({
    reducer: ee.Reducer.percentile([75]), 
    geometry: cityVector,                 
    scale: 30,
    maxPixels: 1e9
  }).get('NDVI');
  //print(ee.Number(pctNDVI));

  //Threshold based on percentile value
  var threshNDVI = ndvi.gte(ee.Number(pctNDVI));
  //Map.addLayer(threshNDVI,{},'NDVI ' + i, false);
  
  //--Determine radius for export--//
  var filterOneCityCenter = cityCenters.filter(filterOneCity).geometry(); //select city center
  var pointRaster = ee.Image().toByte().paint(filterOneCityCenter, 1);   //convert city center point to raster
  
  //Create a cost image to find the farthest away pixel
  var empty = ee.Image(1).clip(vectorBounds);  
  Map.addLayer(empty,{},'blank image for cost vector, ' + i, false);
  var costImage = empty.cumulativeCost(pointRaster, 500000);
  Map.addLayer(costImage,{},'cost image, ' + i, false);
  //Map.addLayer(costImage.clip(cityVector),{},'cost image, ' + i, false);
  Map.addLayer(filterOneCityCenter,{},'city center, ' + i,false);
  //print(filterOneCityCenter);
  
  var maxDistance = costImage.clip(cityVector).select('cumulative_cost').reduceRegion({
    reducer: ee.Reducer.max(),
    geometry: cityVector,
    scale: 1000 
  });
  var distance = maxDistance.get('cumulative_cost');
  var circleBuffer = filterOneCityCenter.buffer(distance);
  Map.addLayer(circleBuffer,{},'circle buffer, ' + i, false);
  
  //Export NDVI to Drive
  Export.image.toDrive({
    image: threshNDVI,
    description: "CCA_NDVI_" + i + "_WGS84m_20230321",
    folder: "CCAExport_GlobalGreenspace",
    region: circleBuffer,
    maxPixels: 10e9,
    scale: 30,
    //crs: "EPSG:4326"
    crs: "EPSG:3395" //WGS 84 / World Mercator
});

  // //Export GHSL Population to Drive
  // Export.image.toDrive({
  //   image: populationCount,
  //   description: "Pop_BoundingCircle_" + i + "_WGS84m_20230316",
  //   folder: "CCAExport_GlobalGreenspace",
  //   region: circleBuffer,
  //   maxPixels: 10e9,
  //   scale: 38,
  //   //crs: "EPSG:4326"
  //   crs: "EPSG:3395" //WGS 84 / World Mercator
// });

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
  .set({'radius': distance})
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
    description: 'boundingCircleVaules_' + i + '20230321',
    folder: "CCATableExport_GlobalGreenspace"
  });
}

// //--Southern Hemisphere Loop--//
// // //Full city list
// // var cityID = ['Auckland','Beira','BeloHorizonte','BuenosAires','Cirebon','Cochabamba','Cordoba','Curitiba','Florianopolis',
// // 'Jequie','Johannesburg','Kinshasa','Luanda','Lubumbashi','Nakuru','Ndola','Palembang','Palmas','Quito','RibeiraoPreto','Santiago',
// // 'SaoPaulo','Suva','Sydney'];

// //Excluding hand export:
// var cityID = ['Auckland','BeloHorizonte','BuenosAires','Cirebon','Cochabamba','Cordoba','Curitiba','Florianopolis',
// 'Jequie','Johannesburg','Kinshasa','Luanda','Lubumbashi','Nakuru','Ndola','Palembang','Palmas','Quito','RibeiraoPreto','Santiago',
// 'SaoPaulo','Suva','Sydney'];

// for (var jj = 0; jj < cityID.length; ++jj) {
//   var i = cityID[jj];
//   var filterOneCity = ee.Filter.eq('City',i);
//   var cityBounds = exportCCA.filter(filterOneCity);
  
//   //--Set up imagery and calculate NDVI--//
//   //Make a cloudless composite of 2015 imagery
//   var L8_comp_SH = ee.Algorithms.Landsat.simpleComposite({
//     collection: L8.filterDate(yearRange).filter(filterSH),//.filterBounds(cityBounds),
//     cloudScoreRange: 3,
//     maxDepth: 100,
//     asFloat: true
//   });
  
//   //Mask out water
//   var L8_noWater = L8_comp_SH.mask(waterMask); 

//   //Calculate NDVI
//   var ndvi = L8_noWater.normalizedDifference(['B5', 'B4']).rename('NDVI');
//   //Map.addLayer(ndvi,{},'NDVI contin',false);

//   //Create a vector from the CCA and use the CCA to bound the NDVI percentile threshold
//   //Select the most common value
//   Map.addLayer(cityImages.mosaic().clip(cityBounds).randomVisualizer(),{},'CCA image, ' + i, false);
//   var mode = cityImages.mosaic().reduceRegion({
//     reducer: ee.Reducer.mode(),
//     geometry: cityBounds,
//     scale: 2445.98 
//   });
//   var thresh = mode.get('labels');

//   //Select only the city pixels
//   var cityPixels = cityImages.mosaic().select('labels').eq(ee.Number(thresh)).clip(cityBounds).selfMask();
//   Map.addLayer(cityPixels,{},'city pixels, ' + i,false);

//   //Covert CCA pixels to a vector boundary
//   var cityVector = cityPixels.reduceToVectors({
//     reducer: ee.Reducer.countEvery(),
//     geometry: cityBounds,
//     scale: 2445.98,
//     geometryType: "polygon"
//   });
//   //Map.addLayer(cityVector,{},'city vector');
  
//   var vectorBounds = cityVector.geometry().bounds();
//   //Map.addLayer(vectorBounds,{},'city bounds');
//   //print(vectorBounds,"vector bounds");
  
//   //Determine 75th percentile threshold
//   var pctNDVI = ndvi.reduceRegion({
//     reducer: ee.Reducer.percentile([75]), //using the lowest threshold from the Goldblatt paper
//     geometry: cityVector,                 
//     scale: 30,
//     maxPixels: 1e9
//   }).get('NDVI');
//   //print(ee.Number(pctNDVI));

//   //Threshold based on percentile value
//   var threshNDVI = ndvi.gte(ee.Number(pctNDVI));
//   //Map.addLayer(threshNDVI,{},'NDVI ' + i, false);
//   //print(ee.Number(pctNDVI), 'NDVI percent, ' + i);
  
//   //--Determine radius for export--//
//   var filterOneCityCenter = cityCenters.filter(filterOneCity).geometry(); //select city center
//   var pointRaster = ee.Image().toByte().paint(filterOneCityCenter, 1);   //convert city center point to raster
  
//   //Create a cost image to find the farthest away pixel
//   var empty = ee.Image(1).clip(vectorBounds);
//   Map.addLayer(empty,{},'blank image for cost vector, ' + i, false);
//   var costImage = empty.cumulativeCost(pointRaster, 100000);
//   Map.addLayer(costImage.clip(cityVector),{},'cost image, ' + i, false);
//   Map.addLayer(filterOneCityCenter,{},'city center, ' + i, false);
  
//   var maxDistance = costImage.clip(cityVector).select('cumulative_cost').reduceRegion({
//     reducer: ee.Reducer.max(),
//     geometry: cityVector,
//     scale: 1000 
//   });
//   var distance = maxDistance.get('cumulative_cost');
//   var circleBuffer = filterOneCityCenter.buffer(distance);
//   Map.addLayer(circleBuffer,{},'circle buffer, ' + i, false);
  
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
//   .set({'radius': distance})
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

#AUTHOR: Michelle Stuhlmacher, PhD
#LAST UPDATED: 2023-03-27

#GOAL: Loop through imagery and calculate landscape metrics

#STEPS:
#1. Import data and libraries, set up DF
#2. Loop through buffer lengths and calculate metrics
#3. Export metric results

# STEP 1 -----------------------------------------------
#Import data and libraries
library(raster)
library(landscapemetrics)
library(dplyr)
library(rgeos)
library(rgdal)

#Set working directory
setwd("D:/GlobalUrbanGreenspace/CityImages_2023.03") #office server

#Image to match the projection of
projR = raster("D:/GlobalUrbanGreenspace/CityImages_2023.03/CCA_NDVI_Accra_WGS84m_20230316.tif")
crs(projR)

#City centers
cbd_shp = shapefile("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace/Data/CityCenters/cityCenter_2023.03.shp") 
#re-project cbd to match imagery
cbd = spTransform(cbd_shp, crs(projR))

greenImages = list.files(path = ".", pattern = "NDVI")

#Create empty DF to store landscape metric values
metricGreen_DF = data.frame(matrix(ncol = 5, nrow = 0))

outerBoundsG = data.frame(matrix(ncol = 17, nrow = 0))

# STEP 2 -----------------------------------------------
#Loop through buffer lengths and calculate metrics

# cityList = c('Accra','Ahmedabad','Ahvaz','Alexandria','Algiers','Anqing','Astrakhan','Auckland','Baghdad','Baku','Bamako',
#              'Beijing','Beira','Belgaum','Belgrade','BeloHorizonte','Berlin','Budapest','BuenosAires','Busan','Cabimas',
#               'Cairo','Caracas','CebuCity','Changzhi','Changzhou','Cheonan','Chicago','Cirebon','Cleveland','Cochabamba',
#               'Coimbatore','Cordoba','Culiacan','Curitiba','Dhaka','Florianopolis','Fukuoka','Gainesville','Gombe','Gomel',
#               'Gorgan','Guadalajara','Guangzhou','GuatemalaCity','Hangzhou','Hindupur','Holguin','HongKong','Houston',
#               'Hyderabad','Ibadan', 'Ilheus','Istanbul','Jaipur','Jalna','Jequie','Jinan','Jinju','Johannesburg','Kabul',
#               'Kaiping','Kanpur','Karachi','Kaunas','Kayseri','Khartoum','Killeen','Kinshasa','Kolkata','Kozhikode',
#               'KualaLumpur','Kyoto','Lagos','Lahore','LeMans','London','LosAngeles','Luanda','Lubumbashi','Malatya','Malegaon',
#               'Manila','Marrakesh','Medan','MexicoCity','Milan','Minneapolis','Modesto','Montreal','Moscow','Mumbai','Myeik',
#               'Nakuru','Ndola','Nikolaev','Okayama','Oldenburg','Oyo','Palembang','Palermo','Palmas','Parbhani',
#               'Paris','Pematangtiantar','Pingxiang','Pokhara','Portland','Pyongyang','Quito','Raleigh','Reynosa','RibeiraoPreto',
#               'Riyadh','Rovno','Saidpur','SaintPetersburg','SanSalvador','Sana','Santiago','SaoPaulo','Seoul','Sheffield',
#               'Shymkent','Sialkot','Singapore','Sitapur','Suva','Sydney','Taipei','Tangshan','Tashkent','Tebessa','TelAviv',
#               'Tianjin','Tijuana','Tokyo','Toledo','Ulaanbaatar','Victoria','Vienna','Vijayawada','Warsaw','Wuhan','Xingping',
#               'Yamaguchi','Yiyang', 'Zhuji','Zwolle')

##New York needs to be run alone due to size
cityList = c('NewYork')

distList = c(4000,
             6000,
             8000,
             10000,
             12000,
             14000,
             16000,
             18000,
             20000,
             22000,
             24000,
             26000,
             28000,
             30000,
             35000,
             40000,
             45000,
             50000,
             55000,
             60000,
             65000,
             70000,
             75000,
             80000,
             85000,
             90000,
             95000,
             100000,
             110000,
             120000,
             130000,
             140000,
             150000,
             160000,
             170000,
             180000,
             190000,
             200000,
             220000,
             250000,
             270000)
#Looked at which city is our largest (New York), used this to set the largest buffer length in the list

#Green (30m pixels)
for (city in cityList) {
  print(city)
  file = paste("CCA_NDVI_",city,"_WGS84m_20230316.tif",sep="")
  green = raster(file)
  cityCenter = cbd[cbd$City == city, ]
  plot(green, col = c("white","#809C8D"))
  plot(cityCenter,col="red",pch = 3,add = T)
  
  #Determine the largest buffer distance based on the size of the image
  rasterWidth = dim(green)
  if (rasterWidth[1] >= rasterWidth[2]){
    maxWidth = rasterWidth[2]
  } else {maxWidth = rasterWidth[1] }
  w = (maxWidth/2)*30
  bmax = buffer(cityCenter,width=w)
  print(w)
  
  #add information on the largest bounding circle to the center shapefile
  cityCenter$radius = w
  
  buffW = buffer(cityCenter,width=w)
  crs(buffW) = crs(green)
  mask_greenW = mask(green,buffW)
  crop_greenW = crop(mask_greenW,buffW)

  boundMetrics = calculate_lsm(crop_greenW,what = c("lsm_c_pland","lsm_c_area_mn","lsm_c_ai"),directions = 8)
  cityCenter$plandG = as.numeric(boundMetrics[6,6])
  cityCenter$areamnG = as.numeric(boundMetrics[4,6])
  cityCenter$aiG = as.numeric(boundMetrics[2,6])
  outerBoundsG = rbind(outerBoundsG,cityCenter@data)
  
  #Add the largest buffer distance to the "dist" list
  dist = c(distList, w)

   #Loop to add buffers to the data frame
   for (x in dist){
     if (x <= w){
       buff = buffer(cityCenter,width=x)
       print(x)
       plot(buff,add = T)
  
       #clip to each possible radius
       crs(buff) = crs(green)
       mask_green = mask(green,buff)
       crop_green = crop(mask_green,buff)
  
       #check landscape
       check_landscape(crop_green, verbose = T)
  
       #calculate metrics:
       #Percentage of Landscape [lsm_c_pland]
       #Area [lsm_c_area_sd,lsm_c_area_mn,lsm_c_area_cv]
       #Aggregation Index [lsm_c_ai]
       metric_green = calculate_lsm(crop_green,what = c("lsm_c_pland","lsm_c_area_sd","lsm_c_area_mn","lsm_c_area_cv","lsm_c_ai"),directions = 8)
  
       #Add to metric DF
       metric_green$radius = x
       metric_green$city = city
       metricGreen_DF = rbind(metricGreen_DF,metric_green)
     }
   }
}

# STEP 3 -----------------------------------------------
#Clean and export results

# ##SCALOGRAM OUTPUT
# #Green 
# #remove the "0" class rows
# metricGreen_DF = metricGreen_DF[!(metricGreen_DF$class==0),]
# 
# #remove unneeded columns
# exportGreen = subset(metricGreen_DF,select = -c(layer,level,class,id))
# exportGreen$LC = "green"
# 
# #Export
# write.csv(exportGreen,"C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace/Data/LandscapeMetrics/LandscapeMetrics_NewYorkGuangzhou_2023.03.21.csv")
write.csv(outerBoundsG,"C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace/Data/LandscapeMetrics/LandscapeMetrics_OuterBounds_NewYork_2023.03.21.csv")

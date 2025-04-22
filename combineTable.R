#AUTHOR: Michelle Stuhlmacher, PhD
#LAST UPDATED: 2023-03-21

#GOAL: Combine table exports with radii, NDVI, and population values

#STEPS:
#1. Import data and libraries, set up DF
#2. Loop through files and append to DF
#4. Export DF

# STEP 1 -----------------------------------------------
#Import data and libraries

#Install libraries
library(dplyr)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace/Data/CityStats/CCATableExport_20230316") 

#Create a list of files
tableList = list.files()

# STEP 2 -----------------------------------------------
#Loop through files and append to DF
for (file in tableList){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

#Remove system.index and .geo column
DF = subset(dataset,select = -c(system.index,.geo))

#Remove duplicates
DF = unique(DF)

# STEP 3 -----------------------------------------------
#Export
write.csv(DF,file = 'C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace/Data/CityStats/BoundingCircle_allCities_2023.03.21.csv')
#AUTHOR: Michelle Stuhlmacher, PhD
#LAST UPDATED: 2022-08-29

#GOAL: Analysis and figures for results presented in SI
#(see analysis.R for results presented in text)

#STEPS:
#1. Load in data and packages
#2. Global green space scaling with area of bounding circle
#3. City level scaling by climate

## STEP 1 ---------------------------------------------------------------
# Load in data and packages
library(ggplot2)
library(SciViews)
library(moments)
library(FSA)
library(smatr)
library(RColorBrewer)
library(gridExtra)
library(matlab)
library(dplyr)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace") #DePaul laptop

##load in metric data
#all concentric circles:
metricDF = read.csv('./Data/LandscapeMetrics/LandscapeMetrics_allCities_2023.03.21.csv')
metricDF$circleAreaKm2 = (pi*(metricDF$radius)^2)/1000000

#outer bounding circle (GEE export)
DF = read.csv('./Data/CityStats/BoundingCircle_allCities_2023.03.21.csv')

#outer bounding circle (metrics)
bcDF = read.csv('./Data/LandscapeMetrics/LandscapeMetrics_OuterBounds_allCities_2023.03.21.csv')

DF = merge(DF,bcDF,by = 'City')

#Calculate the area of the bounding circle from the radius
DF$circleAreaKm2 = (pi*(DF$radius.x)^2)/1000000
DF$IncGroup = factor(DF$IncomeClas, levels = c("H", "UM", "LM", "L"))

#Add biome names
DF$MOIST_NAME = ifelse(DF$MOIST_NUM == 1, "Desert",
                       ifelse(DF$MOIST_NUM == 2, "Dry",
                              ifelse(DF$MOIST_NUM == 3, "Moist",
                                     "ERROR")))
DF$MoistGroup = factor(DF$MOIST_NAME)

DF$TEMP_NAME = ifelse(DF$TEMP_NUM == 1, "Boreal",
                      ifelse(DF$TEMP_NUM == 2, "Cool Temperate",
                             ifelse(DF$TEMP_NUM == 3, "Polar",
                                    ifelse(DF$TEMP_NUM == 4, "Sub Tropical",
                                           ifelse(DF$TEMP_NUM == 5, "Tropical",
                                                  ifelse(DF$TEMP_NUM == 6, "Warm Temperate",
                                                         "ERROR"))))))
DF$TempGroup = factor(DF$TEMP_NAME, levels = c("Cool Temperate","Warm Temperate","Sub Tropical","Tropical","Boreal"))

#for local scaling (step 4) drop any cities with fewer than 3 radii (reduces the number of cities from 150 to 124). NEED TO NOTE THIS CHANGE
DF_3 = subset(DF,radius.x >= 8000)

## STEP 2 ---------------------------------------------------------------
# Global green space scaling with area of bounding circle (SI Figure 2)

##Bounding circle area and PLAND green
pc1 = sma(ln(DF$plandG)~ln(DF$circleAreaKm2))
summary(pc1)
#plot(pc1)
#plot(pc1, which = "residual")
#plot(pc1, which = "qq") 

##----By income group----
pc1 = sma(ln(DF$plandG)~ln(DF$circleAreaKm2))
summary(pc1)

pc2 = sma(ln(DF$plandG)~ln(DF$circleAreaKm2)*DF$IncGroup)
summary(pc2)
plot(pc2)
# plot(pc2, which = "residual")
# plot(pc2, which = "qq") 

#elevation = pc2$coef$H[1,1]
#slope = pc2$coef$H[2,1]

#https://stackoverflow.com/questions/32226513/limiting-the-x-axis-range-of-geom-line-defined-by-slope-and-intercept
#If I want to limit the x and y value range, try out geom_segment:
#geom_segment(data = df3, aes(x = 1, xend = 9, y = a + b, yend = a + b*9))

BC_pland = ggplot(DF, aes(x = ln(circleAreaKm2),y = ln(plandG), color = IncGroup)) +
  #scale_y_continuous(limits = c(min(ln(DF$circleAreaKm2),max(ln(DF$circleAreaKm2))))) +
  geom_point() +
  scale_color_brewer(palette = "PuOr") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2}, ")"))) +
  ylab(expression(paste("ln(percent area covered by green space)"))) +
  theme_minimal() +
  #scale_y_continuous(limits=c(ln(min(DF$plandG)),max(ln(DF$plandG))))+
  ggtitle("A. Area")+
  #H
  geom_abline(slope=as.numeric(pc2$coef$H[2,1]),intercept=as.numeric(pc2$coef$H[1,1]), size = 1, linetype=2, color = "#e66101") +
  #UM
  geom_abline(slope=as.numeric(pc2$coef$UM[2,1]),intercept=as.numeric(pc2$coef$UM[1,1]), size = 1, color = "#fdb863") +
  #LM
  geom_abline(slope=as.numeric(pc2$coef$LM[2,1]),intercept=as.numeric(pc2$coef$LM[1,1]), size = 1,linetype=2, color = "#b2abd2") +
  #L
  geom_abline(slope=as.numeric(pc2$coef$L[2,1]),intercept=as.numeric(pc2$coef$L[1,1]), size = 1, linetype=2,color = "#5e3c99") +
  theme(legend.position = "none")

##Bounding circle area and mean patch area (natural log)
gb1 = sma(ln(DF$areamnG)~ln(DF$circleAreaKm2))
summary(gb1)

gb2 = sma(ln(DF$areamnG)~ln(DF$circleAreaKm2)*DF$IncGroup)
summary(gb2)
plot(gb2)
# plot(gb2, which = "residual")
# plot(gb2, which = "qq")

BC_area = ggplot(DF, aes(x = ln(circleAreaKm2),y = ln(areamnG), color = IncGroup)) +
  geom_point() +
  scale_color_brewer(palette = "PuOr") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2}, ")"))) +
  ylab(expression("ln(average area of green space patches: ha)")) +
  ggtitle("B. Average Patch Area")+
  theme_minimal() +
  #H
  geom_abline(slope=as.numeric(gb2$coef$H[2,1]),intercept=as.numeric(gb2$coef$H[1,1]), size = 1, color = "#e66101") +
  #UM
  geom_abline(slope=as.numeric(gb2$coef$UM[2,1]),intercept=as.numeric(gb2$coef$UM[1,1]), size = 1, color = "#fdb863") +
  #LM
  geom_abline(slope=as.numeric(gb2$coef$LM[2,1]),intercept=as.numeric(gb2$coef$LM[1,1]), size = 1, color = "#b2abd2") +
  #L
  geom_abline(slope=as.numeric(gb2$coef$L[2,1]),intercept=as.numeric(gb2$coef$L[1,1]), size = 1, color = "#5e3c99")+
  theme(legend.position = "none")

##Bounding circle area and AI
gf1 = sma(ln(DF$aiG)~ln(DF$circleAreaKm2))
summary(gf1)
#plot(gf1)
#plot(gf1, which = "residual")
#plot(gf1, which = "qq")

gf2 = sma(ln(DF$aiG)~ln(DF$circleAreaKm2)*DF$IncGroup)
summary(gf2)
plot(gf2)
# plot(gf2, which = "residual")
# plot(gf2, which = "qq")

BC_ai = ggplot(DF, aes(x = ln(circleAreaKm2),y = ln(aiG), color = IncGroup)) +
  geom_point() +
  scale_color_brewer(palette = "PuOr") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2}, ")"))) +
  ylab(expression(paste("ln(green space connectedness)"))) +
  theme_minimal() +
  ggtitle("C. Connectedness") +
  #H
  geom_abline(slope=as.numeric(gf2$coef$H[2,1]),intercept=as.numeric(gf2$coef$H[1,1]), size = 1, color = "#e66101") +
  #UM
  geom_abline(slope=as.numeric(gf2$coef$UM[2,1]),intercept=as.numeric(gf2$coef$UM[1,1]), size = 1, color = "#fdb863") +
  #LM
  geom_abline(slope=as.numeric(gf2$coef$LM[2,1]),intercept=as.numeric(gf2$coef$LM[1,1]), size = 1, color = "#b2abd2") +
  #L
  geom_abline(slope=as.numeric(gf2$coef$L[2,1]),intercept=as.numeric(gf2$coef$L[1,1]), size = 1, color = "#5e3c99") +
  #geom_abline(intercept = as.numeric(gf1$coef[[1]][1,1]), slope = 1, size = 1.25, color = "#333333") +
  theme(legend.position = "none")

grid.arrange(BC_pland,BC_area,BC_ai,ncol = 3)

##----By temperature regime----
#PLAND
pc2_b = sma(ln(DF$plandG)~ln(DF$circleAreaKm2)*DF$TempGroup)
summary(pc2_b)
plot(pc2_b)
plot(pc2_b, which = "residual")
plot(pc2_b, which = "qq")

#Dropped because there are fewer than 3 cities:
#Boreal (and Polar)

#Biomes that are stat sig:
#Tropical 

#Biomes that are not stat sig:
#Cool Temperate 
#Sub Tropical 
#Warm Temperate 

PC_pland_b = ggplot(DF, aes(x = ln(circleAreaKm2),y = ln(plandG), color = TempGroup)) +
  geom_point() +
  scale_color_brewer(name = "Temperature Regimes",palette = "Paired") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2}, ")"))) +
  ylab(expression(paste("ln(percent area covered by green space)"))) +
  ggtitle('D. Area')+
  theme_minimal() +
  scale_y_continuous(limits=c(min(ln(DF$plandG)),max(ln(DF$plandG))))+
  #Cool Temperate (not sig)
  geom_abline(slope=as.numeric(pc2_b$coef$`Cool Temperate`[2,1]),intercept=as.numeric(pc2_b$coef$`Cool Temperate`[1,1]),size = 1,linetype = 2,color = "#a6cee3") +
  #Warm Temperate (not sig)
  geom_abline(slope=as.numeric(pc2_b$coef$`Warm Temperate`[2,1]),intercept=as.numeric(pc2_b$coef$`Warm Temperate`[1,1]),size = 1,linetype = 2,color = "#1f78b4") +
  #Sub Tropical (not sig)
  geom_abline(slope=as.numeric(pc2_b$coef$`Sub Tropical`[2,1]),intercept=as.numeric(pc2_b$coef$`Sub Tropical`[1,1]), size = 1,linetype=2, color = "#b2df8a") +
  #Tropical (sig)
  geom_abline(slope=as.numeric(pc2_b$coef$Tropical[2,1]),intercept=as.numeric(pc2_b$coef$Tropical[1,1]),size = 1,color = "#33a02c") +
  theme(legend.position = "none")

#AREA_MN
area8_b = sma(ln(DF$areamnG)~ln(DF$circleAreaKm2)*DF$TempGroup)
summary(area8_b)
plot(area8_b)

#Dropped because there are fewer than 3 cities:
#Boreal (and Polar)

#Biomes that are stat sig:
#Sub Tropical
#Tropical
#Warm Temperate

#Biomes that are not stat sig:
#Cool Temperate

PC_area_b = ggplot(DF, aes(x = ln(circleAreaKm2),y = ln(areamnG), color = TempGroup)) +
  geom_point() +
  scale_color_brewer(name = "Temperature Regimes",palette = "Paired") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2}, ")"))) +
  ylab(expression("ln(average area of green space patches: ha)")) +
  theme_minimal() +
  ggtitle("E. Average Patch Area") +
  scale_y_continuous(limits=c(min(ln(DF$areamnG)),max(ln(DF$areamnG))))+
  #Cool Temperate (not sig)
  geom_abline(slope=as.numeric(area8_b$coef$`Cool Temperate`[2,1]),intercept=as.numeric(area8_b$coef$`Cool Temperate`[1,1]),size = 1,linetype = 2,color = "#a6cee3") +
  #Warm Temperate (sig)
  geom_abline(slope=as.numeric(area8_b$coef$`Warm Temperate`[2,1]),intercept=as.numeric(area8_b$coef$`Warm Temperate`[1,1]),size = 1,color = "#1f78b4") +
  #Sub Tropical (sig)
  geom_abline(slope=as.numeric(area8_b$coef$`Sub Tropical`[2,1]),intercept=as.numeric(area8_b$coef$`Sub Tropical`[1,1]), size = 1,color = "#b2df8a") +
  #Tropical (sig)
  geom_abline(slope=as.numeric(area8_b$coef$Tropical[2,1]),intercept=as.numeric(area8_b$coef$Tropical[1,1]),size = 1,color = "#33a02c") +
  theme(legend.position = "none")

#AI
ai2_b = sma(ln(DF$aiG)~ln(DF$circleAreaKm2)*DF$TempGroup)
summary(ai2_b)
plot(ai2_b)
#plot(ai2_b, which = "residual")
#plot(ai2_b, which = "qq")

#Dropped because there are fewer than 3 cities:
#Boreal (and Polar)

#Biomes that are stat sig:
#Sub Tropical
#Tropical
#Warm Temperate

#Biomes that are not stat sig:
#Cool Temperate 

PC_ai_b = ggplot(DF, aes(x = ln(circleAreaKm2),y = ln(aiG), color = TempGroup)) +
  geom_point() +
  scale_color_brewer(name = "Temperature Regimes",palette = "Paired") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2}, ")"))) +
  ylab(expression(paste("ln(green space connectedness)"))) +
  theme_minimal() +
  ggtitle("F. Connectedness")+
  scale_y_continuous(limits=c(min(ln(DF$aiG)),max(ln(DF$aiG))))+
  #Cool Temperate (not sig)
  geom_abline(slope=as.numeric(ai2_b$coef$`Cool Temperate`[2,1]),intercept=as.numeric(ai2_b$coef$`Cool Temperate`[1,1]),size = 1,linetype = 2,color = "#a6cee3") +
  #Warm Temperate (sig)
  geom_abline(slope=as.numeric(ai2_b$coef$`Warm Temperate`[2,1]),intercept=as.numeric(ai2_b$coef$`Warm Temperate`[1,1]),size = 1,color = "#1f78b4") +
  #Sub Tropical (sig)
  geom_abline(slope=as.numeric(ai2_b$coef$`Sub Tropical`[2,1]),intercept=as.numeric(ai2_b$coef$`Sub Tropical`[1,1]), size = 1,color = "#b2df8a") +
  #Tropical (sig)
  geom_abline(slope=as.numeric(ai2_b$coef$Tropical[2,1]),intercept=as.numeric(ai2_b$coef$Tropical[1,1]),size = 1,color = "#33a02c") +
  theme(legend.position = "none")

grid.arrange(PC_pland_b,PC_area_b,PC_ai_b,ncol=3)

grid.arrange(BC_pland,BC_area,BC_ai,PC_pland_b,PC_area_b,PC_ai_b,ncol = 3)

## STEP 3 ---------------------------------------------------------------
# City-level scaling by climate (SI Figure 3)

#----1. Fit curves for each city and metric----
#Set up city list
cityList = DF_3$City
metricList = c('pland','ai','area_mn')

#create empty DF
curveDF = data.frame(matrix(ncol = 6, nrow = 0))
colnames(curveDF) = c("City","Metric","BoundCir_R2","BoundCir_pVal","BoundCir_exp","BoundCir_int")

for (city in cityList) {
  print(city)
  for (metric in metricList) {
    #set up the data frame where the values will be stored in the loop
    DF_loop = data.frame(matrix(ncol = 6, nrow = 1))
    colnames(DF_loop) = c("City","Metric","BoundCir_R2","BoundCir_pVal","BoundCir_exp","BoundCir_int")
    DF_loop$City = city
    DF_loop$Metric = metric
    
    #subset the metricDF to the city and metric
    curveDF_loop = metricDF[(metricDF$city == city) & (metricDF$metric == metric) & metricDF$LC == "green", ]
    
    #fit the curve
    model = lm(log(curveDF_loop$value) ~ log(curveDF_loop$circleAreaKm2)) #Bounding Circle Area
    
    #put them to the data frame
    DF_loop$BoundCir_R2 = summary(model)$r.squared
    DF_loop$BoundCir_pVal = as.numeric(summary(model)$coefficients[2,4])
    DF_loop$BoundCir_exp = as.numeric(model$coefficients[2])
    DF_loop$BoundCir_int = as.numeric(model$coefficients[1])
    
    #append the loop data frame to the data frame outside the loop
    curveDF = rbind(curveDF,DF_loop)
  }
}

#Add in the income class for export:
exportTab = merge(curveDF,DF %>% select("City","IncGroup","TempGroup","MoistGroup","Region","pop2015","circleAreaKm2"),by = "City",all.x = TRUE)
#exportTab$LC = "Green"

#----2. Isolate the cities/metrics with stat sig p-values and high R2----
exportTab_sig90 =  exportTab[(exportTab$BoundCir_pVal <= 0.05) & (exportTab$BoundCir_R2 >= 0.90),]

exportTab_sig90PLAND = exportTab_sig90[(exportTab_sig90$Metric == "pland"),]
exportTab_sig90AREA = exportTab_sig90[(exportTab_sig90$Metric == "area_mn"),]
exportTab_sig90AI = exportTab_sig90[(exportTab_sig90$Metric == "ai"),]

#----3. Scatterplots with exponents on the y-axis and bounding circle area on the x-axis----

##PLAND
plandE = ggplot(exportTab_sig90PLAND, aes(x=ln(circleAreaKm2),y=BoundCir_exp, color = TempGroup))+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(name = "Climate",palette = "Paired") +
  ggtitle("A. Area") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2},")"))) +
  geom_hline(yintercept = 1,linetype = 2, color = "#333333") +
  scale_y_continuous(limits = c(0, (max(exportTab_sig90PLAND$BoundCir_exp))))+
  ylab("b")+
  theme(legend.position = "none")

##AREA_MN
areamnE = ggplot(exportTab_sig90AREA, aes(x=ln(circleAreaKm2),y=BoundCir_exp, color = TempGroup))+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(name = "Climate",palette = "Paired") +
  ggtitle("B. Average Patch Area") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2},")"))) +
  ylab("b")+
  geom_hline(yintercept = 1,linetype = 2, color = "#333333") +
  scale_y_continuous(limits = c(0, (max(exportTab_sig90AREA$BoundCir_exp))))+
  theme(legend.position = "none")

##AI
aiE = ggplot(exportTab_sig90AI, aes(x=ln(circleAreaKm2),y=BoundCir_exp, color = TempGroup))+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(name = "Climate",palette = "Paired") +
  ggtitle("C. Connectedness") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2},")"))) +
  ylab("b")+
  theme(legend.position = "none")

grid.arrange(plandE,areamnE,aiE,nrow=1)